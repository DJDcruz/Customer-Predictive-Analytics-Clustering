install.packages("pacman")
  
  pacman::p_load(
    readxl,
    dplyr,
    tidyr,
    ggplot2,
    lubridate,
    cluster,
    future,
    caret,
    rpart,
    rpart.plot,
    zoo,
    pROC
  )

  suppressPackageStartupMessages({
    library(readxl)
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    library(lubridate)
    library(cluster)
    library(caret)
    library(rpart)
    library(rpart.plot)
    library(pROC)
    library(zoo)
  })
  
  set.seed(42)
  
  theme_set(theme_minimal(base_size = 12))
  
  
  # Data Loading & Preprocessing 
  raw_df <- read_excel("online+retail/Online Retail.xlsx")
  cat(sprintf("%d rows, %d columns\n", nrow(raw_df), ncol(raw_df)))
  
  print(str(raw_df))
  print(summary(raw_df))
  
  raw_df <- raw_df %>%
    mutate(InvoiceDate = as.POSIXct(InvoiceDate, tz = "Asia/Dubai"))
  
  cat("Missing values per column:\n")
  print(colSums(is.na(raw_df)))
  cat("\n")
  
  # Drop rows where CustomerID is missing
  df <- raw_df %>%
    filter(!is.na(CustomerID))
  cat(sprintf("After dropping missing CustomerID: %d rows\n", nrow(df)))
  
  # Remove cancelled orders
  df <- df %>%
    filter(!startsWith(as.character(InvoiceNo), "C"))
  cat(sprintf("After removing cancelled orders: %d rows\n", nrow(df)))
  
  # Remove non-positive quantity or price
  df <- df %>%
    filter(Quantity > 0, UnitPrice > 0)
  cat(sprintf("After removing non-positive Quantity & Price: %d rows\n", nrow(df)))
  
  # Treat UnitPrice outliers using IQR capping (Winsorization)
  Q1 <- quantile(df$UnitPrice, 0.25)
  Q3 <- quantile(df$UnitPrice, 0.75)
  IQR_val <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR_val
  upper_bound <- Q3 + 1.5 * IQR_val
  
  cat(sprintf("Rows with UnitPrice outliers: %d\n", sum(df$UnitPrice < lower_bound | df$UnitPrice > upper_bound)))

  
  df <- df %>%
    mutate(UnitPrice = pmin(pmax(UnitPrice, lower_bound), upper_bound))
  
  # Create TotalPrice and change CustomerID to int
  df <- df %>%
    mutate(
      TotalPrice = Quantity * UnitPrice,
      CustomerID = as.integer(CustomerID)
    )
  
  cat(sprintf("\nCleaned dataset shape: %d rows, %d columns\n", nrow(df), ncol(df)))
  
  # EDA & Visualization
  
  total_revenue <- sum(df$TotalPrice, na.rm = TRUE)
  
  cat(sprintf("Total Revenue: £%.2f\n", total_revenue))
  
  df %>%
    mutate(DayOfWeek = wday(InvoiceDate, label = TRUE)) %>%
    group_by(DayOfWeek) %>%
    summarise(Revenue = sum(TotalPrice), .groups = "drop") %>%
    ggplot(aes(x = DayOfWeek, y = Revenue)) +
    geom_col(fill = "steelblue") +
    labs(title = "Revenue by Day of Week", x = NULL, y = "Revenue (GBP)")
  
  df %>%
    mutate(YearMonth = floor_date(InvoiceDate, "month")) %>%
    group_by(YearMonth) %>%
    summarise(Revenue = sum(TotalPrice), .groups = "drop") %>%
    ggplot(aes(x = YearMonth, y = Revenue)) +
    geom_line(color = "steelblue") +
    geom_point() +
    labs(title = "Monthly Revenue Trend", x = NULL, y = "Revenue (GBP)")
  
  
  daily_sales <- df %>%
    mutate(InvoiceDay = as.Date(InvoiceDate)) %>%
    group_by(InvoiceDay) %>%
    summarise(TotalSales = sum(TotalPrice), .groups = "drop") %>%
    arrange(InvoiceDay)
  
  # STL decomposition with weekly seasonality
  ts_sales <- ts(daily_sales$TotalSales, frequency = 7)
  decomp <- stl(ts_sales, s.window = "periodic")
  
  plot(decomp, main = "Time Series Decomposition - Daily Revenue (Weekly Seasonality)")
  
  #
  daily_sales <- daily_sales %>%
    mutate(
      MA7  = zoo::rollmean(TotalSales, k = 7,  fill = NA, align = "right"),
      MA30 = zoo::rollmean(TotalSales, k = 30, fill = NA, align = "right")
    )
  
  ggplot(daily_sales, aes(x = InvoiceDay)) +
    geom_line(aes(y = TotalSales), color = "grey", alpha = 0.7, linewidth = 0.4) +
    geom_line(aes(y = MA7),  color = "steelblue", linewidth = 0.8) +
    geom_line(aes(y = MA30), color = "red",     linewidth = 0.8) +
    labs(
      title    = "Daily Revenue with 7-day and 30-day Moving Averages",
      subtitle = "Grey = No smoothing | Blue = MA7 | Red = MA30",
      x = NULL, y = "Revenue (GBP)"
    )
  
  
  country_revenue <- df %>%
    group_by(Country) %>%
    summarise(Revenue = sum(TotalPrice), .groups = "drop") %>%
    arrange(desc(Revenue))
  
  p1 <- country_revenue %>%
    slice_head(n = 10) %>%
    mutate(Country = reorder(Country, Revenue)) %>%
    ggplot(aes(x = Country, y = Revenue)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(title = "Top 10 Countries by Revenue (incl. UK)", x = NULL, y = "Revenue (GBP)")
  
  non_uk <- country_revenue %>% filter(Country != "United Kingdom")
  
  p2 <- non_uk %>%
    slice_head(n = 10) %>%
    mutate(Country = reorder(Country, Revenue)) %>%
    ggplot(aes(x = Country, y = Revenue)) +
    geom_col(fill = "red") +
    coord_flip() +
    labs(title = "Top 10 Countries by Revenue (excl. UK)", x = NULL, y = "Revenue (GBP)")
  
  print(p1)
  print(p2)
  
  uk_revenue <- country_revenue %>% filter(Country == "United Kingdom") %>% pull(Revenue)
  if (length(uk_revenue) == 0) {
    uk_pct <- 0
  } else {
    uk_pct <- (uk_revenue / sum(country_revenue$Revenue)) * 100
  }
  cat(sprintf("UK revenue share: %.2f%%\n", uk_pct))
  
  # Build RFM table
  reference_date <- max(df$InvoiceDate, na.rm = TRUE) + days(1)
  
  rfm <- df %>%
    group_by(CustomerID) %>%
    summarise(
      Recency = as.integer(difftime(reference_date, max(InvoiceDate), units = "days")),
      Frequency = n_distinct(InvoiceNo),
      Monetary = sum(TotalPrice),
      .groups = "drop"
    )
  
  cat(sprintf("RFM table shape: %d rows, %d columns\n", nrow(rfm), ncol(rfm)))
  print(summary(rfm))
  
  # Clustering Analysis
  
  rfm_log <- rfm %>%
    select(Recency, Frequency, Monetary) %>%
    mutate(across(everything(), log1p))
  
  rfm_scaled <- scale(rfm_log)
  
  pca <- prcomp(rfm_scaled, center = TRUE, scale. = FALSE)
  explained <- (pca$sdev^2) / sum(pca$sdev^2)
  
  barplot(
    explained[1:2],
    names.arg = c("PC1", "PC2"),
    col = "steelblue",
    main = "PCA Ratio Analysis",
    ylab = "Proportion of Variance"
  )
  
  k_range <- 2:10
  inertias <- numeric(length(k_range))
  silhouettes <- numeric(length(k_range))
  dist_mat <- dist(rfm_scaled)
  
  for (i in seq_along(k_range)) {
    k <- k_range[i]
    km <- kmeans(rfm_scaled, centers = k, nstart = 10, iter.max = 300)
    inertias[i] <- km$tot.withinss
    silhouettes[i] <- mean(cluster::silhouette(km$cluster, dist_mat)[, 3])
  }
  
  plot(k_range, inertias, type = "b", pch = 19, col = "blue",
       main = "Elbow Method - Inertia", xlab = "Number of Clusters (k)", ylab = "Inertia")
  
  plot(k_range, silhouettes, type = "b", pch = 19, col = "red",
       main = "Silhouette Score", xlab = "Number of Clusters", ylab = "Silhouette Score")
  
  optimal_k <- 3
  km_final <- kmeans(rfm_scaled, centers = optimal_k, nstart = 10, iter.max = 300)
  rfm$Cluster <- factor(km_final$cluster) 
  
  pca_df <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2], Cluster = rfm$Cluster)
  
  ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) +
    geom_point(alpha = 0.6, size = 1.5) +
    labs(title = paste0("Customer Segments (K-Means, k=", optimal_k, ") in PCA Space"))
  
  cluster_profile <- rfm %>%
    group_by(Cluster) %>%
    summarise(
      Recency_mean = mean(Recency),
      Recency_median = median(Recency),
      Frequency_mean = mean(Frequency),
      Frequency_median = median(Frequency),
      Monetary_mean = mean(Monetary),
      Monetary_median = median(Monetary),
      .groups = "drop"
    )
  
  cat("Cluster Profiles (mean / median):\n")
  print(cluster_profile)
  
  cluster_means <- rfm %>%
    group_by(Cluster) %>%
    summarise(
      Recency = mean(Recency),
      Frequency = mean(Frequency),
      Monetary = mean(Monetary),
      .groups = "drop"
    )
  
  cluster_norm <- cluster_means %>%
    select(-Cluster) %>%
    scale() %>%
    as.data.frame()
  cluster_norm$Cluster <- cluster_means$Cluster
  
  snake_df <- cluster_norm %>%
    pivot_longer(cols = c("Recency", "Frequency", "Monetary"),
                 names_to = "Metric", values_to = "Value")
  
  ggplot(snake_df, aes(x = Metric, y = Value, group = Cluster, color = Cluster)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    labs(title = "Cluster Profile - Normalised RFM Snake Plot", y = "Normalised Value")
  
  
  # Customer Churn Prediction
  
  RECENCY_THRESHOLD <- 60
  FREQUENCY_THRESHOLD <- 3
  MONETARY_THRESHOLD <- 100
  
  rfm <- rfm %>%
    mutate(
      IsChurned = as.integer(
        (Recency > RECENCY_THRESHOLD) |
          ((Frequency <= FREQUENCY_THRESHOLD) & (Monetary <= MONETARY_THRESHOLD))
      )
    )

  cat("Churn distribution:\n")
  print(table(rfm$IsChurned))
  cat(sprintf("\nChurn rate: %.2f%%\n", mean(rfm$IsChurned) * 100))
  
  churn_plot_df <- as.data.frame(table(rfm$IsChurned))
  colnames(churn_plot_df) <- c("IsChurned", "Count")
  churn_plot_df$Label <- ifelse(churn_plot_df$IsChurned == "0", "Active (0)", "Churned (1)")
  
  ggplot(churn_plot_df, aes(x = Label, y = Count, fill = Label)) +
    geom_col(show.legend = FALSE) +
    scale_fill_manual(values = c("steelblue", "red")) +
    labs(title = "Churn Distribution", x = NULL, y = "Number of Customers")
  
  # Behavioral feature engineering
  order_level <- df %>%
    group_by(CustomerID, InvoiceNo) %>%
    summarise(
      OrderQuantity = sum(Quantity),
      OrderDate = min(InvoiceDate),
      .groups = "drop"
    )
  
  basket_stats <- order_level %>%
    group_by(CustomerID) %>%
    summarise(
      AvgBasketSize = mean(OrderQuantity)
    )
  
  cust_core <- df %>%
    group_by(CustomerID) %>%
    summarise(
      UniqueProducts = n_distinct(StockCode),
      AvgUnitPrice = mean(UnitPrice),
      TotalQuantity = sum(Quantity),
      NumActiveMonths = n_distinct(floor_date(InvoiceDate, "month")),
      .groups = "drop"
    )
  
  cust_features <- basket_stats %>% inner_join(cust_core, by = "CustomerID")
  cat(sprintf("%d rows, %d columns\n", nrow(cust_features), ncol(cust_features)))
  
  clf_df <- rfm %>%
    select(CustomerID, IsChurned) %>%
    inner_join(cust_features, by = "CustomerID")
  
  feature_cols <- c(
    "AvgBasketSize", "UniqueProducts", "AvgUnitPrice",
    "TotalQuantity", "NumActiveMonths"
  )
  
  X <- clf_df %>% select(all_of(feature_cols))
  y <- factor(clf_df$IsChurned, levels = c(0, 1))
  
  # Log-transform features
  for (col in c("AvgBasketSize", "UniqueProducts", "TotalQuantity")) {
    X[[col]] <- log1p(X[[col]])
  }
  
  X_scaled <- as.data.frame(scale(X))
  
  # Stratified split 80/20
  idx <- createDataPartition(y, p = 0.8, list = FALSE)
  X_train <- X_scaled[idx, , drop = FALSE]
  X_test <- X_scaled[-idx, , drop = FALSE]
  y_train <- y[idx]
  y_test <- y[-idx]
  
  cat(sprintf("Training set : %d rows x %d cols\n", nrow(X_train), ncol(X_train)))
  cat(sprintf("Test set     : %d rows x %d cols\n", nrow(X_test), ncol(X_test)))
  cat(sprintf("Train churn rate: %.2f%% | Test churn rate: %.2f%%\n", mean(as.numeric(as.character(y_train))) * 100, mean(as.numeric(as.character(y_test))) * 100))
  
  
  # Decision Tree Classifier
  train_df <- X_train
  train_df$IsChurned <- y_train
  
  dt_model <- rpart(
    IsChurned ~ .,
    data = train_df,
    method = "class",
    parms = list(prior = c(0.5, 0.5)),
    control = rpart.control(maxdepth = 6, minbucket = 10, cp = 0.001)
  )
  
  y_pred_dt <- predict(dt_model, newdata = X_test, type = "class")
  y_pred_dt_prob <- predict(dt_model, newdata = X_test, type = "prob")[, "1"]
  
  cat("DECISION TREE - Classification Report\n")

  cm <- confusionMatrix(y_pred_dt, y_test, positive = "1")
  print(cm)
  
  dt_metrics <- c(
    Accuracy = unname(cm$overall["Accuracy"]),
    Precision = unname(cm$byClass["Precision"]),
    Recall = unname(cm$byClass["Recall"]),
    `F1-Score` = unname(cm$byClass["F1"]),
    `ROC-AUC` = as.numeric(pROC::auc(y_test, y_pred_dt_prob))
  )
  
  cat("\nSummary metrics:\n")
  for (n in names(dt_metrics)) {
    cat(sprintf("  %-12s: %.4f\n", n, dt_metrics[[n]]))
  }
  
  rpart.plot(dt_model, type = 2, extra = 104, fallen.leaves = TRUE, 
             main = "Decision Tree")
  
  fi <- dt_model$variable.importance
  if (!is.null(fi)) {
    fi_df <- data.frame(
      Feature = names(fi),
      Importance = as.numeric(fi)
    ) %>% arrange(Importance)
  
    ggplot(fi_df, aes(x = reorder(Feature, Importance), y = Importance)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Decision Tree - Feature Importance", x = NULL, y = "Importance")
  }
  
  comparison_df <- data.frame(
    Model = "Decision Tree",
    Accuracy = dt_metrics[["Accuracy"]],
    Precision = dt_metrics[["Precision"]],
    Recall = dt_metrics[["Recall"]],
    F1_Score = dt_metrics[["F1-Score"]],
    ROC_AUC = dt_metrics[["ROC-AUC"]]
  )
  
  comparison_df[,-1] <- round(comparison_df[,-1], 4)
  print(comparison_df)
  
cm_table <- as.data.frame(table(Actual = y_test, Predicted = y_pred_dt))

cm_table$Actual    <- factor(ifelse(cm_table$Actual    == "1", "Churned", "Active"), levels = c("Churned", "Active"))
cm_table$Predicted <- factor(ifelse(cm_table$Predicted == "1", "Churned", "Active"), levels = c("Churned", "Active"))

ggplot(cm_table, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  scale_fill_gradient(low = "#9ecae1", high = "#08519c") +
  labs(title = "Decision Tree - Confusion Matrix")
  
  roc_obj <- pROC::roc(response = y_test, predictor = y_pred_dt_prob, levels = c("0", "1"))
  plot(roc_obj, col = "red", lwd = 2, main = "ROC Curve - Decision Tree")
  abline(a = 0, b = 1, lty = 2)
  