library(readr)
library(FSelector)
library(corrplot)
library(class)
library(tree)
library(e1071)
library(ggplot2)
library(knitr)
library(plotly)
library(randomForest)
library(reshape2)
library(caret)

tour_package <- read_csv("C:/Users/ecemt/Downloads/tour_package.csv")
summary(tour_package)
# view dataset features
str(tour_package)

targetFeature <- tour_package["ProdTaken"]
str(targetFeature)

# removing unnecessary features
features <- tour_package[, !colnames(tour_package) %in% c("CustomerID")]
str(features)

# separate descriptive features as categorical and numerical 
categoricalColumns <- c('ProdTaken', 'TypeofContact', 'CityTier', 'Occupation', 'Gender', 'ProductPitched', 'MaritalStatus', 'Passport', 'OwnCar', 'Designation')
features[categoricalColumns] <- lapply(features[categoricalColumns], as.factor)
str(features)

categoricalColumns <- features[sapply(features, is.factor)]
str(categoricalColumns)

numericalColumns  <- features[sapply(features, is.numeric)]
str(numericalColumns)

# mean, mode, median calculation for numerical features. na.rm parameter ignores NA values
summaryStats <- sapply(numericalColumns, function(x) c(mean = mean(x, na.rm = TRUE), mode = as.numeric(names(sort(table(x), decreasing = TRUE)[1])), median = median(x, na.rm = TRUE)))
summaryTable <- as.data.frame(t(summaryStats))
summaryTable <- format(summaryTable, digits = 2, nsmall = 2)
print(summaryTable)

# Veri setindeki her bir sütundaki null değer sayısını ve sütunları bul
null_counts <- colSums(is.na(features))

# Null değer içeren sütunları ve sayılarını yazdır
for (col_name in names(null_counts)) {
  if (null_counts[col_name]  > 0) {
    cat(sprintf("Column: %s | Column Type: %s | Null Count: %d\n", col_name, typeof(features[[col_name]]), null_counts[col_name]))
  }
}

for (col_name in names(features)) {
  if(is.numeric(features[[col_name]])){
    col_mean <- mean(features[[col_name]], na.rm = TRUE)  # Sütunun ortalaması
    features[[col_name]][is.na(features[[col_name]])] <- col_mean
  }else{
    mode_val <- as.factor(names(sort(table(features$TypeofContact, useNA = "always"), decreasing = TRUE)[1]))
    features[[col_name]][is.na(features[[col_name]])] <- mode_val
  }
}

  # Fixing Fe Male Data
  features$Gender <- gsub(' ', '', features$Gender)  # Boşlukları kaldır
  features$Gender <- tolower(features$Gender)  # Tüm harfleri küçük harfe çevir
  features$Gender <- paste0(toupper(substr(features$Gender, 1, 1)), substr(features$Gender, 2, nchar(features$Gender)))  # Her kelimenin ilk harfini büyük harfe çevir
  
  for (col_name in names(features)) {
    num_unique_values <- length(unique(features[[col_name]]))
    cat("Column Name and Total Unique Value:", col_name, num_unique_values, "\n")
  }
  
  # Histogramları çizme
  par(mfrow = c(2, 2))  # 3x3 bir düzen içinde grafiği yerleştir
  
  hist(numericalColumns$Age, main = "Age", xlab = "Age", col = "skyblue", border = "black")
  hist(numericalColumns$MonthlyIncome, main = "MonthlyIncome", xlab = "MonthlyIncome", col = "skyblue", border = "black")
  hist(numericalColumns$DurationOfPitch, main = "DurationOfPitch", xlab = "\nDurationOfPitch", col = "skyblue", border = "black")
  
  par(mfrow = c(2, 5))  # 3x3 bir düzen içinde grafiği yerleştir
  for (col_name in names(numericalColumns)) {
    boxplot(numericalColumns[[col_name]], main = col_name)
  }
  
  remove_outliers <- function(df, coef = 1.5) {
    cleaned_df <- df
    
    for (col_name in names(cleaned_df)) {
      if (is.numeric(cleaned_df[[col_name]])) {
        q1 <- quantile(cleaned_df[[col_name]], 0.25)
        q3 <- quantile(cleaned_df[[col_name]], 0.75)
        iqr <- q3 - q1
        
        lower_bound <- q1 - coef * iqr
        upper_bound <- q3 + coef * iqr
        
        cleaned_df <- cleaned_df[!(cleaned_df[[col_name]] < lower_bound | cleaned_df[[col_name]] > upper_bound), ]
      }
    }
    return(cleaned_df)
  }
  
  # Outlier'ları temizle
  features <- remove_outliers(features)
  str(features$ProdTaken)  
  str(features$Age)

  
  par(mfrow = c(1, 3)) 
  k_age <- 10
  result <- cut(features$Age, breaks = k_age, labels = FALSE, include.lowest = TRUE)
  discretized_age <- cut(features$Age, breaks = k_age, labels = FALSE, include.lowest = TRUE)
  bins_age <- seq(min(features$Age), max(features$Age), length.out = k_age + 1)
  
  # 'Age' sütununa kategori orta noktalarının atanması
  features$Age <- discretized_age
  features$Age <- bins_age[features$Age] + (bins_age[2] - bins_age[1]) / 2
  features$Age <- as.integer(features$Age)
  
  hist(features$Age, main = "Age", xlab = "Age", col = "skyblue", border = "black", breaks = bins_age)
  
  k_MonthlyIncome <- 20
  result <- cut(features$MonthlyIncome, breaks = k_MonthlyIncome, labels = FALSE, include.lowest = TRUE)
  discretized_MonthlyIncome <- cut(features$MonthlyIncome, breaks = k_MonthlyIncome, labels = FALSE, include.lowest = TRUE)
  bins_MonthlyIncome <- seq(min(features$MonthlyIncome), max(features$MonthlyIncome), length.out = k_MonthlyIncome + 1)
  
  # 'MonthlyIncome' sütununa kategori orta noktalarının atanması
  features$MonthlyIncome <- discretized_MonthlyIncome
  features$MonthlyIncome <- bins_MonthlyIncome[features$MonthlyIncome] + (bins_MonthlyIncome[2] - bins_MonthlyIncome[1]) / 2
  features$MonthlyIncome <- as.integer(features$MonthlyIncome)
  
  hist(features$MonthlyIncome, main = "MonthlyIncome", xlab = "MonthlyIncome", col = "skyblue", border = "black", breaks = bins_MonthlyIncome)
  
  k_DurationOfPitch <- 14
  result <- cut(features$DurationOfPitch, breaks = k_DurationOfPitch, labels = FALSE, include.lowest = TRUE)
  discretized_DurationOfPitch <- cut(features$DurationOfPitch, breaks = k_DurationOfPitch, labels = FALSE, include.lowest = TRUE)
  bins_DurationOfPitch <- seq(min(features$DurationOfPitch), max(features$DurationOfPitch), length.out = k_DurationOfPitch + 1)
  
  # 'DurationOfPitch' sütununa kategori orta noktalarının atanması
  features$DurationOfPitch <- discretized_DurationOfPitch
  features$DurationOfPitch <- bins_DurationOfPitch[features$DurationOfPitch] + (bins_DurationOfPitch[2] - bins_DurationOfPitch[1]) / 2
  features$DurationOfPitch <- as.integer(features$DurationOfPitch)
  
  hist(features$DurationOfPitch, main = "DurationOfPitch", xlab = "DurationOfPitch", col = "skyblue", border = "black",  breaks = bins_DurationOfPitch)
  prod_taken_counts <- table(features$Age)
  
  prod_taken_counts <- table(targetFeature)
  prod_taken_percentages <- prop.table(prod_taken_counts)
  
  # Pasta grafiğini çiz
  par(mfrow = c(1, 1))
  pie(prod_taken_percentages, labels = paste(names(prod_taken_percentages), " (", format(prod_taken_percentages, digits = 2, nsmall = 2), ")", sep = ""), 
      main = "ProdTaken Ratio", 
      col = colorRampPalette(c("white", "orange"))(length(prod_taken_percentages)))
  
  par(mfrow = c(3, 3))  # Grafiği 9x2 bir düzen içinde yerleştir
  for (col_name in names(categoricalColumns)) {
    column_counts <- table(features[[col_name]])
    column_percentages <- prop.table(column_counts)
    
    # Beyazdan başlayarak turuncu tonları oluştur
    orange_colors <- colorRampPalette(c("white", "orange"))(length(column_percentages))
    
    # Pasta grafiğini çiz ve renkleri kullan
    pie(column_percentages, 
        labels = paste(names(column_percentages), " (", format(column_percentages, digits = 2, nsmall = 2), ")", sep = ""), 
        col = orange_colors,
        main = paste( col_name, sep = ""))}
  
  
  #Transformation
  features$TypeofContact <- ifelse(features$TypeofContact == 'Self Enquiry', 1,
                                              ifelse(features$TypeofContact == 'Company Invited', 0, -1))
  
  features$Occupation <- ifelse(features$Occupation == 'Salaried', 0,
                                              ifelse(features$Occupation == 'Large Business', 1,
                                                     ifelse(features$Occupation == 'Free Lancer', 2,
                                                            ifelse(features$Occupation == 'Small Business', 3, -1))))
  
  features$Gender <- ifelse(features$Gender == 'Female', 1,
                                              ifelse(features$Gender == 'Male', 0, -1))
  
  features$ProductPitched <- ifelse(features$ProductPitched == 'Basic', 0,
                                           ifelse(features$ProductPitched == 'Deluxe', 1,
                                                  ifelse(features$ProductPitched == 'Super Deluxe', 2,
                                                         ifelse(features$ProductPitched == 'Standard', 3,
                                                                ifelse(features$ProductPitched == 'King', 4, -1)))))
  
  
  features$MaritalStatus <- ifelse(features$MaritalStatus == 'Married', 0,
                                           ifelse(features$MaritalStatus == 'Divorced', 1,
                                                  ifelse(features$MaritalStatus == 'Unmarried', 2,
                                                         ifelse(features$MaritalStatus == 'Single', 3, -1))))

  
  features$Designation <- ifelse(features$Designation == 'Executive', 0,
                                               ifelse(features$Designation == 'Manager', 1,
                                                      ifelse(features$Designation == 'AVP', 2,
                                                             ifelse(features$Designation == 'VP', 3,
                                                                    ifelse(features$Designation == 'Senior Manager', 4, -1)))))
  
  features[] <- lapply(features, as.numeric)
  str(features)
  
  par(mfrow = c(1, 1))
  # Calculate Correlation matrix
  spearman_correlation_matrix <- cor(features, method = "spearman")
  corrplot(spearman_correlation_matrix, method='color', type='upper', 
           addCoef.col = "black", tl.col = "black", 
           diag = FALSE, # Diagonaldeki değerleri gösterme
           order = "hclust", # Matrisi kümeleme
           tl.cex = 0.5, # Etiket boyutu
           tl.srt = 45, # Etiketleri döndürme
           p.mat = NULL, # P değerlerini gösterme
           insig = "blank", # Anlamsız değerleri gösterme
           addCoefAsPercent = TRUE, # Katsayıları yüzde olarak gösterme
           number.cex = 0.6, # Sayıları küçültme
           col = colorRampPalette(c("blue", "white", "red"))(100), # Renk skalası
           addrect = 3 # Renk skalası için dikdörtgen ekleme
  )
  
  features <- features[, !colnames(features) %in% "Designation"]
  
  folds <- createFolds(features$ProdTaken, k = 5)
  
  # -------------------------------------- DECISION TREE -------------------------------------------
  
  
  
  features$ProdTaken <- as.factor(features$ProdTaken)
  table <- tree(ProdTaken ~ ., data=features, split = "gini")
  
  best_conf_matrix <- list()
  dt_acc <- numeric()
  set.seed(77)
  highestAcc <- 0
  highestAccTree <- NULL
  
  for (i in 1:5) {
    train_indices <- unlist(folds[-i])
    test_indices <- folds[[i]]
    
    train_data <- features[train_indices, ]
    test_data <- features[test_indices, ]
    
    training_tree <- tree(ProdTaken ~ ., data = train_data, split ="gini" )
    testing_table <- test_data
    
    target_column <- testing_table[["ProdTaken"]]
    
    confusion_matrix <- table(predict(training_tree, testing_table, type = "class"), target_column)
    accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
   
    dt_acc <- c(dt_acc, accuracy)
    if(accuracy>highestAcc){
      highestAcc <- accuracy
      best_conf_matrix <- confusion_matrix
      highestAccTree <- training_tree
    }
    
  }
  print(dt_acc)
  plot(dt_acc, type="l", ylab="Accuracy Rate", xlab="Iterations", main="Accuracy
Rate")
  cat("Average Accuracy of DT after iteration: ", mean(dt_acc), "\n")
  plot(highestAccTree, type = "uniform")
  text(highestAccTree, cex = 0.6)
  
  precision <- best_conf_matrix[2, 2] / (best_conf_matrix[2, 2] + best_conf_matrix[1, 2])
  
  # Recall
  recall <- best_conf_matrix[2, 2] / (best_conf_matrix[2, 2] + best_conf_matrix[2, 1])
  
  # F1 score
  f1 <- 2 * (precision * recall) / (precision + recall)
  
  # Results
  cat("Precision:", precision, "\n")
  cat("Recall:", recall, "\n")
  cat("F1 Score:", f1, "\n")
  conf_matrix_reshaped <- matrix(c(best_conf_matrix[2, 2], best_conf_matrix[2, 1], best_conf_matrix[1, 2], best_conf_matrix[1, 1]), nrow = 2, byrow = TRUE)
  
  rownames(conf_matrix_reshaped) <- c("Positive", "Negative")
  colnames(conf_matrix_reshaped) <- c("Positive", "Negative")
  
  conf_matrix_data <- melt(conf_matrix_reshaped)
  conf_matrix_data <- data.frame(True = conf_matrix_data$Var1,
                                 Predicted = conf_matrix_data$Var2,
                                 Count = conf_matrix_data$value)
  
  # Plot Confusion Matrix Heatmap
  ggplot(conf_matrix_data, aes(x=Predicted, y=True, fill=Count)) +
    geom_tile() + geom_text(aes(label=Count), vjust=1, size=4) +
    scale_fill_gradient(low="white", high="red") +
    labs(title="DT Confusion Matrix", x="Predicted", y="Actual") +
    theme_minimal()
  
  
  # --------------------------- DT with different split parameter (deviance) -----------------------------
  
  dt_acc <- numeric()
  set.seed(77)
  highestAcc <- 0
  highestAccTree <- NULL
  
  for (i in 1:5) {
    train_indices <- unlist(folds[-i])
    test_indices <- folds[[i]]
    
    train_data <- features[train_indices, ]
    test_data <- features[test_indices, ]
    
    training_tree <- tree(ProdTaken ~ ., data = train_data, split ="deviance" )
    testing_table <- test_data
    
    target_column <- testing_table[["ProdTaken"]]
    confusion_matrix <- table(predict(training_tree, testing_table, type = "class"), target_column)
    accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    
    dt_acc <- c(dt_acc, accuracy)
    if(accuracy>highestAcc){
      highestAcc <- accuracy
      highestAccTree <- training_tree
    }
  }
  print(dt_acc)
  plot(dt_acc, type="l", ylab="Accuracy Rate", xlab="Iterations", main="Accuracy
Rate ")
  cat("Average Accuracy of DT after iteration: ", mean(dt_acc), "\n")
  plot(highestAccTree, type = "uniform")
  text(highestAccTree, cex = 0.6)
  
  # -------------------------------------- KNN -------------------------------------------
  
  best_conf_matrix <- list()
  set.seed(70)
  accuracy_vector <- numeric()
  
  # K values from 1 to 9
  for (k_value in seq(1, 9, by = 2)) {
    fold_accuracies <- c() 
    
    for (i in 1:5) { # n fold validation
      train_indices <- unlist(folds[-i])
      test_indices <- folds[[i]]
      
      train_data <- features[train_indices, ]
      test_data <- features[test_indices, ]
      
      if (!is.factor(train_data$ProdTaken)) {
        train_data$ProdTaken <- as.factor(train_data$ProdTaken)
      }
      
      if (!is.factor(test_data$ProdTaken)) {
        test_data$ProdTaken <- as.factor(test_data$ProdTaken)
      }
      
      knn_model <- knn(train = train_data[, -which(names(train_data) == "ProdTaken")], 
                       test = test_data[, -which(names(test_data) == "ProdTaken")], 
                       cl = train_data$ProdTaken, 
                       k = k_value)
      
      confusion_matrix <- table(knn_model, test_data$ProdTaken)
      print(confusion_matrix)
      
      accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
      
      best_conf_matrix <- confusion_matrix
      fold_accuracies <- c(fold_accuracies, accuracy)
      print(fold_accuracies)
    }
    
    mean_accuracy <- mean(fold_accuracies)
    
    accuracy_vector <- c(accuracy_vector, mean_accuracy)
  }
  
  best_k <- which.max(accuracy_vector)
  cat(sprintf("\nBest K value: %d, Mean Accuracy: %.4f\n", best_k, max(accuracy_vector)))
  precision <- best_conf_matrix[2, 2] / (best_conf_matrix[2, 2] + best_conf_matrix[1, 2])
  
  # Recall
  recall <- best_conf_matrix[2, 2] / (best_conf_matrix[2, 2] + best_conf_matrix[2, 1])
  
  # F1 score
  f1 <- 2 * (precision * recall) / (precision + recall)
  
  # Results
  cat("Precision:", precision, "\n")
  cat("Recall:", recall, "\n")
  cat("F1 Score:", f1, "\n")
  conf_matrix_reshaped <- matrix(c(best_conf_matrix[2, 2], best_conf_matrix[2, 1], best_conf_matrix[1, 2], best_conf_matrix[1, 1]), nrow = 2, byrow = TRUE)
  
  rownames(conf_matrix_reshaped) <- c("Positive", "Negative")
  colnames(conf_matrix_reshaped) <- c("Positive", "Negative")
  
  conf_matrix_data <- melt(conf_matrix_reshaped)
  conf_matrix_data <- data.frame(True = conf_matrix_data$Var1,
                                 Predicted = conf_matrix_data$Var2,
                                 Count = conf_matrix_data$value)
  
  # Plot Confusion Matrix Heatmap
  ggplot(conf_matrix_data, aes(x=Predicted, y=True, fill=Count)) +
    geom_tile() + geom_text(aes(label=Count), vjust=1, size=4) +
    scale_fill_gradient(low="white", high="red") +
    labs(title="KNN Confusion Matrix", x="Predicted", y="Actual") +
    theme_minimal()
  
  # ----------------------------------- Naive Bayes -----------------------------------
  
  set.seed(123)
  best_var_smoothing <- 0
  accuracy_vector <- numeric()
  var_smoothings <- c(1e-9, 1e-8, 1e-7, 1e-6)
  fold_accuracies <- c()  
    
  for (var_smoothing in var_smoothings) {
    for (i in 1:5) {
      
      train_indices <- unlist(folds[-i])
      test_indices <- folds[[i]]
      
      train_data <- features[train_indices, ]
      test_data <- features[test_indices, ]
      
      if (!is.factor(train_data$ProdTaken)) {
        train_data$ProdTaken <- as.factor(train_data$ProdTaken)
      }
      
      if (!is.factor(test_data$ProdTaken)) {
        test_data$ProdTaken <- as.factor(test_data$ProdTaken)
      }
      
      nb_model <- naiveBayes(ProdTaken ~ ., data = train_data, laplace = var_smoothing)
      
      predictions <- predict(nb_model, newdata = test_data)
      
      confusion_matrix <- table(predictions, test_data$ProdTaken)
      
      accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
      best_conf_matrix <- confusion_matrix
      fold_accuracies <- c(fold_accuracies, accuracy)
    }
    
    accuracy_vector <- c(accuracy_vector, mean(fold_accuracies))
    
    if(mean(fold_accuracies)> max(accuracy_vector)){
      best_var_smoothing<- var_smoothing
    }
    cat(sprintf("var_smooth = %e, Mean Accuracy: %.4f\n", var_smoothing, mean(fold_accuracies)))
  }
  
  cat(sprintf("Naive Bayes Mean Accuracy: %.4f\n",  mean(fold_accuracies)))
  precision <- best_conf_matrix[2, 2] / (best_conf_matrix[2, 2] + best_conf_matrix[1, 2])
  
  # Recall
  recall <- best_conf_matrix[2, 2] / (best_conf_matrix[2, 2] + best_conf_matrix[2, 1])
  
  # F1 score
  f1 <- 2 * (precision * recall) / (precision + recall)
  
  # Results
  cat("Precision:", precision, "\n")
  cat("Recall:", recall, "\n")
  cat("F1 Score:", f1, "\n")
  conf_matrix_reshaped <- matrix(c(best_conf_matrix[2, 2], best_conf_matrix[2, 1], best_conf_matrix[1, 2], best_conf_matrix[1, 1]), nrow = 2, byrow = TRUE)
  
  rownames(conf_matrix_reshaped) <- c("Positive", "Negative")
  colnames(conf_matrix_reshaped) <- c("Positive", "Negative")
  
  conf_matrix_data <- melt(conf_matrix_reshaped)
  conf_matrix_data <- data.frame(True = conf_matrix_data$Var1,
                                 Predicted = conf_matrix_data$Var2,
                                 Count = conf_matrix_data$value)
  
  # Plot Confusion Matrix Heatmap
  ggplot(conf_matrix_data, aes(x=Predicted, y=True, fill=Count)) +
    geom_tile() + geom_text(aes(label=Count), vjust=1, size=4) +
    scale_fill_gradient(low="white", high="red") +
    labs(title="Naive Bayes Confusion Matrix", x="Predicted", y="Actual") +
    theme_minimal()
  
  
  # -------------------------------------- RANDOM FOREST -------------------------------------------
  
  # Convert ProdTaken column to a factor (if it's not already)
  features$ProdTaken <- as.factor(features$ProdTaken)
  best_conf_matrix <- list()
  rf_acc <- numeric()
  highestAcc <- 0
  highestAccForest <- NULL
  
  num_folds <- 5
  fold_indices <- sample(rep(1:num_folds, length.out = nrow(features)))
  
  for (fold in 1:num_folds) {
    training_data <- features[fold_indices != fold, ]
    testing_data <- features[fold_indices == fold, ]
    
    forest_model <- randomForest(ProdTaken ~ ., data = training_data, ntree = 200)
    testing_table <- testing_data
    
    target_column <- testing_table$ProdTaken
    predictions <- predict(forest_model, testing_table)
    
    confusion_matrix <- table(predictions, target_column)
    
    accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    
    rf_acc <- c(rf_acc, accuracy)
    
    if (accuracy > highestAcc) {
      highestAcc <- accuracy
      highestAccForest <- forest_model
      best_conf_matrix <- confusion_matrix
    }
    
    print(highestAcc)
  }
  
  plot(rf_acc, type = "l", ylab = "Accuracy Rate", xlab = "Iterations", main = "Accuracy Rate for Wheat Type With Different Subsets of Data")
  
  cat("Average Accuracy of Random Forest after iteration: ", mean(rf_acc), "\n")
  # Confusion Matrix
  # Precision
  precision <- best_conf_matrix[2, 2] / (best_conf_matrix[2, 2] + best_conf_matrix[1, 2])
  
  # Recall
  recall <- best_conf_matrix[2, 2] / (best_conf_matrix[2, 2] + best_conf_matrix[2, 1])
  
  # F1 score
  f1 <- 2 * (precision * recall) / (precision + recall)
  
  # Results
  cat("Precision:", precision, "\n")
  cat("Recall:", recall, "\n")
  cat("F1 Score:", f1, "\n")
  conf_matrix_reshaped <- matrix(c(best_conf_matrix[2, 2], best_conf_matrix[2, 1], best_conf_matrix[1, 2], best_conf_matrix[1, 1]), nrow = 2, byrow = TRUE)
  
  rownames(conf_matrix_reshaped) <- c("Positive", "Negative")
  colnames(conf_matrix_reshaped) <- c("Positive", "Negative")
  
  conf_matrix_data <- melt(conf_matrix_reshaped)
  conf_matrix_data <- data.frame(True = conf_matrix_data$Var1,
                                 Predicted = conf_matrix_data$Var2,
                                 Count = conf_matrix_data$value)
  
  # Plot Confusion Matrix Heatmap
  ggplot(conf_matrix_data, aes(x=Predicted, y=True, fill=Count)) +
    geom_tile() + geom_text(aes(label=Count), vjust=1, size=4) +
    scale_fill_gradient(low="white", high="red") +
    labs(title="Random Forest Confusion Matrix", x="Predicted", y="Actual") +
    theme_minimal()
