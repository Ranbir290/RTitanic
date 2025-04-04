# Load necessary libraries
library(randomForest)
library(caret)  # For train-test split and evaluation metrics
library(ggplot2)  # For visualization
library(pheatmap)  # For confusion matrix heatmap

# Ensure the dataset exists before loading
file_path <- "processed_titanic.csv"
if (!file.exists(file_path)) {
  stop("❌ Dataset not found! Check the file path.")
}

# Load the dataset
train_data <- read.csv(file_path)

# Define features (X) and target variable (y)
X <- train_data[, !(names(train_data) %in% c("Survived"))]  # Drop "Survived"
y <- as.factor(train_data$Survived)  # Convert target variable to factor

# Handle missing values (replace NA with column median)
X[is.na(X)] <- apply(X, 2, function(x) median(x, na.rm = TRUE))

# Convert categorical variables if needed
X <- model.matrix(~ . -1, data = X)  # One-hot encoding without intercept

# Split data into training (80%) and testing (20%)
set.seed(42)
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[train_index, ]
X_test <- X[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]

# Train Random Forest Model with Best Parameters
rf_best <- randomForest(
  x = X_train, 
  y = y_train, 
  ntree = 200, 
  maxnodes = 20, 
  nodesize = 1, 
  mtry = sqrt(ncol(X_train)), 
  importance = TRUE
)

# Predictions
y_pred <- predict(rf_best, X_test)

# Evaluate Model Performance
accuracy <- sum(y_pred == y_test) / length(y_test)
cat(sprintf("🚀 Tuned Random Forest Accuracy: %.4f\n", accuracy))

# Classification Report
conf_matrix <- confusionMatrix(y_pred, y_test)
print(conf_matrix)

# Confusion Matrix Visualization
pheatmap(
  conf_matrix$table, 
  color = colorRampPalette(c("white", "blue"))(100),
  display_numbers = TRUE,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  main = "🌀 Confusion Matrix"
)
