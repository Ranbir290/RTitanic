# Load necessary libraries
library(readr)        # For reading CSV files
library(caret)        # For splitting data and evaluating performance
library(randomForest) # For using the trained Random Forest model
library(e1071)        # For confusion matrix

# Load the preprocessed dataset
train_data <- read_csv("processed_titanic.csv")  # Ensure you save preprocessed data earlier

# Split the dataset into features (X) and target variable (y)
X <- train_data[, -which(names(train_data) == "Survived")]  # Exclude "Survived" column from features
y <- train_data$Survived  # The target variable "Survived"

# Split the dataset into training and testing sets (80% training, 20% testing)
set.seed(42)  # Set a seed for reproducibility of the split
train_index <- createDataPartition(y, p = 0.8, list = FALSE)  # 80% training data
X_train <- X[train_index, ]  # Training set features
y_train <- y[train_index]    # Training set target variable
X_test <- X[-train_index, ]  # Testing set features
y_test <- y[-train_index]    # Testing set target variable

# Load the trained model
rf_model <- readRDS("Random-Forest.rds")  # Load the trained model (R's equivalent of joblib.load)

# Predict using the trained model
y_pred <- predict(rf_model, X_test)  # Make predictions on the test set

# Perform error analysis (misclassified samples)
misclassified <- X_test
misclassified$Actual <- y_test  # Actual values
misclassified$Predicted <- y_pred  # Predicted values
misclassified <- misclassified[misclassified$Actual != misclassified$Predicted, ]  # Filter misclassified rows

# Print misclassified samples
cat("Misclassified Samples:\n")
print(head(misclassified))

# Show confusion matrix
cat("\nConfusion Matrix:\n")
conf_matrix <- confusionMatrix(factor(y_pred), factor(y_test))  # Compute confusion matrix
print(conf_matrix)
