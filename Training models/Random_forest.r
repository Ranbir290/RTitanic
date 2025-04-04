# Load necessary libraries
library(readr)        # For reading CSV files
library(dplyr)        # For data manipulation
library(caret)        # For data preprocessing and model evaluation
library(randomForest) # For Random Forest Classifier

# Load the preprocessed dataset
train_data <- read_csv("processed_titanic.csv")  # Load preprocessed dataset

# Define features (X) and target variable (y)
X <- train_data %>% select(-Survived)  # Independent variables
y <- as.factor(train_data$Survived)    # Target variable (converted to factor)

# Split into 80% training and 20% testing
set.seed(42)  # Ensure reproducibility
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[train_index, ]
X_test <- X[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]

# Train the Random Forest Classifier
rf_model <- randomForest(x = X_train, y = y_train, ntree = 100, random_state = 42)

# Make predictions
y_pred_rf <- predict(rf_model, X_test)

# Evaluate the model
accuracy_rf <- mean(y_pred_rf == y_test)  # Compute accuracy
cat(sprintf("Random Forest Accuracy: %.4f\n", accuracy_rf))  # Print accuracy score

# Print classification report
conf_matrix <- confusionMatrix(y_pred_rf, y_test)

cat("\nClassification Report:\n")
print(conf_matrix)

# Print confusion matrix
cat("\nConfusion Matrix:\n")
print(conf_matrix$table)
