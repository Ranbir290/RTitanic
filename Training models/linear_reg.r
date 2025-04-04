# Load necessary libraries
library(readr)        # For reading CSV files
library(dplyr)        # For data manipulation
library(caret)        # For data preprocessing and machine learning
library(e1071)        # Needed for caret functions

# Load the preprocessed dataset (Ensure this file is already cleaned)
train_data <- read_csv("processed_titanic.csv")  

# Define features (X) and target variable (y)
X <- train_data %>% select(-Survived)  # Independent variables
y <- train_data$Survived  # Target variable

# Split into 80% training and 20% testing
set.seed(42)  # Ensure reproducibility
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[train_index, ]
X_test <- X[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]

# Standardize the features (recommended for logistic regression)
scaler <- preProcess(X_train, method = c("center", "scale"))
X_train_scaled <- predict(scaler, X_train)
X_test_scaled <- predict(scaler, X_test)

# Train the Logistic Regression model
model <- glm(y_train ~ ., data = X_train_scaled, family = binomial)

# Make predictions (probabilities)
y_pred_prob <- predict(model, X_test_scaled, type = "response")

# Convert probabilities to binary predictions (threshold = 0.5)
y_pred <- ifelse(y_pred_prob > 0.5, 1, 0)

# Evaluate the model
accuracy <- mean(y_pred == y_test)  # Compute accuracy
cat(sprintf("Accuracy: %.4f\n", accuracy))  # Print accuracy score

# Print classification report (using confusionMatrix from caret)
conf_matrix <- confusionMatrix(as.factor(y_pred), as.factor(y_test))

cat("\nClassification Report:\n")
print(conf_matrix)

# Print confusion matrix
cat("\nConfusion Matrix:\n")
print(conf_matrix$table)
