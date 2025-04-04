# Load necessary libraries
library(readr)        # For reading CSV files
library(dplyr)        # For data manipulation
library(caret)        # For train-test splitting and evaluation
library(randomForest) # For Random Forest model

# Load the dataset
train_data <- read_csv("processed_titanic.csv")  

# Select features (X) and target variable (y)
X <- train_data %>% select(-Survived)  # All features except 'Survived'
y <- as.factor(train_data$Survived)    # Convert 'Survived' to a factor

# Split the data into training (80%) and testing (20%) sets
set.seed(42)  # Ensure reproducibility
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[train_index, ]
X_test <- X[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]

# Initialize and train the Random Forest model
rf_model <- randomForest(x = X_train, y = y_train, ntree = 200, random_state = 42)

# Predict on the test data
y_pred_rf <- predict(rf_model, X_test)

# Evaluate the model's performance
accuracy_rf <- mean(y_pred_rf == y_test)  # Compute accuracy
cat(sprintf("\nRandom Forest Model Accuracy: %.4f\n", accuracy_rf))

# Print classification report
conf_matrix <- confusionMatrix(y_pred_rf, y_test)

cat("\nClassification Report:\n")
print(conf_matrix)

# Print confusion matrix
cat("\nConfusion Matrix:\n")
print(conf_matrix$table)

# Save the trained model
saveRDS(rf_model, "Random-Forest.rds")
cat("\n✅ Model saved successfully as 'Random-Forest.rds'\n")
