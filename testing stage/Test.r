library(randomForest)
library(readr)

# Load the trained model
rf_model <- readRDS("Random-Forest.rds")  # Ensure this file exists

# Load the new test dataset
test_data <- read_csv("test_input.csv")

# Predict survival using the trained model
predictions <- predict(rf_model, test_data)

# Add predictions to the test data
test_data$Predicted_Survival <- predictions

# Save the results
write_csv(test_data, "test_predictions.csv")

cat("Predictions saved successfully! 🎯\n")
print(test_data)