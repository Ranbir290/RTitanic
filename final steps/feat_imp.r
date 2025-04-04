# Load necessary libraries
library(readr)        # For reading CSV files
library(randomForest) # For handling Random Forest models
library(ggplot2)      # For plotting
library(dplyr)        # For data manipulation

# Load the preprocessed data
train_data <- read_csv("processed_titanic.csv")  # Ensure the correct path for the dataset

# Define your features (X) and target (y) columns
X <- train_data[, -which(names(train_data) == "Survived")]  # All features except 'Survived'
y <- train_data$Survived  # Target variable

# Load the best Random Forest model
rf_best <- readRDS("Random-Forest.rds")  # Load the trained Random Forest model

# Feature Importances
feature_importances <- rf_best$importance[, 1]  # Extract feature importance values

# Create a DataFrame with feature importances
feature_importance_df <- data.frame(
  Feature = names(X),      # Feature names
  Importance = feature_importances  # Importance values
)

# Sort the features by importance in descending order
feature_importance_df <- feature_importance_df %>%
  arrange(desc(Importance))

# Plot the feature importances using ggplot2
ggplot(feature_importance_df, aes(x = Importance, y = reorder(Feature, Importance))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Feature Importance in Random Forest Model", x = "Importance", y = "Feature")
