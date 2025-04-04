# Load necessary libraries
library(readr)   # For reading CSV files
library(dplyr)   # For data manipulation

# Load the dataset
train_data <- read_csv("train.csv")

# Fill missing values
train_data$Age[is.na(train_data$Age)] <- median(train_data$Age, na.rm = TRUE)
train_data$Embarked[is.na(train_data$Embarked)] <- names(which.max(table(train_data$Embarked)))

# Drop unnecessary columns
train_data <- train_data %>% select(-Cabin, -Name, -Ticket, -PassengerId)

# Convert 'Sex' to numerical
train_data$Sex <- ifelse(train_data$Sex == "male", 0, 1)

# One-Hot Encoding for 'Embarked' column
train_data <- train_data %>%
  mutate(Embarked_Q = ifelse(Embarked == "Q", 1, 0),
         Embarked_S = ifelse(Embarked == "S", 1, 0)) %>%
  select(-Embarked)  # Drop original 'Embarked' column after encoding

# Select features and target variable
target <- "Survived"
features <- c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked_Q", "Embarked_S")

# Prepare the feature matrix (X) and target vector (y)
X <- train_data %>% select(all_of(features))
y <- train_data %>% pull(Survived)

# ✅ Print to confirm
cat("Features Selected for Model Training:\n")
print(head(X))

cat("\nTarget (Survived):\n")
print(head(y))

# Save the processed dataset
write_csv(train_data, "processed_titanic.csv")
