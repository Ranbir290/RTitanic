# Load necessary libraries
library(readr)   # For reading CSV files
library(dplyr)   # For data manipulation

# Load the dataset (Ensure the correct path)
train_data <- read_csv("train.csv")

# Fill missing Age values with the median age
train_data$Age[is.na(train_data$Age)] <- median(train_data$Age, na.rm = TRUE)

# Fill missing Embarked values with the most common port (mode)
train_data$Embarked[is.na(train_data$Embarked)] <- names(which.max(table(train_data$Embarked)))

# Drop unnecessary columns (Cabin, Name, Ticket, PassengerId)
train_data <- train_data %>% select(-Cabin, -Name, -Ticket, -PassengerId)

# Convert 'Sex' column to numerical (male = 0, female = 1)
train_data$Sex <- ifelse(train_data$Sex == "male", 0, 1)

# Convert 'Embarked' column to numerical using one-hot encoding
train_data <- train_data %>%
  mutate(across(Embarked, as.factor)) %>%  # Convert Embarked to a factor
  model.matrix(~ Embarked - 1, data = .) %>%  # One-hot encoding
  as.data.frame() %>%  # Convert matrix back to data frame
  bind_cols(train_data %>% select(-Embarked))  # Combine with the original data (without Embarked)

# ✅ Print to check the first few rows
print("First 5 rows after preprocessing:")
print(head(train_data))

# ✅ Print to check missing values
print("\nMissing values after preprocessing:")
print(colSums(is.na(train_data)))
