library(readr)
library(dplyr)

# Load the dataset
train_data <- read_csv("train.csv")
test_data <- read_csv("test.csv")

# Display first few rows
print(head(train_data))

# Check data information
print(str(train_data))

# Check for missing values
print(colSums(is.na(train_data)))
