# Load required libraries
library(tidyverse)

# Load and preprocess data (same as your "without log" setup)
df_merged <- read_csv("data/merged_data.csv") %>%
  mutate(across(c("retirement_status", "age_group", "PIN_Change_Due", "has_chip", "card_brand", "card_type"), as.factor))

# Function to detect outliers using IQR method
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  outliers <- which(x < lower_bound | x > upper_bound)
  return(outliers)
}

# Identify outliers for credit_limit and yearly_income
credit_limit_outliers <- detect_outliers(df_merged$credit_limit)
yearly_income_outliers <- detect_outliers(df_merged$yearly_income)

# Find unique rows with outliers in either variable
combined_outlier_rows <- unique(c(credit_limit_outliers, yearly_income_outliers))

# Report the number of outliers
n_credit_limit_outliers <- length(credit_limit_outliers)
n_yearly_income_outliers <- length(yearly_income_outliers)
n_combined_outliers <- length(combined_outlier_rows)

cat("Number of outliers in credit_limit:", n_credit_limit_outliers, "\n")
cat("Number of outliers in yearly_income:", n_yearly_income_outliers, "\n")
cat("Total number of unique rows with outliers removed:", n_combined_outliers, "\n")
cat("Original dataset rows:", nrow(df_merged), "\n")

# Remove rows with outliers
df_cleaned <- df_merged[-combined_outlier_rows, ]

# Report the new dataset size
cat("Rows in cleaned dataset:", nrow(df_cleaned), "\n")

# Save the cleaned dataset to a new CSV file
write_csv(df_cleaned, "data/ancovamerged_data_no_outliers.csv")

# Optional: Verify the summary of cleaned data
summary(df_cleaned$credit_limit)
summary(df_cleaned$yearly_income)