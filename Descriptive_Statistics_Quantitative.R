# Load necessary libraries
library(readr)
library(dplyr)

# Read the CSV file
df_merged <- read_csv("data/users_with_credit_limit.csv")

# Function to generate the statistics table
generate_statistics <- function(data, selected_columns) {
  # Calculate statistics for each column
  stats_list <- lapply(data[selected_columns], function(x) {
    c(
      Sample_Size = sum(!is.na(x)),
      Min = min(x, na.rm = TRUE),
      Q1 = quantile(x, 0.25, na.rm = TRUE),
      Median = median(x, na.rm = TRUE),
      Mean = mean(x, na.rm = TRUE),
      Q3 = quantile(x, 0.75, na.rm = TRUE),
      Max = max(x, na.rm = TRUE),
      SD = sd(x, na.rm = TRUE),
      IQR = IQR(x, na.rm = TRUE)
    )
  })
  
  # Convert list to data frame
  stats_table <- as.data.frame(stats_list)
  
  # Rename the rows with the statistic names
  rownames(stats_table) <- c("Sample Size", "Min", "1st Q", "Median", "Mean", "3rd Q", "Max", "SD", "IQR")
  
  return(stats_table)
}

# Example: Select columns for which you want statistics
selected_columns <- c("current_age", "retirement_age", "per_capita_income", "yearly_income", "total_debt", "credit_score", "credit_limit_user")

# Generate the statistics table
stats_table <- generate_statistics(df_merged, selected_columns)

# Print the table
print(stats_table)
