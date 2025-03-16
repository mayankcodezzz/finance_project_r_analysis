# Load necessary libraries
library(ggplot2)
library(GGally)  # Add this for ggpairs()

# Load and preprocess data
data <- read_csv("data/merged_data.csv")
# Select relevant columns for pairwise comparison
selected_data <- data[, c("yearly_income", "credit_limit")]

# Pairwise scatterplot with correlations
ggpairs(selected_data, columnLabels = c("Yearly Income", "Credit Limit"))

# Boxplot of Yearly Income
ggplot(data, aes(y = yearly_income)) +
  geom_boxplot(fill = "yellow", color = "black") +
  labs(title = "",
       y = "Yearly Income") +
  theme_minimal()

# Boxplot of Credit Limit
ggplot(data, aes(y = credit_limit)) +
  geom_boxplot(fill = "yellow", color = "black") +
  labs(title = "",
       y = "Credit Limit") +
  theme_minimal()