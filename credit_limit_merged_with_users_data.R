# Load necessary library
library(dplyr)

# Read the users.csv file
users <- read.csv("data/feature_engineered_users.csv", stringsAsFactors = FALSE)

# Read the cards.csv file
cards <- read.csv("data/feature_engineered_cards.csv", stringsAsFactors = FALSE)
# Convert credit_limit to numeric (if it's not already)
cards$credit_limit <- as.numeric(cards$credit_limit)

# Summing up the credit limit for each user
credit_limit_user <- cards %>%
  group_by(client_id) %>%
  summarise(credit_limit_user = sum(credit_limit, na.rm = TRUE))

# Merge with the users dataset
merged_data <- users %>%
  left_join(credit_limit_user, by = c("id" = "client_id"))

# Replace NA values in credit_limit_user with 0
merged_data$credit_limit_user[is.na(merged_data$credit_limit_user)] <- 0

# Ensure credit_limit_user is numeric
merged_data$credit_limit_user <- as.numeric(merged_data$credit_limit_user)

# Save the result to a new CSV file
write.csv(merged_data, "data/users_with_credit_limit.csv", row.names = FALSE)

# Print confirmation message
print("Merged CSV file created successfully: users_with_credit_limit.csv")
