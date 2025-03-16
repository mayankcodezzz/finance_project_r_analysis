# Importing modules
library(readr)
library(dplyr)
library(lubridate)

# Read CSV files
df_cards <- read_csv("data/cards_preprocessed_data.csv")
df_users <- read_csv("data/users_preprocessed_data.csv")

# Feature Engineering for Users Dataframe
# 1. Determine retirement status
df_users <- df_users %>% 
  mutate(retirement_status = ifelse(current_age >= retirement_age, "Retired", "Not Retired"))

# 2. Categorize Age Groups (Suggestion by Tulsi Patel)
categorize_age <- function(age) {
  if (age <= 30) {
    return("18-30") 
  } else if (age <= 45) {
    return("31-45") 
  } else if (age <= 60) {
    return("46-60") 
  } else {
    return("60+") 
  }
}
df_users <- df_users %>%
  mutate(age_group = sapply(current_age, categorize_age))

# 3. Calculate Debt-to-Income Ratio
df_users <- df_users %>% 
  mutate(Debt_to_Income_Ratio = total_debt / yearly_income)

# Feature Engineering for Cards Dataframe
# 1. Flag if PIN Change is Due
df_cards <- df_cards %>% 
  mutate(PIN_Change_Due = ifelse(year_pin_last_changed < year(Sys.Date()) - 7, "Yes", "No"))

# Define file paths
data_dir <- "data"
file_path_users <- file.path(data_dir, "feature_engineered_users.csv")
file_path_cards <- file.path(data_dir, "feature_engineered_cards.csv")

# Save feature-engineered dataframes as separate CSV files
write_csv(df_users, file_path_users)
write_csv(df_cards, file_path_cards)

# Optional: Print confirmation
cat("Feature-engineered users data saved to:", file_path_users, "\n")
cat("Feature-engineered cards data saved to:", file_path_cards, "\n")