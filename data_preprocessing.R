### **Importing modules**
library(readr)
library(dplyr)
library(lubridate)

# Read CSV files
df_cards <- read_csv("data/cards_data.csv")
df_users <- read_csv("data/users_data.csv")

# Drop the 'card_on_dark_web' column
df_cards <- df_cards %>% select(-card_on_dark_web)

# Convert currency columns to numeric
df_cards$credit_limit <- as.numeric(gsub("[$,]", "", df_cards$credit_limit))
df_users$per_capita_income <- as.numeric(gsub("[$,]", "", df_users$per_capita_income))
df_users$yearly_income <- as.numeric(gsub("[$,]", "", df_users$yearly_income))
df_users$total_debt <- as.numeric(gsub("[$,]", "", df_users$total_debt))

# Convert account open date to date format
df_cards$acct_open_date <- parse_date_time(df_cards$acct_open_date, orders = "m/Y", quiet = TRUE)

# Define file paths
data_dir <- "data"
file_path_cards <- file.path(data_dir, "cards_preprocessed_data.csv")
file_path_users <- file.path(data_dir, "users_preprocessed_data.csv")

# Save preprocessed data
write_csv(df_cards, file_path_cards)
write_csv(df_users, file_path_users)