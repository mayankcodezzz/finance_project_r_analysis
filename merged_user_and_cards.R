### **Importing modules**
library(readr)
library(dplyr)
library(lubridate)


# Read CSV files
df_cards <- read_csv("data/feature_engineered_cards.csv")
df_users <- read_csv("data/feature_engineered_users.csv")

# Merge Data
df_merged <- left_join(df_users, df_cards, by = c("id" = "client_id"))

# Define file path
data_dir <- "data"
file_path_merged <- file.path(data_dir, "merged_data.csv")

# Save merged data
write_csv(df_merged, file_path_merged)
