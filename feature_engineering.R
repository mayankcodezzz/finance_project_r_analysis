### **Importing modules**
library(readr)
library(dplyr)
library(lubridate)


# Read CSV files
df_cards <- read_csv("data/cards_preprocessed_data.csv")
df_users <- read_csv("data/users_preprocessed_data.csv")

# Determine retirement status
df_users <- df_users %>% 
  mutate(retirement_status = ifelse(current_age >= retirement_age, "Retired", "Not Retired"))

# Categorize Age Groups
categorize_age <- function(age) {
  if (age <= 30) {
    return("17-30")
  } else if (age <= 45) {
    return("31-45")
  } else if (age <= 60) {
    return("46-60")
  } else {
    return("60+")
  }
}

df_users$age_group <- sapply(df_users$current_age, categorize_age)

# Flag if PIN Change is Due
df_cards <- df_cards %>% 
  mutate(PIN_Change_Due = ifelse(year_pin_last_changed < year(Sys.Date()) - 7, "Yes", "No"))

# Calculate Debt-to-Income Ratio
df_users <- df_users %>% 
  mutate(Debt_to_Income_Ratio = total_debt / yearly_income)

# Merge Data
df_merged <- left_join(df_users, df_cards, by = c("id" = "client_id"))

# Define file path
data_dir <- "data"
file_path_merged <- file.path(data_dir, "merged_data.csv")

# Save merged data
write_csv(df_merged, file_path_merged)
