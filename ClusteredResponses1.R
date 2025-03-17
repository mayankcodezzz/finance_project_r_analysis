# Load required libraries
library(tidyverse)
library(geepack)

# Step 1: Load and transform the data
data <- read.csv("data/merged_data.csv")

# Select relevant columns 
cards_long <- data %>%
  select(id, age_group, card_type, PIN_Change_Due, id.y)

# Convert PIN_Change_Due to binary (1 = Yes, 0 = No)
cards_long$PIN_Change_Due <- ifelse(cards_long$PIN_Change_Due == "Yes", 1, 0)

# Convert age_group to ordinal numeric
cards_long <- cards_long %>%
  mutate(age_group_num = case_when(
    age_group == "18-30" ~ 1,
    age_group == "31-45" ~ 2,
    age_group == "46-60" ~ 3,
    age_group == "60+" ~ 4
  ))

# Check the first few rows
cat("First few rows of transformed data:\n")
print(head(cards_long))

# Save the transformed data
write.csv(cards_long, "data/cards_long_transformed.csv", row.names = FALSE)
cat("Transformed data saved as 'cards_long_transformed.csv'\n")

# Step 2: Descriptive Statistics (New Section)
# Overall summary for PIN_Change_Due
overall_summary <- cards_long %>%
  dplyr::summarise(
    Sample_Size = n(),
    Count_No = sum(PIN_Change_Due == 0),
    Count_Yes = sum(PIN_Change_Due == 1),
    Proportion_Yes = mean(PIN_Change_Due)
  )

# Descriptive statistics by age_group
age_group_summary <- cards_long %>%
  group_by(age_group) %>%
  dplyr::summarise(
    Sample_Size = n(),
    Count_No = sum(PIN_Change_Due == 0),
    Count_Yes = sum(PIN_Change_Due == 1),
    Proportion_Yes = mean(PIN_Change_Due)
  )

# Descriptive statistics by card_type
card_type_summary <- cards_long %>%
  group_by(card_type) %>%
  dplyr::summarise(
    Sample_Size = n(),
    Count_No = sum(PIN_Change_Due == 0),
    Count_Yes = sum(PIN_Change_Due == 1),
    Proportion_Yes = mean(PIN_Change_Due)
  )

# Descriptive statistics by age_group and card_type (cross-tabulation)
cross_summary <- cards_long %>%
  group_by(age_group, card_type) %>%
  dplyr::summarise(
    Sample_Size = n(),
    Count_No = sum(PIN_Change_Due == 0),
    Count_Yes = sum(PIN_Change_Due == 1),
    Proportion_Yes = mean(PIN_Change_Due)
  )

# Display descriptive statistics
cat("\nDescriptive Statistics:\n")
cat("Overall PIN_Change_Due:\n")
print(overall_summary)
cat("\nBy Age Group:\n")
print(age_group_summary)
cat("\nBy Card Type:\n")
print(card_type_summary)
cat("\nCross-Tabulation (Age Group x Card Type):\n")
print(cross_summary)

# Step 3: Fit the GEE model
gee_model <- geeglm(PIN_Change_Due ~ age_group_num + card_type, 
                    id = id, 
                    family = binomial(link = "logit"), 
                    corstr = "exchangeable", 
                    data = cards_long)

# Display the summary
cat("\nGEE Model Summary:\n")
print(summary(gee_model))

# Perform Type III-style Wald tests
cat("\nType III Wald Tests:\n")
print(anova(gee_model))