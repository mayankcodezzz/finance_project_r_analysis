# Load required libraries
library(tidyverse)
library(geepack)

# Step 1: Load and transform the data
data <- read.csv("data/merged_data.csv")

# Select relevant columns (replace age_group with gender)
cards_long <- data %>%
  select(id, gender, card_type, PIN_Change_Due, id.y)

# Convert PIN_Change_Due to binary (1 = Yes, 0 = No)
cards_long$PIN_Change_Due <- ifelse(cards_long$PIN_Change_Due == "Yes", 1, 0)

# Convert gender to a factor (assuming "Female" and "Male" as levels)
# Adjust levels based on your actual data
cards_long <- cards_long %>%
  mutate(gender = factor(gender, levels = c("Female", "Male")))

# Check the first few rows
head(cards_long)

# Save the transformed data
write.csv(cards_long, "data/cards_long_transformed.csv", row.names = FALSE)
cat("Transformed data saved as 'cards_long_transformed.csv'\n")

# Step 2: Fit the GEE model (use gender instead of age_group_num)
gee_model <- geeglm(PIN_Change_Due ~ gender + card_type, 
                    id = id, 
                    family = binomial(link = "logit"), 
                    corstr = "exchangeable", 
                    data = cards_long)

# Display the summary
summary(gee_model)

# Perform Type III-style Wald tests
anova(gee_model)