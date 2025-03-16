# Load libraries
library(VGAM)
library(tidyverse)

# Step 2: Read and Prepare the Data
# Read the CSV file (adjust the path as needed)
credit_data <- read.csv("data/merged_data.csv")

# Subset relevant columns
credit_data_subset <- credit_data[, c("card_brand", "credit_score", "yearly_income", "gender")]

# Ensure gender is a factor (for proper modeling)
credit_data_subset$gender <- as.factor(credit_data_subset$gender)

# Step 3: Fit the Multicategory Logit Model
# Fit the model with Mastercard as the baseline (default if alphabetically last)
model1 <- vglm(card_brand ~ credit_score + yearly_income + gender, 
               family = multinomial, 
               data = credit_data_subset)

# View coefficients in matrix form
coef(model1, matrix = TRUE)

# Step 4: Model Summary
# Summarize the model to check coefficient significance
summary(model1)

# Step 5: Likelihood-Ratio Test
# Fit the intercept-only (null) model
model0 <- vglm(card_brand ~ 1, 
               family = multinomial, 
               data = credit_data_subset)

# Perform likelihood-ratio test to compare models
lrtest(model0, model1)

# Step 6: Estimating Response Probabilities
# Calculate max credit score and average yearly income for prediction
max_credit_score <- max(credit_data_subset$credit_score)
avg_yearly_income <- mean(credit_data_subset$yearly_income)

# Create new data frame for prediction (Female with max credit score and avg income)
new_data <- data.frame(
  credit_score = max_credit_score,
  yearly_income = avg_yearly_income,
  gender = "Female"
)

# Predict response probabilities
predicted_probs <- predict(model1, new_data, type = "response")
print(predicted_probs)

# Step 7: Plotting Estimated Response Probabilities
# Generate fitted values for all observations
fits <- bind_cols(fitted(model1), credit_score = credit_data_subset$credit_score)

# Reshape data for plotting
fits_long <- fits %>%
  rename("Visa" = V1, "Mastercard" = V2) %>%  # Adjust names based on actual categories
  pivot_longer(cols = c(Visa, Mastercard), names_to = "name", values_to = "Prob")

# Create the plot
ggplot(fits_long, aes(x = credit_score, y = Prob, color = name)) +
  geom_line() +
  ylim(0, 1) +
  xlim(min(credit_data_subset$credit_score), max(credit_data_subset$credit_score)) +
  scale_color_discrete(name = "Card Brand") +
  labs(x = "Credit Score", y = "Predicted Probability") +
  theme_classic()

# Save the plot to a file
ggsave("card_brand_probabilities.png")

# Step 8: Checking Model Assumptions and Goodness of Fit
# Since explanatory variables are continuous, residual deviance test isn't valid
# Instead, compare to a more complex model with interaction

# Fit model with credit_score:gender interaction
model2 <- vglm(card_brand ~ credit_score + yearly_income + gender + credit_score:gender, 
               family = multinomial, 
               data = credit_data_subset)

# Perform likelihood-ratio test to compare models
lrtest(model1, model2)

# Step 9: Informal Residual Inspection
# Extract fitted probabilities and observed outcomes
fitted_probs <- fitted(model1)
observed <- credit_data_subset$card_brand

# Combine for inspection (first few rows)
inspection <- cbind(observed, fitted_probs)
head(inspection)

# Step 10: Additional Assumption Checks
# Check for independence assumption (manual inspection of data structure)
# Look for repeated IDs if 'id' column exists
if ("id" %in% names(credit_data)) {
  id_counts <- table(credit_data$id)
  print("Number of observations per ID:")
  summary(id_counts)
  if (max(id_counts) > 1) {
    warning("Repeated IDs detected; independence assumption may be violated.")
  }
}

# End of Analysis