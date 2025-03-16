# Load required libraries
library(tidyverse)
library(emmeans)
library(car)
library(stats)
library(nortest)
library(moments)

# Load and preprocess data
df_merged <- read_csv("data/merged_data.csv") %>%
  mutate(
    across(c("retirement_status", "age_group","PIN_Change_Due","has_chip","card_brand","card_type"), as.factor),
    credit_limit = sqrt(credit_limit) , # Add 1 to avoid log(0)
    yearly_income = sqrt(yearly_income + 1)
  )

# ANCOVA function (fixed and streamlined)
perform_ancova <- function(response, covariate, categorical_var, data) {
  # Check linearity
  correlation <- cor(data[[response]], data[[covariate]], use = "complete.obs")
  linearity_check <- cor.test(data[[response]], data[[covariate]], method = "pearson")$p.value
  
  # Check interaction (homogeneity of slopes)
  formula_full <- as.formula(paste(response, "~", covariate, "+", categorical_var, "+", covariate, ":", categorical_var))
  lm_full <- lm(formula_full, data = data)
  anova_result <- Anova(lm_full, type = "III")
  interaction_p <- anova_result$"Pr(>F)"[4]
  
  # Choose model based on interaction
  if (interaction_p > 0.05) {
    formula_ancova <- as.formula(paste(response, "~", covariate, "+", categorical_var))
  } else {
    formula_ancova <- formula_full
  }
  lm_ancova <- lm(formula_ancova, data = data)
  ancova_summary <- summary(lm_ancova)
  
  # Assumption checks
  levene_result <- leveneTest(as.formula(paste(response, "~", categorical_var)), data = data)
  variance_p <- levene_result$"Pr(>F)"[1]
  
  residuals <- residuals(lm_ancova)
  ad_test <- ad.test(residuals)  # Fixed: Anderson-Darling test
  normality_p <- ad_test$p.value
  skew <- skewness(residuals)
  
  # Adjusted means and pairwise comparisons
  adjusted_means <- emmeans(lm_ancova, specs = categorical_var)
  pairwise_comparisons <- contrast(adjusted_means, method = "pairwise", adjust = "tukey")
  
  return(list(
    response = response,
    covariate = covariate,
    categorical_var = categorical_var,
    correlation = correlation,
    linearity_p = linearity_check,
    interaction_p = interaction_p,
    r_squared = ancova_summary$r.squared,
    covariate_p = ancova_summary$coefficients[2, 4],
    any_cat_significant = any(ancova_summary$coefficients[grep(categorical_var, rownames(ancova_summary$coefficients)), 4] < 0.05, na.rm = TRUE),
    normality_p = normality_p,
    variance_p = variance_p,
    skewness = skew,
    ancova_summary = ancova_summary,
    adjusted_means = adjusted_means,
    pairwise_comparisons = pairwise_comparisons
  ))
}

# Selected models
selected_models <- list(
  list(response = "credit_limit", covariate = "yearly_income", categorical_var = "card_brand"), # 1
  list(response = "credit_limit", covariate = "yearly_income", categorical_var = "card_type") # 2
)

# Model numbers for reference
model_numbers <- c(1,2)

# Run ANCOVA on selected models
results <- list()
for (i in seq_along(selected_models)) {
  model <- selected_models[[i]]
  results[[i]] <- perform_ancova(model$response, model$covariate, model$categorical_var, df_merged)
  
  cat("\n=================================================\n")
  cat("Model ", model_numbers[i], ": ", model$response, " ~ ", model$covariate, " + ", model$categorical_var, "\n")
  cat("Assumption Checks:\n")
  cat("  - Correlation (Linear Relationship):", results[[i]]$correlation, "(p =", results[[i]]$linearity_p, ")\n")
  cat("  - Homogeneity of Variance (Levene’s p):", results[[i]]$variance_p, "\n")
  cat("  - Normality of Residuals (Anderson-Darling p):", results[[i]]$normality_p, "\n")
  cat("  - Residual Skewness:", results[[i]]$skewness, "\n")
  cat("  - Interaction p-value (Homogeneity of Slopes):", results[[i]]$interaction_p, "\n")
  cat("Model Results:\n")
  cat("  - R-squared:", results[[i]]$r_squared, "\n")
  cat("  - Covariate p-value:", results[[i]]$covariate_p, "\n")
  cat("  - Any Categorical Term Significant:", results[[i]]$any_cat_significant, "\n")
  cat("\nANCOVA Summary:\n")
  print(results[[i]]$ancova_summary)
  cat("\nAdjusted Means:\n")
  print(results[[i]]$adjusted_means)
  cat("\nPairwise Comparisons:\n")
  print(results[[i]]$pairwise_comparisons)
}

