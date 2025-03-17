# Load required libraries
library(tidyverse)
library(emmeans)
library(car)
library(stats)
library(nortest)
library(moments)

# Load and preprocess data
df_merged <- read_csv("data/merged_data.csv") %>%
  mutate(across(c("card_brand"), as.factor))

# Calculate total number of cards and distribution across card_brand
card_distribution <- df_merged %>%
  group_by(card_brand) %>%
  dplyr::summarise(count = n()) %>%
  mutate(percentage = round(count / sum(count) * 100, 2))
cat("\nCard Brand Distribution:\n")
print(card_distribution)

# Plot 
ggplot(df_merged, aes(x = credit_limit, y = yearly_income, color = card_brand)) +
  geom_point() +
  labs(x = "Credit Limit",
       y = "Yearly Income") +
  scale_color_discrete(name = "Card Brand", labels = c("Amex", "Discover", "Mastercard", "Visa")) +
  theme_classic()

# Descriptive statistics
desc_stats <- df_merged %>%
  dplyr::summarise(
    across(c(credit_limit, yearly_income),
           list(min = ~min(.),
                q1 = ~quantile(., 0.25),
                median = ~median(.),
                mean = ~mean(.),
                q3 = ~quantile(., 0.75),
                max = ~max(.),
                sd = ~sd(.),
                iqr = ~IQR(.)))
  )
print(desc_stats, width = Inf)

# ANCOVA function
perform_ancova <- function(response, covariate, categorical_var, data) {
  correlation <- cor(data[[response]], data[[covariate]], use = "complete.obs")
  linearity_check <- cor.test(data[[response]], data[[covariate]], method = "pearson")$p.value
  
  formula_full <- as.formula(paste(response, "~", covariate, "+", categorical_var, "+", covariate, ":", categorical_var))
  lm_full <- lm(formula_full, data = data)
  anova_result <- Anova(lm_full, type = "III")
  interaction_p <- anova_result$"Pr(>F)"[4]
  
  formula_ancova <- if (interaction_p > 0.05) {
    as.formula(paste(response, "~", covariate, "+", categorical_var))
  } else {
    formula_full
  }
  lm_ancova <- lm(formula_ancova, data = data)
  ancova_summary <- summary(lm_ancova)
  
  levene_result <- leveneTest(as.formula(paste(response, "~", categorical_var)), data = data)
  variance_p <- levene_result$"Pr(>F)"[1]
  
  residuals <- residuals(lm_ancova)
  ad_test <- ad.test(residuals)
  normality_p <- ad_test$p.value
  skew <- skewness(residuals)
  
  adjusted_means <- emmeans(lm_ancova, specs = categorical_var)
  pairwise_comparisons <- contrast(adjusted_means, method = "pairwise", adjust = "tukey")
  
  return(list(
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

# Run ANCOVA
selected_models <- list(
  list(response = "credit_limit", covariate = "yearly_income", categorical_var = "card_brand")
)
model_numbers <- c(1)
results <- list()

for (i in seq_along(selected_models)) {
  model <- selected_models[[i]]
  results[[i]] <- perform_ancova(model$response, model$covariate, model$categorical_var, df_merged)
  
  cat("\n=================================================\n")
  cat("Model ", model_numbers[i], ": ", model$response, " ~ ", model$categorical_var, " + ", model$covariate, "\n")
  cat("Assumption Checks:\n")
  cat("  - Correlation:", results[[i]]$correlation, "(p =", results[[i]]$linearity_p, ")\n")
  cat("  - Homogeneity of Variance (Levene’s p):", results[[i]]$variance_p, "\n")
  cat("  - Normality (Anderson-Darling p):", results[[i]]$normality_p, "\n")
  cat("  - Skewness:", results[[i]]$skewness, "\n")
  cat("  - Interaction p-value:", results[[i]]$interaction_p, "\n")
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