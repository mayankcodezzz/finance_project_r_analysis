# Load required libraries
library(tidyverse)
library(stats)
library(emmeans)
library(ggplot2)
library(gridExtra)  # For arranging multiple plots
library(lmtest)     # For Breusch-Pagan test
library(lmtest)

# Load and preprocess data
df_merged <- read_csv("data/users_with_credit_limit.csv") %>%
  mutate(
    across(c("retirement_status", "gender", "age_group"), as.factor),
    per_capita_income = per_capita_income + 1,
    credit_limit_user = credit_limit_user + 1,
    yearly_income = yearly_income + 1,
    total_debt = total_debt + 1,
    credit_score=credit_score+1
  )

# Define updated function with residual analysis and plot saving
analyze_glm <- function(formula, data, model_name, cat_var, covar_name, 
                        family = Gamma(link = "identity"), start = NULL) {
  # Fit GLM
  glm_model <- glm(formula, data = data, family = family, start = start)
  
  # Summary
  cat("\n\n### ", model_name, "\n")
  print(summary(glm_model))
  
  # Dispersion parameter
  dispersion <- summary(glm_model)$dispersion
  cat("Dispersion parameter:", dispersion, "\n")
  
  # Adjusted means
  cat("\nAdjusted Means:\n")
  adjusted_means <- emmeans(glm_model, specs = cat_var)
  print(adjusted_means)
  
  # Pairwise comparisons
  cat("\nPairwise Comparisons:\n")
  pairwise <- contrast(adjusted_means, method = "pairwise", adjust = "tukey")
  print(pairwise)
  
  # Residual analysis
  residuals <- residuals(glm_model, type = "deviance")
  fitted <- fitted(glm_model)
  predictor <- data[[cat_var]]
  covariate <- data[[covar_name]]
  
  
  print(bptest(glm_model,student=FALSE))
  print(shapiro.test(residuals))
  
  # Standardized residuals (using rstandard)
  std_residuals <- rstandard(glm_model)
  cat("\nStandardized Residuals Analysis:\n")
  outliers <- which(abs(std_residuals) > 3)
  if (length(outliers) > 0) {
    cat("Potential outliers (standardized residuals > 3):\n")
    print(data[outliers, c("yearly_income", cat_var, covar_name)])
  } else {
    cat("No standardized residuals exceed 3.\n")
  }
  
  # Diagnostic plots
  p1 <- ggplot(data.frame(fitted = fitted, residuals = residuals), 
               aes(x = fitted, y = residuals)) +
    geom_point(alpha = 0.5) + 
    geom_hline(yintercept = 0, lty = 2) +
    labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Deviance Residuals") +
    theme_minimal()
  
  p2 <- ggplot(data.frame(predictor = predictor, residuals = residuals), 
               aes(x = predictor, y = residuals)) +
    geom_boxplot() + 
    geom_hline(yintercept = 0, lty = 2) +
    labs(title = paste("Residuals vs", cat_var), x = cat_var, y = "Deviance Residuals") +
    theme_minimal()
  
  p3 <- ggplot(data.frame(covariate = covariate, residuals = residuals), 
               aes(x = covariate, y = residuals)) +
    geom_point(alpha = 0.5) + 
    geom_hline(yintercept = 0, lty = 2) +
    labs(title = paste("Residuals vs", covar_name), x = covar_name, y = "Deviance Residuals") +
    theme_minimal()
  
  p4 <- ggplot(data.frame(residuals = residuals), 
               aes(sample = residuals)) +
    stat_qq() + stat_qq_line() +
    labs(title = "Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
  
  # Arrange and save plots to PNG
  plot_file <- paste0(gsub("[: ]", "_", model_name), "_plots.png")
  png(plot_file, width = 800, height = 800)
  grid.arrange(p1, p2, p3, p4, ncol = 2, top = model_name)
  dev.off()
  cat("Plots saved to:", plot_file, "\n")
  
  return(glm_model)
}

# Run analysis for all three models
model <- analyze_glm(yearly_income ~ per_capita_income + retirement_status + 
                        per_capita_income:retirement_status, 
                      df_merged, "Model 1: Yearly Income vs Per Capita Income and Retirement Status", 
                      "retirement_status", "per_capita_income")