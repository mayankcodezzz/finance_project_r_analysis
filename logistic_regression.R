# Load required libraries
library(tidyverse)
library(car)
library(lmtest)
library(pROC)
library(ggplot2)

# Load dataset
df_users <- read.csv("data/feature_engineered_users.csv")

# Creating binary response variable: DTI > 0.8
df_users <- df_users %>% 
  mutate(DTI_high = ifelse(Debt_to_Income_Ratio > 0.8, 1, 0))

# Subset to relevant variables
df_logistic <- df_users %>% 
  select(id, DTI_high, current_age, credit_score)

# Descriptive statistics for explanatory variables
desc_stats <- df_logistic %>%
  dplyr::summarise(
    across(c(current_age, credit_score),
           list(min = ~min(.),
                q1 = ~quantile(., 0.25),
                median = ~median(.),
                mean = ~mean(.),
                q3 = ~quantile(., 0.75),
                max = ~max(.),
                sd = ~sd(.),
                iqr = ~IQR(.)))
  )
print("Descriptive Statistics:")
print(desc_stats)

# Proportion of DTI_high
prop_DTI_high <- mean(df_logistic$DTI_high)
print(paste("Overall Proportion of users with DTI > 0.8:", round(prop_DTI_high, 4)))

# Fitting logistic regression model
logit_model <- glm(DTI_high ~ current_age + credit_score, 
                   family = binomial, data = df_logistic)
print("Logistic Regression Model Summary:")
print(summary(logit_model)$coefficients)

# --- Assumption Tests ---

# 1. Test for Linearity of Predictors in the Logit (Box-Tidwell Test)
# Add interaction terms between each predictor and its log to test for linearity
df_logistic <- df_logistic %>%
  mutate(current_age_log = current_age * log(current_age),
         credit_score_log = credit_score * log(credit_score))

# 3. Test for Multicollinearity (Correlation and Variance Inflation Factor - VIF)
# Correlation between current_age and credit_score
correlation <- cor(df_logistic$current_age, df_logistic$credit_score)
print("Correlation between current_age and credit_score:")
print(correlation)

# Variance Inflation Factor (VIF)
vif_values <- vif(logit_model)
print("Variance Inflation Factor (VIF) for Predictors:")
print(vif_values)

# 4. Large Sample Size (Already satisfied, but confirm)
sample_size <- nrow(df_logistic)
print("Sample Size Check:")
print(paste("Sample size:", sample_size))
if (sample_size >= 2000) {
  print("Large sample size assumption satisfied (>= 2000).")
} else {
  print("Sample size may be too small for robust inference.")
}

# --- Continue with the rest of the analysis ---

# Fitted model equation
coeffs <- coef(logit_model)
fitted_eq <- paste("logit[P(DTI_high = 1)] =", round(coeffs[1], 4), "+", 
                   round(coeffs[2], 4), "*current_age +", 
                   round(coeffs[3], 4), "*credit_score")
print(fitted_eq)

# Odds ratios
odds_ratios <- exp(coeffs)
print("Odds Ratios for Predictors:")
print(odds_ratios)

# Confidence intervals
print("95% Profile Likelihood Confidence Intervals for Odds Ratios:")
print(exp(confint(logit_model)))
print("95% Wald Confidence Intervals for Odds Ratios:")
print(exp(confint.default(logit_model)))

# Predicted probabilities at min and max age
new_data <- data.frame(
  current_age = c(min(df_logistic$current_age), max(df_logistic$current_age)),
  credit_score = mean(df_logistic$credit_score)
)
pred_probs <- predict(logit_model, new_data, type = "response")
print("Predicted Probabilities at Min and Max Age (Mean Credit Score):")
print(pred_probs)

# Graph 1: Distribution of Current Age (Histogram)
p1 <- ggplot(df_logistic, aes(x = current_age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(x = "Current Age (years)", y = "Count") +
  theme_classic()
print("Generating Histogram of Current Age")
print(p1)

# Graph 2: Scatterplot of Current Age vs. DTI_high (Raw Data)
p2 <- ggplot(df_logistic, aes(x = current_age, y = DTI_high)) +
  geom_point() +
  labs(x = "Current Age (years)", y = "P(DTI_high = 1): High Debt-to-Income Ratio") +
  theme_classic()
print("Generating Scatterplot of Current Age vs DTI_high")
print(p2)

# Graph 3: Scatterplot of Credit Score vs. DTI_high
p3 <- ggplot(df_logistic, aes(x = credit_score, y = DTI_high)) +
  geom_point() +
  labs(x = "Credit Score", y = "P(DTI_high = 1): High Debt-to-Income Ratio") +
  theme_classic()
print("Generating Scatterplot of Credit Score vs DTI_high")
print(p3)

# Graph 4: Scatterplot with Fitted Logistic Curve (Current Age vs. DTI_high)
p4 <- ggplot(df_logistic, aes(x = current_age, y = DTI_high)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(x = "Current Age (years)", y = "P(DTI_high = 1): High Debt-to-Income Ratio") +
  theme_classic()
print("Generating Scatterplot with Fitted Curve (Current Age vs DTI_high)")
print(p4)

# Graph 5: Scatterplot with Confidence Bands (Current Age vs DTI_high)
p5 <- ggplot(df_logistic, aes(x = current_age, y = DTI_high)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Current Age (years)", y = "P(DTI_high = 1): High Debt-to-Income Ratio") +
  theme_classic()
print("Generating Scatterplot with Confidence Bands (Current Age vs DTI_high)")
print(p5)

# Likelihood Ratio Test
LRT <- logit_model$null.deviance - logit_model$deviance
df_LRT <- logit_model$df.null - logit_model$df.residual
p_LRT <- 1 - pchisq(LRT, df_LRT)
print(paste("LRT Statistic:", round(LRT, 4), "with", df_LRT, "df, p-value:", round(p_LRT, 6)))

# Type III Analysis of Deviance
print("Type III Analysis of Deviance:")
print(Anova(logit_model, type = "III"))

# Classification Table (Cutoff = 0.5)
pred_prob <- predict(logit_model, type = "response")
pred_class <- ifelse(pred_prob > 0.5, 1, 0)
class_tab <- table(df_logistic$DTI_high, pred_class)
rownames(class_tab) <- c("DTI_low", "DTI_high")
colnames(class_tab) <- c("Pred_low", "Pred_high")
print("Classification Table (Cutoff = 0.5):")
print(class_tab)

# Sensitivity, Specificity, and Correct Classification
sensitivity <- class_tab[2, 2] / sum(class_tab[2, ])
specificity <- class_tab[1, 1] / sum(class_tab[1, ])
correct_class <- (class_tab[1, 1] + class_tab[2, 2]) / sum(class_tab)
print(paste("Sensitivity:", round(sensitivity, 4)))
print(paste("Specificity:", round(specificity, 4)))
print(paste("Correct Classification Rate:", round(correct_class, 4)))

# Classification Table (Cutoff = Sample Proportion)
pi0 <- mean(df_logistic$DTI_high)
pred_class_pi0 <- ifelse(pred_prob > pi0, 1, 0)
class_tab_pi0 <- table(df_logistic$DTI_high, pred_class_pi0)
rownames(class_tab_pi0) <- c("DTI_low", "DTI_high")
colnames(class_tab_pi0) <- c("Pred_low", "Pred_high")
print(paste("Classification Table (Cutoff = Sample Proportion:", round(pi0, 4), ")"))
print(class_tab_pi0)

# Graph 6: ROC Curve
roc_obj <- roc(df_logistic$DTI_high, pred_prob)
auc_val <- round(auc(roc_obj), 4)
print(paste("Computed AUC Value:", auc_val))
p6 <- ggroc(roc_obj, legacy.axes = TRUE) +
  labs(x = "1 - Specificity", y = "Sensitivity", title = paste("ROC Curve (AUC =", auc_val, ")")) +
  theme_classic() +
  annotate("segment", x = 0, xend = 1, y = 0, yend = 1, lty = 2)
print("Generating ROC Curve")
print(p6)

# Multiple Correlation
mult_corr <- cor(df_logistic$DTI_high, fitted(logit_model))
print(paste("Multiple Correlation between Observed and Fitted Values:", round(mult_corr, 4)))

# Confidence Interval at Mean
new_width <- data.frame(
  current_age = mean(df_logistic$current_age),
  credit_score = mean(df_logistic$credit_score)
)
logit_pi_hat <- predict(logit_model, new_width, se = TRUE)
LL <- logit_pi_hat$fit - qnorm(0.975) * logit_pi_hat$se.fit
UL <- logit_pi_hat$fit + qnorm(0.975) * logit_pi_hat$se.fit
LL_pi <- exp(LL) / (1 + exp(LL))
UL_pi <- exp(UL) / (1 + exp(UL))
print("95% Confidence Interval for P(DTI_high = 1) at Mean Current Age and Credit Score:")
print(cbind(LL_pi, UL_pi))