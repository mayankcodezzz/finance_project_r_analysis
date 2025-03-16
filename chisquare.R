# Load necessary libraries
library(dplyr)       # For data manipulation
library(ggplot2)     # For visualization
library(epitools)    # For odds ratios and risk ratios
library(vcd)         # For mosaic plots and association measures
library(DescTools)   # For G-test and additional association measures
library(vcdExtra)    # For CMH test (linear trend)

# Read the data and convert to factors
data <- read.csv("data/merged_data.csv") %>%
  mutate(across(c("age_group", "PIN_Change_Due", "gender"), as.factor))

# Create a contingency table for age_group and PIN_Change_Due
contingency_table <- table(data$age_group, data$PIN_Change_Due)
print("Contingency Table (Counts):")
print(contingency_table)

# Add row and column totals
table_with_totals <- addmargins(contingency_table)
print("Contingency Table with Totals:")
print(table_with_totals)

# Conditional distribution (proportions by row)
conditional_dist <- prop.table(contingency_table, 1)
print("Conditional Distribution (Proportions by Age Group):")
print(round(conditional_dist, 3))

# --- Pearson Chi-Square Test ---
chi_test <- chisq.test(contingency_table, correct = FALSE)
print("Pearson's Chi-Square Test Results:")
print(chi_test)

# Expected counts to verify assumptions
print("Expected Counts:")
print(round(chi_test$expected, 2))

# Check if any expected counts are below 5
if (any(chi_test$expected < 5)) {
  print("Warning: Some expected counts are below 5. Chi-square test may be unreliable.")
  print("Consider Fisher's Exact Test for small counts:")
  fisher_result <- fisher.test(contingency_table, simulate.p.value = TRUE)
  print(fisher_result)
} else {
  print("All expected counts are >= 5. Chi-square test is reliable.")
}

# Standardized residuals
print("Standardized Residuals:")
print(round(chi_test$stdres, 3))

# --- Likelihood Ratio Test (G-test) ---
g_test <- GTest(contingency_table)
print("Likelihood Ratio Test (G-test) Results:")
print(g_test)

# --- Partitioning Chi-Squared (Example: 18-30 vs. 31-45) ---
print("Partitioning G-test: 18-30 vs. 31-45")
partition_1 <- GTest(contingency_table[1:2, ])  # Comparing 18-30 and 31-45
print(partition_1)

print("Partitioning G-test: 46-60 vs. 60+")
partition_2 <- GTest(contingency_table[3:4, ])  # Comparing 46-60 and 60+
print(partition_2)

# --- Linear Trend Test (CMH) for Ordinal Age Group ---
print("Cochran-Mantel-Haenszel Test for Linear Trend:")
# Assign scores to age_group (ordinal): 18-30 = 1, 31-45 = 2, 46-60 = 3, 60+ = 4
cmh_test <- CMHtest(contingency_table, rscores = c(1, 2, 3, 4))
print(cmh_test)

# --- Measures of Association ---
# Odds Ratios
odds_result <- oddsratio(contingency_table, method = "wald", rev = "both")
print("Odds Ratios with 95% CI:")
print(odds_result$measure)

# Relative Risks
risk_result <- riskratio(contingency_table, method = "wald", rev = "both")
print("Relative Risks with 95% CI:")
print(risk_result$measure)

# Pearson's Contingency Coefficient
cont_coef <- ContCoef(contingency_table)
print("Pearson's Contingency Coefficient:")
print(cont_coef)

# Cramér's V
cramer_v <- CramerV(contingency_table)
print("Cramér's V:")
print(cramer_v)

# Phi Coefficient (only for 2x2 tables)
if (nrow(contingency_table) == 2 & ncol(contingency_table) == 2) {
  phi_coef <- Phi(contingency_table)
  print("Phi Coefficient:")
  print(phi_coef)
} else {
  print("Phi Coefficient not computed (table is not 2x2).")
}

# --- Three-Way Contingency Table (with gender as control) ---
print("Three-Way Contingency Table (Age Group vs PIN Change Due by Gender):")
three_way_table <- table(data$age_group, data$PIN_Change_Due, data$gender)
print(three_way_table)

# Conditional odds ratios for each gender
print("Conditional Odds Ratios by Gender:")
for (gender_level in levels(data$gender)) {
  cat("\nGender:", gender_level, "\n")
  sub_table <- three_way_table[, , gender_level]
  odds <- oddsratio(sub_table, method = "wald")
  print(odds$measure)
}

# --- Summary of Findings ---
cat("\nSummary of Findings:\n")
cat("Pearson's Chi-Square Test: X-squared =", chi_test$statistic, ", df =", chi_test$parameter, ", p-value =", chi_test$p.value, "\n")
cat("Likelihood Ratio Test: G-squared =", g_test$statistic, ", df =", g_test$parameter, ", p-value =", g_test$p.value, "\n")
cat("CMH Test for Linear Trend: M-squared =", cmh_test$table["cor", "Chisq"], ", df = 1, p-value =", cmh_test$table["cor", "Prob"], "\n")
if (chi_test$p.value < 0.05 | g_test$p.value < 0.05) {
  cat("There is evidence of a significant association between age group and PIN change due status (p < 0.05).\n")
} else {
  cat("There is no significant evidence of an association between age group and PIN change due status (p >= 0.05).\n")
}

# Interpretation of residuals
cat("\nInterpretation of Standardized Residuals:\n")
cat("Large positive residuals (> 2 or 3) indicate more observations than expected under independence.\n")
cat("Large negative residuals (< -2 or -3) indicate fewer observations than expected under independence.\n")