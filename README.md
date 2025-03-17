# **Financial Metrics Analysis Project** 
## Overview
This project investigates the factors influencing key financial metrics such as total debt, yearly income, credit limits, debt-to-income (DTI) ratios, and PIN change due status. Using a variety of statistical models—including regression, ANCOVA, logistic regression, chi-square, and Generalized Estimating Equations (GEE)—the analysis uncovers patterns and relationships driven by variables like income, age, credit score, retirement status, and credit card type/brand. The findings provide actionable insights for financial planning and policy-making.

The project is based on a large dataset (2,000–6,146 entries) and aims to inform stakeholders about financial behavior trends, despite some statistical assumption violations (e.g., normality, homoscedasticity) that suggest cautious interpretation.

## Objectives
Assess how yearly income, age, and credit score affect total debt.<br />
Evaluate the impact of per capita income and retirement status on yearly income.<br />
Investigate the effects of income and card type/brand on credit limits.<br />
Determine how age and credit score influence the probability of high DTI (>0.8).<br />
Explore associations between age groups, card types, and PIN change due status.<br />

## Dataset
The dataset is sourced from two primary CSV files:<br />
 User Data: Contains demographic and financial information for 2,000 unique users.<br />
 Card Data: Includes details for 6,146 unique credit card entries.<br />
## Key Variables
## User Data
id: Unique identifier for each individual. <br />
current_age: Age in years.<br />
yearly_income: Annual income (numeric).<br />
total_debt: Total debt owed (numeric).<br />
credit_score: Individual’s credit score.<br />
num_credit_cards: Number of credit cards held.<br />
Debt_to_Income_Ratio: Calculated DTI for financial risk assessment.<br />
retirement_status: Retired or Not Retired (derived).<br />
age_group: Categorized as 17-30, 31-45, 46-60, 60+.<br />

## Card Data
id.y: Unique identifier for each card.<br />
card_brand: Brand (e.g., Visa, Mastercard, Amex).<br />
card_type: Type (e.g., Credit, Debit, Debit Prepaid).<br />
credit_limit: Credit limit (numeric).<br />
acct_open_date: Account opening date.<br />
PIN_Change_Due: Indicates if PIN hasn’t changed in the past 2 years (1=Yes, 0=No).<br />

## Data Preprocessing
Cleaning: Removed irrelevant columns (e.g., card_on_dark_web), converted currency fields to numeric, and reformatted dates.<br />
Feature Engineering: Derived retirement_status, age_group, Debt_to_Income_Ratio, and PIN_Change_Due. Merged user and card data using id and client_id.<br />

## Methodology
Regression: To assess continuous outcomes like total debt and credit limits.<br />
ANCOVA: For adjusted comparisons across groups.<br />
Logistic Regression: To evaluate probabilities (e.g., high DTI, PIN change due).<br />
Chi-Square: For categorical associations.<br />
GEE: To account for correlated data.<br />
