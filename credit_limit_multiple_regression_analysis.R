# Load required library
library(tidyverse)
library(GGally)
library(car)
library(lmtest)
library(nortest)

# Load data (replace with your file path if needed)
df_merged <- read_csv("data/merged_data.csv")


# Subset the data to the four variables of interest
df_subset <- df_merged %>% 
  select(total_debt,yearly_income, current_age,credit_score)

# correlation matrix 
with(df_subset,cor(cbind(total_debt,yearly_income, current_age,credit_score)))
# weak <0.5  0.8>moderate >.5 strong>0.8 1

# Creating scatterplot matrix
ggpairs(df_subset, 
        columnLabels = c("Total Debt", "Yearly Income", "Current Age","Credit Score"))

# Fitting the multiple regression model in R
lm <- lm(total_debt~yearly_income+current_age+credit_score,data=df_subset)
summary(lm)

# Finding the variance inflation factors (VIFs) in R:
vif(lm)

# Making all the necessary diagnostic plots in R

## Residuals vs Fitted values(y^)
ggplot(lm,aes(x=.fitted,y=.resid))+geom_point()+
geom_hline(yintercept=0,lty=2)+labs(x="Fitted Values",y="Residuals")+
  theme_classic()

## Residuals Vs Explanatory Variables

### Linearity between Residuals Vs yearly_income controlling for current_age and credit_score
ggplot(lm,aes(x=yearly_income,y=.resid))+geom_point()+
  geom_hline(yintercept=0,lty=2)+labs(x=expression(X[1]),y="Residuals")+
  theme_classic()+geom_smooth()

### Linearity between Residuals Vs current_age controlling for yearly_income and credit_score
ggplot(lm,aes(x=current_age,y=.resid))+geom_point()+
  geom_hline(yintercept=0,lty=2)+labs(x=expression(X[2]),y="Residuals")+
  theme_classic()+geom_smooth()

### Linearity between Residuals Vs credit_score controlling for yearly_income and current_age
ggplot(lm,aes(x=credit_score,y=.resid))+geom_point()+
  geom_hline(yintercept=0,lty=2)+labs(x=expression(X[1]),y="Residuals")+
  theme_classic()+geom_smooth()

## normality of residuals 
ggplot(lm,aes(sample=.resid))+stat_qq()+stat_qq_line(color="red")+
  theme_classic()

# Shapiro-Wilk test to assess the normality of the error terms by using the residuals.
ad.test(residuals(lm)) 


# Breusch-Pagan test to assess the equal spread of error terms.
bptest(lm,student=FALSE)


