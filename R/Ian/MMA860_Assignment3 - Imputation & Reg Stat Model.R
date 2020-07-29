#MMA 860 Assignment 3

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(sqldf)
library(readxl)
library(mice)

##QUESTION 1

#import dataset
missing <- read_excel("Queen's MMA\\MMA 860\\Assignment 3\\MMA_860_Assignment_3_Data_v1_0.xlsx", 1)

#view summary of df; notice 13 missing observations across X1,...,X5
summary(missing)
md.pattern(missing)

#run original regression (for reference if necessary)
mod1 <- lm(Y ~ X1 + X2 + X3 + X4 + X5, missing)
summary(mod1)

#perform multiple imputation - set m = 13 since 13/100 observations are missing some value
imputed_data <- mice(missing, m=13, maxit=30, meth='pmm', seed=1)
summary(imputed_data)

#check a random set of the imputed data (to confirm if imputation worked)
completed_data <- complete(imputed_data, 1)

#run new regression using pooled results from imputation and evaluate
mod2 <- with(imputed_data, lm(Y ~ X1 + X2 + X3 + X4 + X5))
summary(mod2)
summary(pool(mod2))

#observations: In comparison with the original regression output, based off of an alpha significance level of 0.05, 
#we notice that X3 comes back as statistically significant with strong evidence, X1 & X2 & X4 are on the fringe of
#significance, and X5 comes back as statistically insignificant.

##QUESTION 2

#import and explore dataset
winedata <- read_excel("Queen's MMA\\MMA 860\\Assignment 3\\MMA_860_Assignment_3_Data_v1_0.xlsx", 2)

str(winedata)
summary(winedata)

#build regression

  #all variables
winemod <- lm(Rating ~ Price + Alcohol + Residual_Sugar + Sulphates + pH + Country, winedata)
summary(winemod)

  #remove residual sugar and pH (due to insignificant pvals)
winemod <- lm(Rating ~ Price + Alcohol + Residual_Sugar + Sulphates + Country, winedata)
summary(winemod)

#observations: Canada is the base case for country, t-tests for France/Italy/US are in reference to this. US being
#country of origin comes back as insignificant; all other variables are significant with strong statistical evidence

#test for heteroskedasticity - examine residuals vs fitted plot
plot(winemod)

#also test using breusch-pagan test
ncvTest(winemod)

#observations: No signs of an obvious pattern in the residuals vs fitted plot, conclude that we do not have an issue
#with heteroskedastic data here; fail to reject the breusch-pagan test (where null is that there exists no heteroskedasticity)

#predict wine rating given certain parameters
predict(winemod, data.frame(Price = 39.99, Alcohol = 13.9, Residual_Sugar = 1.96, Sulphates = 0.5, Country = "France"))