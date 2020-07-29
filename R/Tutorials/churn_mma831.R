#Based on: https://towardsdatascience.com/predict-customer-churn-with-r-9e62357d47b4
#Great site for sample data for real applications:
https://community.ibm.com/community/user/businessanalytics/blogs/steven-macko/2017/06/19/guide-to-ibm-cognos-analytics-sample-data-sets

#
library(plyr)
library(dplyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)

#R-packages...
#install.packages('corrplot')
#install.packages('randomForest')
#install.packages('party')
#install.packages('ggthemes')
#install.packages('caret')

#read data
churn <- read.csv("Queen's MMA\\MMA 831\\R Tutorials\\customer_churn.csv", header=TRUE, sep = ",")
str(churn)

#data description:
#gender (female, male)
#SeniorCitizen (Whether the customer is a senior citizen or not (1, 0))
#Partner (Whether the customer has a partner or not (Yes, No))
#Dependents (Whether the customer has dependents or not (Yes, No))
#tenure (Number of months the customer has stayed with the company)
#PhoneService (Whether the customer has a phone service or not (Yes, No))
#MultipleLines (Whether the customer has multiple lines r not (Yes, No, No phone service)
#InternetService (Customer’s internet service provider (DSL, Fiber optic, No)
#OnlineSecurity (Whether the customer has online security or not (Yes, No, No internet service)
#OnlineBackup (Whether the customer has online backup or not (Yes, No, No internet service)
#DeviceProtection (Whether the customer has device protection or not (Yes, No, No internet service)
#TechSupport (Whether the customer has tech support or not (Yes, No, No internet service)
#streamingTV (Whether the customer has streaming TV or not (Yes, No, No internet service)
#streamingMovies (Whether the customer has streaming movies or not (Yes, No, No internet service)
#Contract (The contract term of the customer (Month-to-month, One year, Two year)
#PaperlessBilling (Whether the customer has paperless billing or not (Yes, No))
#PaymentMethod (The customer’s payment method (Electronic check, Mailed check, Bank transfer (automatic), Credit card (automatic)))
#MonthlyCharges (The amount charged to the customer monthly — numeric)
#TotalCharges (The total amount charged to the customer — numeric)
#Churn ( Whether the customer churned or not (Yes or No))

#We use sapply to check the number if missing values in each columns.
#We found that there are 11 missing values in “TotalCharges” columns
sapply(churn, function(x) sum(is.na(x))) #removing nulls
churn <- churn[complete.cases(churn), ]

str(churn)
#install.packages('survival')
#install.packages('survminer')
library(survival)
library(survminer)
library(dplyr)

#The [TENURE] futime column holds the survival times. This is the response variable
# fustat, on the other hand, tells you if an individual patients’ survival time is censored.

#create binary flag for CENSORED
churn$censor_flag <- ifelse(churn$Churn=="No", 1, 0)
str(churn)

#drop original churn factor variable (or else you'll have leakage!)
churn = subset(churn, select = -c(Churn) )
str(churn)

#Now, you are prepared to create a survival object.
#That is basically a compiled version of the futime and fustat columns that can be interpreted by the survfit function

surv_object = Surv(time = churn$tenure, event = churn$censor_flag) #creating a pair with just tenure and censor_flag information

#Look at the object
#The  + behind survival times indicates censored data points, you lived at least X
surv_object 

#Kaplan Meier Estimate in R
#The next step is to fit the Kaplan-Meier curves.
#You can easily do that by passing the surv_object to the survfit function.
#we can also stratify by a particular group...lets caompare by Contract
fitKM <- survfit(surv_object ~ Contract, data = churn)
summary(fitKM)

#Much easier to see on a plot
ggsurvplot(fitKM, data = churn, pval = TRUE)
#observations: 2 year contract survives the longest versus the one year contract, and lastly the month to month contract
#definately statistically significant impact of contract on attrition

#let's try something else...this examines survival probability by the multiplelines feature
fit2 <- survfit(surv_object ~ MultipleLines, data = churn)
ggsurvplot(fit2, data = churn, pval = TRUE)

#Now what, is there a more systematic way to look at the different covariates? SHOULD CREATE A MODEL incorporating all features

#Cox proportional hazards models allow you to include covariates. 
#You can build Cox proportional hazards models using the coxph function
# and then we can visualize them using the ggforest. 
#These type of plot is called a forest plot. It shows so-called hazard ratios (HR) which are derived from the model for all covariates that we included in the formula in coxph
churn_complete<-churn[complete.cases(churn),]

#fitting the model with a few more features
fit.coxph = coxph(surv_object ~ gender + InternetService + Contract, 
                   data = churn_complete)

ggforest(fit.coxph, data = churn_complete)
#observations: people with fiber optic internet service are extremely unlikely to cancel; same with 2 year contract

#to predict new observartions:
churn_new_subset = churn_complete[1:4,]
head(churn_new_subset)

churn_score_risk = predict(fit.coxph, churn_new_subset, type="lp", se.fit=TRUE)
churn_score_time = predict(fit.coxph, churn_new_subset, type="risk", se.fit=TRUE)

#churn_score_time = predict(fit.coxph, churn_new_subset, type="expexted", se.fit=TRUE)
#The survival probability for a subject is equal to exp(-expected).
