#Purpose: Build classification model to predict which new applicants for bank pilot should be offered credit

#Cleansing/Manipulation Packages:
pacman::p_load("dplyr","tidyr","stringr","lubridate","janitor")
#Graphing Packages:
pacman::p_load("ggplot2","esquisse","gridExtra")
#ML Packages:
pacman::p_load("car","MLmetrics","estimatr","fpp","forecast","caret","ROCR","lift","randomForest","glmnet","MASS",
               "e1071","partykit","rpart","ROCR","lift","randomForest","xgboost","tensorflow", "keras")
#Other Packages:
pacman::p_load("sqldf","readxl","geosphere")

#import dataset (can save and use the CSV that's been saved on teams - "MMA867 A3 -- credit data.csv")
credit <- read.csv("Queen's MMA\\MMA 867\\Assignment 3\\MMA867 A3 -- credit data.csv", header=TRUE, sep = ",")

#----- PRE-PROCESSING -----

#explore data
summary(credit)
  #observation: notice no NA's (missing data)
str(credit)

#change necessary data types
  #we want to change these variables from integers to factor variables
credit$SEX <- as.factor(credit$SEX)
credit$EDUCATION <- as.factor(credit$EDUCATION)
credit$MARRIAGE <- as.factor(credit$MARRIAGE)
credit$default_0 <- as.factor(credit$default_0)

credit$PAY_1T <- as.factor(credit$PAY_1)
credit$PAY_2T <- as.factor(credit$PAY_2)
credit$PAY_3T <- as.factor(credit$PAY_3)
credit$PAY_4T <- as.factor(credit$PAY_4)
credit$PAY_5T <- as.factor(credit$PAY_5)
credit$PAY_6T <- as.factor(credit$PAY_6)

str(credit)

#---NOTE: There are a handful of '0' values for Marriage (42 records) and Education (11 records).
#         '0' is unaccounted for in the data dictionary for these variables.
#         After conferring with TA, We will leave them as is (to not potentially contaminate the other buckets).

#----- FEATURE ENGINEERING -----

#1 Pay_Amt/Age
credit$PAY_AMT1_DIV_AGE <- credit$PAY_AMT1/credit$AGE
credit$PAY_AMT2_DIV_AGE <- credit$PAY_AMT2/credit$AGE
credit$PAY_AMT3_DIV_AGE <- credit$PAY_AMT3/credit$AGE
credit$PAY_AMT4_DIV_AGE <- credit$PAY_AMT4/credit$AGE
credit$PAY_AMT5_DIV_AGE <- credit$PAY_AMT5/credit$AGE
credit$PAY_AMT6_DIV_AGE <- credit$PAY_AMT6/credit$AGE

#2 Pay_Amt/Education
#convert Education to numeric type, engineer new variable, convert Education back to factor
credit$EDUCATION <- as.numeric(credit$EDUCATION)

credit$PAY_AMT1_DIV_EDU <- credit$PAY_AMT1/credit$EDUCATION
credit$PAY_AMT2_DIV_EDU <- credit$PAY_AMT2/credit$EDUCATION
credit$PAY_AMT3_DIV_EDU <- credit$PAY_AMT3/credit$EDUCATION
credit$PAY_AMT4_DIV_EDU <- credit$PAY_AMT4/credit$EDUCATION
credit$PAY_AMT5_DIV_EDU <- credit$PAY_AMT5/credit$EDUCATION
credit$PAY_AMT6_DIV_EDU <- credit$PAY_AMT6/credit$EDUCATION

credit$EDUCATION <- credit$EDUCATION-1 #for whatever reason, converting to numeric above added +1
credit$EDUCATION <- as.factor(credit$EDUCATION)

#3 Bill_Amt/Age
#Creating 6 variables - 1 for each Bill_Amt
credit$BILL_AMT1_DIV_AGE <- credit$BILL_AMT1/credit$AGE
credit$BILL_AMT2_DIV_AGE <- credit$BILL_AMT2/credit$AGE
credit$BILL_AMT3_DIV_AGE <- credit$BILL_AMT3/credit$AGE
credit$BILL_AMT4_DIV_AGE <- credit$BILL_AMT4/credit$AGE
credit$BILL_AMT5_DIV_AGE <- credit$BILL_AMT5/credit$AGE
credit$BILL_AMT6_DIV_AGE <- credit$BILL_AMT6/credit$AGE

#4 Bill_Amt/Education
#Creating 6 variables - 1 for each Bill_Amt
#convert Education to numeric type, engineer new variable, convert Education back to factor
credit$EDUCATION <- as.numeric(credit$EDUCATION)

credit$BILL_AMT1_DIV_EDU <- credit$BILL_AMT1/credit$EDUCATION
credit$BILL_AMT2_DIV_EDU <- credit$BILL_AMT2/credit$EDUCATION
credit$BILL_AMT3_DIV_EDU <- credit$BILL_AMT3/credit$EDUCATION
credit$BILL_AMT4_DIV_EDU <- credit$BILL_AMT4/credit$EDUCATION
credit$BILL_AMT5_DIV_EDU <- credit$BILL_AMT5/credit$EDUCATION
credit$BILL_AMT6_DIV_EDU <- credit$BILL_AMT6/credit$EDUCATION

credit$EDUCATION <- credit$EDUCATION-1 #for whatever reason, converting to numeric above added +1
credit$EDUCATION <- as.factor(credit$EDUCATION)

#5 Bill_Amt - Pay_Amt
#Creating 5 variables - 1 for each pair of Bill-Pay period (Pay_Amt1 is payment for Bill_Amt2)
credit$PAY_DIFFERENCE_AMT1 <- credit$BILL_AMT2-credit$PAY_AMT1
credit$PAY_DIFFERENCE_AMT2 <- credit$BILL_AMT3-credit$PAY_AMT2
credit$PAY_DIFFERENCE_AMT3 <- credit$BILL_AMT4-credit$PAY_AMT3
credit$PAY_DIFFERENCE_AMT4 <- credit$BILL_AMT5-credit$PAY_AMT4
credit$PAY_DIFFERENCE_AMT5 <- credit$BILL_AMT6-credit$PAY_AMT5

#6 (Bill_Amt-Pay_Amt)/Age
#Creating 5 variables - 1 for each Bill-Pay pair
credit$PAY_DIFFERENCE_AMT1_DIV_AGE <- credit$PAY_DIFFERENCE_AMT1/credit$AGE
credit$PAY_DIFFERENCE_AMT2_DIV_AGE <- credit$PAY_DIFFERENCE_AMT2/credit$AGE
credit$PAY_DIFFERENCE_AMT3_DIV_AGE <- credit$PAY_DIFFERENCE_AMT3/credit$AGE
credit$PAY_DIFFERENCE_AMT4_DIV_AGE <- credit$PAY_DIFFERENCE_AMT4/credit$AGE
credit$PAY_DIFFERENCE_AMT5_DIV_AGE <- credit$PAY_DIFFERENCE_AMT5/credit$AGE

#7 Bill_Amt-Pay amt/Education
#Creating 5 variables - 1 for each Bill-pay pair
#convert Education to numeric type, engineer new variable, convert Education back to factor
credit$EDUCATION <- as.numeric(credit$EDUCATION)

credit$PAY_DIFFERENCE_AMT1_DIV_EDU <- credit$PAY_DIFFERENCE_AMT1/credit$EDUCATION
credit$PAY_DIFFERENCE_AMT2_DIV_EDU <- credit$PAY_DIFFERENCE_AMT2/credit$EDUCATION
credit$PAY_DIFFERENCE_AMT3_DIV_EDU <- credit$PAY_DIFFERENCE_AMT3/credit$EDUCATION
credit$PAY_DIFFERENCE_AMT4_DIV_EDU <- credit$PAY_DIFFERENCE_AMT4/credit$EDUCATION
credit$PAY_DIFFERENCE_AMT5_DIV_EDU <- credit$PAY_DIFFERENCE_AMT5/credit$EDUCATION

credit$EDUCATION <- credit$EDUCATION-1 #for whatever reason, converting to numeric above added +1
credit$EDUCATION <- as.factor(credit$EDUCATION)

#8 Credit limit/Age
#Creating 1 variable
credit$CREDIT_LIMIT_DIV_AGE <- credit$LIMIT_BAL/credit$AGE

#9 Credit Limit/Education
#Creating 1 variable
#convert Education to numeric type, engineer new variable, convert Education back to factor
credit$EDUCATION <- as.numeric(credit$EDUCATION)

credit$CREDIT_LIMIT_DIV_EDU <- credit$LIMIT_BAL/credit$EDUCATION

credit$EDUCATION <- credit$EDUCATION-1 #for whatever reason, converting to numeric above added +1
credit$EDUCATION <- as.factor(credit$EDUCATION)

#12 Total Payments Satisfied
#can check for only 5 bill-pay pairs so this number will range from 0 to 5
credit$TOTAL_PAYMENTS_SATISFIED <- 0

credit$TOTAL_PAYMENTS_SATISFIED <- if_else(credit$PAY_AMT1>=credit$BILL_AMT2,1+credit$TOTAL_PAYMENTS_SATISFIED, credit$TOTAL_PAYMENTS_SATISFIED)
credit$TOTAL_PAYMENTS_SATISFIED <- if_else(credit$PAY_AMT2>=credit$BILL_AMT3,1+credit$TOTAL_PAYMENTS_SATISFIED, credit$TOTAL_PAYMENTS_SATISFIED)
credit$TOTAL_PAYMENTS_SATISFIED <- if_else(credit$PAY_AMT3>=credit$BILL_AMT4,1+credit$TOTAL_PAYMENTS_SATISFIED, credit$TOTAL_PAYMENTS_SATISFIED)
credit$TOTAL_PAYMENTS_SATISFIED <- if_else(credit$PAY_AMT4>=credit$BILL_AMT5,1+credit$TOTAL_PAYMENTS_SATISFIED, credit$TOTAL_PAYMENTS_SATISFIED)
credit$TOTAL_PAYMENTS_SATISFIED <- if_else(credit$PAY_AMT5>=credit$BILL_AMT6,1+credit$TOTAL_PAYMENTS_SATISFIED, credit$TOTAL_PAYMENTS_SATISFIED)

#13 Total Payments Missed - need to run code for FE #12 to run this
#exact inverse of Total Payments Satisfied
credit$TOTAL_PAYMENTS_MISSED <- 5 - credit$TOTAL_PAYMENTS_SATISFIED
  #collinearity?

#16 Zero Bill
#Creating 6 variables, change to factors
credit$ZERO_BILL1 <- if_else(credit$BILL_AMT1==0,1,0)
credit$ZERO_BILL2 <- if_else(credit$BILL_AMT2==0,1,0)
credit$ZERO_BILL3 <- if_else(credit$BILL_AMT3==0,1,0)
credit$ZERO_BILL4 <- if_else(credit$BILL_AMT4==0,1,0)
credit$ZERO_BILL5 <- if_else(credit$BILL_AMT5==0,1,0)
credit$ZERO_BILL6 <- if_else(credit$BILL_AMT6==0,1,0)

#17 Zero Pay
#Creating 6 variables, change to factors
credit$ZERO_PAY1 <- if_else(credit$PAY_AMT1==0,1,0)
credit$ZERO_PAY2 <- if_else(credit$PAY_AMT2==0,1,0)
credit$ZERO_PAY3 <- if_else(credit$PAY_AMT3==0,1,0)
credit$ZERO_PAY4 <- if_else(credit$PAY_AMT4==0,1,0)
credit$ZERO_PAY5 <- if_else(credit$PAY_AMT5==0,1,0)
credit$ZERO_PAY6 <- if_else(credit$PAY_AMT6==0,1,0)

#14 Total Zero Bill Months - need FE #16 to run this
credit$TOTAL_ZERO_BILL_MONTHS <- credit$ZERO_BILL1+credit$ZERO_BILL2+credit$ZERO_BILL3+credit$ZERO_BILL4+credit$ZERO_BILL5+credit$ZERO_BILL6

credit$ZERO_BILL1 <- as.factor(credit$ZERO_BILL1)
credit$ZERO_BILL2 <- as.factor(credit$ZERO_BILL2)
credit$ZERO_BILL3 <- as.factor(credit$ZERO_BILL3)
credit$ZERO_BILL4 <- as.factor(credit$ZERO_BILL4)
credit$ZERO_BILL5 <- as.factor(credit$ZERO_BILL5)
credit$ZERO_BILL6 <- as.factor(credit$ZERO_BILL6)

#15 Total Zero Pay Months - need FE #17 to run this
#change FE #17 back to numeric, then return to factor
credit$TOTAL_ZERO_PAY_MONTHS <- credit$ZERO_PAY1+credit$ZERO_PAY2+credit$ZERO_PAY3+credit$ZERO_PAY4+credit$ZERO_PAY5+credit$ZERO_PAY6

credit$ZERO_PAY1 <- as.factor(credit$ZERO_PAY1)
credit$ZERO_PAY2 <- as.factor(credit$ZERO_PAY2)
credit$ZERO_PAY3 <- as.factor(credit$ZERO_PAY3)
credit$ZERO_PAY4 <- as.factor(credit$ZERO_PAY4)
credit$ZERO_PAY5 <- as.factor(credit$ZERO_PAY5)
credit$ZERO_PAY6 <- as.factor(credit$ZERO_PAY6)

#18 Bill Range
i <- 1
credit$BILL_RANGE <- 0

for (i in 1:nrow(credit)){
  credit$BILL_RANGE[i] <- max(credit$BILL_AMT1[i], credit$BILL_AMT2[i], credit$BILL_AMT3[i], credit$BILL_AMT4[i], credit$BILL_AMT5[i], credit$BILL_AMT6[i]) - min(credit$BILL_AMT1[i], credit$BILL_AMT2[i], credit$BILL_AMT3[i], credit$BILL_AMT4[i], credit$BILL_AMT5[i], credit$BILL_AMT6[i])
}

#19 Pay Range
i <- 1
credit$PAY_RANGE <- 0

for (i in 1:nrow(credit)){
  credit$PAY_RANGE[i] <- max(credit$PAY_AMT1[i], credit$PAY_AMT2[i], credit$PAY_AMT3[i], credit$PAY_AMT4[i], credit$PAY_AMT5[i], credit$PAY_AMT6[i]) - min(credit$PAY_AMT1[i], credit$PAY_AMT2[i], credit$PAY_AMT3[i], credit$PAY_AMT4[i], credit$PAY_AMT5[i], credit$PAY_AMT6[i])
}

#23 Cumulative % of Bill Paid
credit$CUMU_PERCENT_BILL_PAID1 <- if_else(credit$BILL_AMT1 == 0,1,credit$PAY_AMT1/credit$BILL_AMT1)
credit$CUMU_PERCENT_BILL_PAID2 <- if_else(credit$BILL_AMT2 == 0,1,credit$PAY_AMT2/credit$BILL_AMT2)
credit$CUMU_PERCENT_BILL_PAID3 <- if_else(credit$BILL_AMT3 == 0,1,credit$PAY_AMT3/credit$BILL_AMT3)
credit$CUMU_PERCENT_BILL_PAID4 <- if_else(credit$BILL_AMT4 == 0,1,credit$PAY_AMT4/credit$BILL_AMT4)
credit$CUMU_PERCENT_BILL_PAID5 <- if_else(credit$BILL_AMT5 == 0,1,credit$PAY_AMT5/credit$BILL_AMT5)
credit$CUMU_PERCENT_BILL_PAID6 <- if_else(credit$BILL_AMT6 == 0,1,credit$PAY_AMT6/credit$BILL_AMT6)

#24 Dummy: Bill Amount is 1 SD away
#creating 6 variables, 1 for each Bill Amt month, convert to factors
credit$BILL_AMT1_1_SD_AWAY <- 0
credit$BILL_AMT2_1_SD_AWAY <- 0
credit$BILL_AMT3_1_SD_AWAY <- 0
credit$BILL_AMT4_1_SD_AWAY <- 0
credit$BILL_AMT5_1_SD_AWAY <- 0
credit$BILL_AMT6_1_SD_AWAY <- 0

credit$BILL_AMT1_1_SD_AWAY <- if_else(credit$BILL_AMT1 >= mean(credit$BILL_AMT1)+sqrt(var(credit$BILL_AMT1)), 1, credit$BILL_AMT1_1_SD_AWAY)
credit$BILL_AMT1_1_SD_AWAY <- if_else(credit$BILL_AMT1 <= mean(credit$BILL_AMT1)-sqrt(var(credit$BILL_AMT1)), 1, credit$BILL_AMT1_1_SD_AWAY)

credit$BILL_AMT2_1_SD_AWAY <- if_else(credit$BILL_AMT2 >= mean(credit$BILL_AMT2)+sqrt(var(credit$BILL_AMT2)), 1, credit$BILL_AMT2_1_SD_AWAY)
credit$BILL_AMT2_1_SD_AWAY <- if_else(credit$BILL_AMT2 <= mean(credit$BILL_AMT2)-sqrt(var(credit$BILL_AMT2)), 1, credit$BILL_AMT2_1_SD_AWAY)

credit$BILL_AMT3_1_SD_AWAY <- if_else(credit$BILL_AMT3 >= mean(credit$BILL_AMT3)+sqrt(var(credit$BILL_AMT3)), 1, credit$BILL_AMT3_1_SD_AWAY)
credit$BILL_AMT3_1_SD_AWAY <- if_else(credit$BILL_AMT3 <= mean(credit$BILL_AMT3)-sqrt(var(credit$BILL_AMT3)), 1, credit$BILL_AMT3_1_SD_AWAY)

credit$BILL_AMT4_1_SD_AWAY <- if_else(credit$BILL_AMT4 >= mean(credit$BILL_AMT4)+sqrt(var(credit$BILL_AMT4)), 1, credit$BILL_AMT4_1_SD_AWAY)
credit$BILL_AMT4_1_SD_AWAY <- if_else(credit$BILL_AMT4 <= mean(credit$BILL_AMT4)-sqrt(var(credit$BILL_AMT4)), 1, credit$BILL_AMT4_1_SD_AWAY)

credit$BILL_AMT5_1_SD_AWAY <- if_else(credit$BILL_AMT5 >= mean(credit$BILL_AMT5)+sqrt(var(credit$BILL_AMT5)), 1, credit$BILL_AMT5_1_SD_AWAY)
credit$BILL_AMT5_1_SD_AWAY <- if_else(credit$BILL_AMT5 <= mean(credit$BILL_AMT5)-sqrt(var(credit$BILL_AMT5)), 1, credit$BILL_AMT5_1_SD_AWAY)

credit$BILL_AMT6_1_SD_AWAY <- if_else(credit$BILL_AMT6 >= mean(credit$BILL_AMT6)+sqrt(var(credit$BILL_AMT6)), 1, credit$BILL_AMT6_1_SD_AWAY)
credit$BILL_AMT6_1_SD_AWAY <- if_else(credit$BILL_AMT6 <= mean(credit$BILL_AMT6)-sqrt(var(credit$BILL_AMT6)), 1, credit$BILL_AMT6_1_SD_AWAY)

credit$BILL_AMT1_1_SD_AWAY <- as.factor(credit$BILL_AMT1_1_SD_AWAY)
credit$BILL_AMT2_1_SD_AWAY <- as.factor(credit$BILL_AMT2_1_SD_AWAY)
credit$BILL_AMT3_1_SD_AWAY <- as.factor(credit$BILL_AMT3_1_SD_AWAY)
credit$BILL_AMT4_1_SD_AWAY <- as.factor(credit$BILL_AMT4_1_SD_AWAY)
credit$BILL_AMT5_1_SD_AWAY <- as.factor(credit$BILL_AMT5_1_SD_AWAY)
credit$BILL_AMT6_1_SD_AWAY <- as.factor(credit$BILL_AMT6_1_SD_AWAY)

#25 Dummy: Paid Amount is 1 SD away
#creating 6 variables, 1 for each Pay Amt month, convert to factors
credit$PAY_AMT1_1_SD_AWAY <- 0
credit$PAY_AMT2_1_SD_AWAY <- 0
credit$PAY_AMT3_1_SD_AWAY <- 0
credit$PAY_AMT4_1_SD_AWAY <- 0
credit$PAY_AMT5_1_SD_AWAY <- 0
credit$PAY_AMT6_1_SD_AWAY <- 0

credit$PAY_AMT1_1_SD_AWAY <- if_else(credit$PAY_AMT1 >= mean(credit$PAY_AMT1)+sqrt(var(credit$PAY_AMT1)), 1, credit$PAY_AMT1_1_SD_AWAY)
credit$PAY_AMT1_1_SD_AWAY <- if_else(credit$PAY_AMT1 <= mean(credit$PAY_AMT1)-sqrt(var(credit$PAY_AMT1)), 1, credit$PAY_AMT1_1_SD_AWAY)

credit$PAY_AMT2_1_SD_AWAY <- if_else(credit$PAY_AMT2 >= mean(credit$PAY_AMT2)+sqrt(var(credit$PAY_AMT2)), 1, credit$PAY_AMT2_1_SD_AWAY)
credit$PAY_AMT2_1_SD_AWAY <- if_else(credit$PAY_AMT2 <= mean(credit$PAY_AMT2)-sqrt(var(credit$PAY_AMT2)), 1, credit$PAY_AMT2_1_SD_AWAY)

credit$PAY_AMT3_1_SD_AWAY <- if_else(credit$PAY_AMT3 >= mean(credit$PAY_AMT3)+sqrt(var(credit$PAY_AMT3)), 1, credit$PAY_AMT3_1_SD_AWAY)
credit$PAY_AMT3_1_SD_AWAY <- if_else(credit$PAY_AMT3 <= mean(credit$PAY_AMT3)-sqrt(var(credit$PAY_AMT3)), 1, credit$PAY_AMT3_1_SD_AWAY)

credit$PAY_AMT4_1_SD_AWAY <- if_else(credit$PAY_AMT4 >= mean(credit$PAY_AMT4)+sqrt(var(credit$PAY_AMT4)), 1, credit$PAY_AMT4_1_SD_AWAY)
credit$PAY_AMT4_1_SD_AWAY <- if_else(credit$PAY_AMT4 <= mean(credit$PAY_AMT4)-sqrt(var(credit$PAY_AMT4)), 1, credit$PAY_AMT4_1_SD_AWAY)

credit$PAY_AMT5_1_SD_AWAY <- if_else(credit$PAY_AMT5 >= mean(credit$PAY_AMT5)+sqrt(var(credit$PAY_AMT5)), 1, credit$PAY_AMT5_1_SD_AWAY)
credit$PAY_AMT5_1_SD_AWAY <- if_else(credit$PAY_AMT5 <= mean(credit$PAY_AMT5)-sqrt(var(credit$PAY_AMT5)), 1, credit$PAY_AMT5_1_SD_AWAY)

credit$PAY_AMT6_1_SD_AWAY <- if_else(credit$PAY_AMT6 >= mean(credit$PAY_AMT6)+sqrt(var(credit$PAY_AMT6)), 1, credit$PAY_AMT6_1_SD_AWAY)
credit$PAY_AMT6_1_SD_AWAY <- if_else(credit$PAY_AMT6 <= mean(credit$PAY_AMT6)-sqrt(var(credit$PAY_AMT6)), 1, credit$PAY_AMT6_1_SD_AWAY)

credit$PAY_AMT1_1_SD_AWAY <- as.factor(credit$PAY_AMT1_1_SD_AWAY)
credit$PAY_AMT2_1_SD_AWAY <- as.factor(credit$PAY_AMT2_1_SD_AWAY)
credit$PAY_AMT3_1_SD_AWAY <- as.factor(credit$PAY_AMT3_1_SD_AWAY)
credit$PAY_AMT4_1_SD_AWAY <- as.factor(credit$PAY_AMT4_1_SD_AWAY)
credit$PAY_AMT5_1_SD_AWAY <- as.factor(credit$PAY_AMT5_1_SD_AWAY)
credit$PAY_AMT6_1_SD_AWAY <- as.factor(credit$PAY_AMT6_1_SD_AWAY)

#26 Dummy: Bill amt - Paid amt 1 SD away - you need FE variable #5 to use code for this variable
#creating 5 variables, 1 for each Bill-Pay pair, convert to factors
credit$PAY_DIFFERENCE_AMT1_1_SD_AWAY <- 0
credit$PAY_DIFFERENCE_AMT2_1_SD_AWAY <- 0
credit$PAY_DIFFERENCE_AMT3_1_SD_AWAY <- 0
credit$PAY_DIFFERENCE_AMT4_1_SD_AWAY <- 0
credit$PAY_DIFFERENCE_AMT5_1_SD_AWAY <- 0

credit$PAY_DIFFERENCE_AMT1_1_SD_AWAY <- if_else(credit$PAY_DIFFERENCE_AMT1 >= mean(credit$PAY_DIFFERENCE_AMT1)+sqrt(var(credit$PAY_DIFFERENCE_AMT1)), 1, credit$PAY_DIFFERENCE_AMT1_1_SD_AWAY)
credit$PAY_DIFFERENCE_AMT1_1_SD_AWAY <- if_else(credit$PAY_DIFFERENCE_AMT1 <= mean(credit$PAY_DIFFERENCE_AMT1)-sqrt(var(credit$PAY_DIFFERENCE_AMT1)), 1, credit$PAY_DIFFERENCE_AMT1_1_SD_AWAY)

credit$PAY_DIFFERENCE_AMT2_1_SD_AWAY <- if_else(credit$PAY_DIFFERENCE_AMT2 >= mean(credit$PAY_DIFFERENCE_AMT2)+sqrt(var(credit$PAY_DIFFERENCE_AMT2)), 1, credit$PAY_DIFFERENCE_AMT2_1_SD_AWAY)
credit$PAY_DIFFERENCE_AMT2_1_SD_AWAY <- if_else(credit$PAY_DIFFERENCE_AMT2 <= mean(credit$PAY_DIFFERENCE_AMT2)-sqrt(var(credit$PAY_DIFFERENCE_AMT2)), 1, credit$PAY_DIFFERENCE_AMT2_1_SD_AWAY)

credit$PAY_DIFFERENCE_AMT3_1_SD_AWAY <- if_else(credit$PAY_DIFFERENCE_AMT3 >= mean(credit$PAY_DIFFERENCE_AMT3)+sqrt(var(credit$PAY_DIFFERENCE_AMT3)), 1, credit$PAY_DIFFERENCE_AMT3_1_SD_AWAY)
credit$PAY_DIFFERENCE_AMT3_1_SD_AWAY <- if_else(credit$PAY_DIFFERENCE_AMT3 <= mean(credit$PAY_DIFFERENCE_AMT3)-sqrt(var(credit$PAY_DIFFERENCE_AMT3)), 1, credit$PAY_DIFFERENCE_AMT3_1_SD_AWAY)

credit$PAY_DIFFERENCE_AMT4_1_SD_AWAY <- if_else(credit$PAY_DIFFERENCE_AMT4 >= mean(credit$PAY_DIFFERENCE_AMT4)+sqrt(var(credit$PAY_DIFFERENCE_AMT4)), 1, credit$PAY_DIFFERENCE_AMT4_1_SD_AWAY)
credit$PAY_DIFFERENCE_AMT4_1_SD_AWAY <- if_else(credit$PAY_DIFFERENCE_AMT4 <= mean(credit$PAY_DIFFERENCE_AMT4)-sqrt(var(credit$PAY_DIFFERENCE_AMT4)), 1, credit$PAY_DIFFERENCE_AMT4_1_SD_AWAY)

credit$PAY_DIFFERENCE_AMT5_1_SD_AWAY <- if_else(credit$PAY_DIFFERENCE_AMT5 >= mean(credit$PAY_DIFFERENCE_AMT5)+sqrt(var(credit$PAY_DIFFERENCE_AMT5)), 1, credit$PAY_DIFFERENCE_AMT5_1_SD_AWAY)
credit$PAY_DIFFERENCE_AMT5_1_SD_AWAY <- if_else(credit$PAY_DIFFERENCE_AMT5 <= mean(credit$PAY_DIFFERENCE_AMT5)-sqrt(var(credit$PAY_DIFFERENCE_AMT5)), 1, credit$PAY_DIFFERENCE_AMT5_1_SD_AWAY)

credit$PAY_DIFFERENCE_AMT1_1_SD_AWAY <- as.factor(credit$PAY_DIFFERENCE_AMT1_1_SD_AWAY)
credit$PAY_DIFFERENCE_AMT2_1_SD_AWAY <- as.factor(credit$PAY_DIFFERENCE_AMT2_1_SD_AWAY)
credit$PAY_DIFFERENCE_AMT3_1_SD_AWAY <- as.factor(credit$PAY_DIFFERENCE_AMT3_1_SD_AWAY)
credit$PAY_DIFFERENCE_AMT4_1_SD_AWAY <- as.factor(credit$PAY_DIFFERENCE_AMT4_1_SD_AWAY)
credit$PAY_DIFFERENCE_AMT5_1_SD_AWAY <- as.factor(credit$PAY_DIFFERENCE_AMT5_1_SD_AWAY)

#27 Age Buckets
#convert to factor
credit$AGE_BUCKET <- "a"

credit$AGE_BUCKET <- if_else(credit$AGE < 18,"< 18 yrs",credit$AGE_BUCKET)
credit$AGE_BUCKET <- if_else(credit$AGE >= 18 & credit$AGE < 25,"18-24 yrs",credit$AGE_BUCKET)
credit$AGE_BUCKET <- if_else(credit$AGE >= 25 & credit$AGE < 35,"25-34 yrs",credit$AGE_BUCKET)
credit$AGE_BUCKET <- if_else(credit$AGE >= 35 & credit$AGE < 45,"35-44 yrs",credit$AGE_BUCKET)
credit$AGE_BUCKET <- if_else(credit$AGE >= 45 & credit$AGE < 55,"45-54 yrs",credit$AGE_BUCKET)
credit$AGE_BUCKET <- if_else(credit$AGE >= 55,"> 55 yrs",credit$AGE_BUCKET)

credit$AGE_BUCKET <- as.factor((credit$AGE_BUCKET))

#28 Limit_Bal Buckets i.e. Credit Limit Buckets
#convert to factor
credit$CREDIT_LIMIT_BUCKET <- "a"

credit$CREDIT_LIMIT_BUCKET <- if_else(credit$LIMIT_BAL < 25000,"< 25K",credit$CREDIT_LIMIT_BUCKET)
credit$CREDIT_LIMIT_BUCKET <- if_else(credit$LIMIT_BAL >= 25000 & credit$LIMIT_BAL < 50000,"25-50K",credit$CREDIT_LIMIT_BUCKET)
credit$CREDIT_LIMIT_BUCKET <- if_else(credit$LIMIT_BAL >= 50000 & credit$LIMIT_BAL < 75000,"50-75K",credit$CREDIT_LIMIT_BUCKET)
credit$CREDIT_LIMIT_BUCKET <- if_else(credit$LIMIT_BAL >= 75000 & credit$LIMIT_BAL < 100000,"75-100K",credit$CREDIT_LIMIT_BUCKET)
credit$CREDIT_LIMIT_BUCKET <- if_else(credit$LIMIT_BAL >= 100000 & credit$LIMIT_BAL < 125000,"100-125K",credit$CREDIT_LIMIT_BUCKET)
credit$CREDIT_LIMIT_BUCKET <- if_else(credit$LIMIT_BAL >= 125000 & credit$LIMIT_BAL < 150000,"125-150K",credit$CREDIT_LIMIT_BUCKET)
credit$CREDIT_LIMIT_BUCKET <- if_else(credit$LIMIT_BAL >= 150000 & credit$LIMIT_BAL < 175000,"150-175K",credit$CREDIT_LIMIT_BUCKET)
credit$CREDIT_LIMIT_BUCKET <- if_else(credit$LIMIT_BAL >= 175000 & credit$LIMIT_BAL < 200000,"175-200K",credit$CREDIT_LIMIT_BUCKET)
credit$CREDIT_LIMIT_BUCKET <- if_else(credit$LIMIT_BAL >= 200000 & credit$LIMIT_BAL < 225000,"200-225K",credit$CREDIT_LIMIT_BUCKET)
credit$CREDIT_LIMIT_BUCKET <- if_else(credit$LIMIT_BAL >= 225000 & credit$LIMIT_BAL < 250000,"225-250K",credit$CREDIT_LIMIT_BUCKET)
credit$CREDIT_LIMIT_BUCKET <- if_else(credit$LIMIT_BAL >= 250000 & credit$LIMIT_BAL < 300000,"250-300K",credit$CREDIT_LIMIT_BUCKET)
credit$CREDIT_LIMIT_BUCKET <- if_else(credit$LIMIT_BAL >= 300000 & credit$LIMIT_BAL < 400000,"300-400K",credit$CREDIT_LIMIT_BUCKET)
credit$CREDIT_LIMIT_BUCKET <- if_else(credit$LIMIT_BAL >= 400000 & credit$LIMIT_BAL < 500000,"400-500K",credit$CREDIT_LIMIT_BUCKET)
credit$CREDIT_LIMIT_BUCKET <- if_else(credit$LIMIT_BAL >= 500000 & credit$LIMIT_BAL < 700000,"500-700K",credit$CREDIT_LIMIT_BUCKET)
credit$CREDIT_LIMIT_BUCKET <- if_else(credit$LIMIT_BAL >= 700000 & credit$LIMIT_BAL < 900000,"700-900K",credit$CREDIT_LIMIT_BUCKET)
credit$CREDIT_LIMIT_BUCKET <- if_else(credit$LIMIT_BAL >= 900000 & credit$LIMIT_BAL < 1000000,"900K-1Mn",credit$CREDIT_LIMIT_BUCKET)
credit$CREDIT_LIMIT_BUCKET <- if_else(credit$LIMIT_BAL >= 1000000,"> 1Mn",credit$CREDIT_LIMIT_BUCKET)

credit$CREDIT_LIMIT_BUCKET <- as.factor(credit$CREDIT_LIMIT_BUCKET)

#29a Frequency of Late Payments - capturing count of payments that are 3 or more months late
credit$LATE_PAYMENT_FREQUENCY3 <- 0

credit$LATE_PAYMENT_FREQUENCY3 <- if_else(credit$PAY_1 >= 3,credit$LATE_PAYMENT_FREQUENCY3+1,credit$LATE_PAYMENT_FREQUENCY3)
credit$LATE_PAYMENT_FREQUENCY3 <- if_else(credit$PAY_2 >= 3,credit$LATE_PAYMENT_FREQUENCY3+1,credit$LATE_PAYMENT_FREQUENCY3)
credit$LATE_PAYMENT_FREQUENCY3 <- if_else(credit$PAY_3 >= 3,credit$LATE_PAYMENT_FREQUENCY3+1,credit$LATE_PAYMENT_FREQUENCY3)
credit$LATE_PAYMENT_FREQUENCY3 <- if_else(credit$PAY_4 >= 3,credit$LATE_PAYMENT_FREQUENCY3+1,credit$LATE_PAYMENT_FREQUENCY3)
credit$LATE_PAYMENT_FREQUENCY3 <- if_else(credit$PAY_5 >= 3,credit$LATE_PAYMENT_FREQUENCY3+1,credit$LATE_PAYMENT_FREQUENCY3)
credit$LATE_PAYMENT_FREQUENCY3 <- if_else(credit$PAY_6 >= 3,credit$LATE_PAYMENT_FREQUENCY3+1,credit$LATE_PAYMENT_FREQUENCY3)

#29b Frequency of Late Payments - capturing count of payments that are 5 or more months late
credit$LATE_PAYMENT_FREQUENCY5 <- 0

credit$LATE_PAYMENT_FREQUENCY5 <- if_else(credit$PAY_1 >= 5,credit$LATE_PAYMENT_FREQUENCY5+1,credit$LATE_PAYMENT_FREQUENCY5)
credit$LATE_PAYMENT_FREQUENCY5 <- if_else(credit$PAY_2 >= 5,credit$LATE_PAYMENT_FREQUENCY5+1,credit$LATE_PAYMENT_FREQUENCY5)
credit$LATE_PAYMENT_FREQUENCY5 <- if_else(credit$PAY_3 >= 5,credit$LATE_PAYMENT_FREQUENCY5+1,credit$LATE_PAYMENT_FREQUENCY5)
credit$LATE_PAYMENT_FREQUENCY5 <- if_else(credit$PAY_4 >= 5,credit$LATE_PAYMENT_FREQUENCY5+1,credit$LATE_PAYMENT_FREQUENCY5)
credit$LATE_PAYMENT_FREQUENCY5 <- if_else(credit$PAY_5 >= 5,credit$LATE_PAYMENT_FREQUENCY5+1,credit$LATE_PAYMENT_FREQUENCY5)
credit$LATE_PAYMENT_FREQUENCY5 <- if_else(credit$PAY_6 >= 5,credit$LATE_PAYMENT_FREQUENCY5+1,credit$LATE_PAYMENT_FREQUENCY5)

#29c Frequency of Late Payments - capturing count of payments that are 7 or more months late
credit$LATE_PAYMENT_FREQUENCY7 <- 0

credit$LATE_PAYMENT_FREQUENCY7 <- if_else(credit$PAY_1 >= 7,credit$LATE_PAYMENT_FREQUENCY7+1,credit$LATE_PAYMENT_FREQUENCY7)
credit$LATE_PAYMENT_FREQUENCY7 <- if_else(credit$PAY_2 >= 7,credit$LATE_PAYMENT_FREQUENCY7+1,credit$LATE_PAYMENT_FREQUENCY7)
credit$LATE_PAYMENT_FREQUENCY7 <- if_else(credit$PAY_3 >= 7,credit$LATE_PAYMENT_FREQUENCY7+1,credit$LATE_PAYMENT_FREQUENCY7)
credit$LATE_PAYMENT_FREQUENCY7 <- if_else(credit$PAY_4 >= 7,credit$LATE_PAYMENT_FREQUENCY7+1,credit$LATE_PAYMENT_FREQUENCY7)
credit$LATE_PAYMENT_FREQUENCY7 <- if_else(credit$PAY_5 >= 7,credit$LATE_PAYMENT_FREQUENCY7+1,credit$LATE_PAYMENT_FREQUENCY7)
credit$LATE_PAYMENT_FREQUENCY7 <- if_else(credit$PAY_6 >= 7,credit$LATE_PAYMENT_FREQUENCY7+1,credit$LATE_PAYMENT_FREQUENCY7)

#29d Frequency of Late Payments - capturing count of payments that are 1 or more months late
credit$LATE_PAYMENT_FREQUENCY1 <- 0

credit$LATE_PAYMENT_FREQUENCY1 <- if_else(credit$PAY_1 >= 1,credit$LATE_PAYMENT_FREQUENCY1+1,credit$LATE_PAYMENT_FREQUENCY1)
credit$LATE_PAYMENT_FREQUENCY1 <- if_else(credit$PAY_2 >= 1,credit$LATE_PAYMENT_FREQUENCY1+1,credit$LATE_PAYMENT_FREQUENCY1)
credit$LATE_PAYMENT_FREQUENCY1 <- if_else(credit$PAY_3 >= 1,credit$LATE_PAYMENT_FREQUENCY1+1,credit$LATE_PAYMENT_FREQUENCY1)
credit$LATE_PAYMENT_FREQUENCY1 <- if_else(credit$PAY_4 >= 1,credit$LATE_PAYMENT_FREQUENCY1+1,credit$LATE_PAYMENT_FREQUENCY1)
credit$LATE_PAYMENT_FREQUENCY1 <- if_else(credit$PAY_5 >= 1,credit$LATE_PAYMENT_FREQUENCY1+1,credit$LATE_PAYMENT_FREQUENCY1)
credit$LATE_PAYMENT_FREQUENCY1 <- if_else(credit$PAY_6 >= 1,credit$LATE_PAYMENT_FREQUENCY1+1,credit$LATE_PAYMENT_FREQUENCY1)

#30 Average Months Delayed - run #29d (late payment frequency of 1 or more months) before running this code
#convert PAY_x variables to numeric, create FE variable, convert PAY_x back to numeric
credit$PAY_1 <- as.numeric(credit$PAY_1)
credit$PAY_2 <- as.numeric(credit$PAY_2)
credit$PAY_3 <- as.numeric(credit$PAY_3)
credit$PAY_4 <- as.numeric(credit$PAY_4)
credit$PAY_5 <- as.numeric(credit$PAY_5)
credit$PAY_6 <- as.numeric(credit$PAY_6)

i=1
credit$AVERAGE_MONTHS_DELAYED <- 0

for (i in 1:nrow(credit)) {
  credit$AVERAGE_MONTHS_DELAYED[i] <- if_else(credit$PAY_1[i] >= 1,credit$PAY_1[i],credit$AVERAGE_MONTHS_DELAYED[i])
  credit$AVERAGE_MONTHS_DELAYED[i] <- if_else(credit$PAY_2[i] >= 1,credit$PAY_2[i]+credit$AVERAGE_MONTHS_DELAYED[i],credit$AVERAGE_MONTHS_DELAYED[i])
  credit$AVERAGE_MONTHS_DELAYED[i] <- if_else(credit$PAY_3[i] >= 1,credit$PAY_3[i]+credit$AVERAGE_MONTHS_DELAYED[i],credit$AVERAGE_MONTHS_DELAYED[i])
  credit$AVERAGE_MONTHS_DELAYED[i] <- if_else(credit$PAY_4[i] >= 1,credit$PAY_4[i]+credit$AVERAGE_MONTHS_DELAYED[i],credit$AVERAGE_MONTHS_DELAYED[i])
  credit$AVERAGE_MONTHS_DELAYED[i] <- if_else(credit$PAY_5[i] >= 1,credit$PAY_5[i]+credit$AVERAGE_MONTHS_DELAYED[i],credit$AVERAGE_MONTHS_DELAYED[i])
  credit$AVERAGE_MONTHS_DELAYED[i] <- if_else(credit$PAY_6[i] >= 1,credit$PAY_6[i]+credit$AVERAGE_MONTHS_DELAYED[i],credit$AVERAGE_MONTHS_DELAYED[i])
  
  credit$AVERAGE_MONTHS_DELAYED[i] <- if_else(credit$LATE_PAYMENT_FREQUENCY1[i] > 0, credit$AVERAGE_MONTHS_DELAYED[i]/credit$LATE_PAYMENT_FREQUENCY1[i], 0)
}

#31 Dummy: Average months delayed 1 SD away - you need FE variable #30 to use code for this variable
#creating 5 variables, 1 for each Bill-Pay pair, convert to factor
credit$AVERAGE_MONTHS_DELAYED_1_SD_AWAY <- 0

credit$AVERAGE_MONTHS_DELAYED_1_SD_AWAY <- if_else(credit$AVERAGE_MONTHS_DELAYED >= mean(credit$AVERAGE_MONTHS_DELAYED)+sqrt(var(credit$AVERAGE_MONTHS_DELAYED)), 1, credit$AVERAGE_MONTHS_DELAYED_1_SD_AWAY)
credit$AVERAGE_MONTHS_DELAYED_1_SD_AWAY <- if_else(credit$AVERAGE_MONTHS_DELAYED <= mean(credit$AVERAGE_MONTHS_DELAYED)-sqrt(var(credit$AVERAGE_MONTHS_DELAYED)), 1, credit$AVERAGE_MONTHS_DELAYED_1_SD_AWAY)

credit$AVERAGE_MONTHS_DELAYED_1_SD_AWAY <- as.factor(credit$AVERAGE_MONTHS_DELAYED_1_SD_AWAY)

#32 Month on month percentage change in Bill Amount
#creating 5 new variables
#BILL_PERCENTAGE_CHANGE1 = (BILL_AMT1-BILL_AMT2)/BILL_AMT2
#same logic applies for months 2-3, 3-4, 4-5, 5-6 pairs
#if BILL_AMT2 = 0 but BILL_AMT1 > 0, then change will be captured as 100%
credit$BILL_PERCENTAGE_CHANGE1 <- if_else(credit$BILL_AMT2 != 0, (credit$BILL_AMT1-credit$BILL_AMT2)/credit$BILL_AMT2, 1)
credit$BILL_PERCENTAGE_CHANGE2 <- if_else(credit$BILL_AMT3 != 0, (credit$BILL_AMT2-credit$BILL_AMT3)/credit$BILL_AMT3, 1)
credit$BILL_PERCENTAGE_CHANGE3 <- if_else(credit$BILL_AMT4 != 0, (credit$BILL_AMT3-credit$BILL_AMT4)/credit$BILL_AMT4, 1)
credit$BILL_PERCENTAGE_CHANGE4 <- if_else(credit$BILL_AMT5 != 0, (credit$BILL_AMT4-credit$BILL_AMT5)/credit$BILL_AMT5, 1)
credit$BILL_PERCENTAGE_CHANGE5 <- if_else(credit$BILL_AMT6 != 0, (credit$BILL_AMT5-credit$BILL_AMT6)/credit$BILL_AMT6, 1)

#33 Average Bill Amount as a % of Credit Limit (LIMIT_BAL field in table)
credit$BILL_AMT1_PERCENTAGE <- credit$BILL_AMT1/credit$LIMIT_BAL
credit$BILL_AMT2_PERCENTAGE <- credit$BILL_AMT2/credit$LIMIT_BAL
credit$BILL_AMT3_PERCENTAGE <- credit$BILL_AMT3/credit$LIMIT_BAL
credit$BILL_AMT4_PERCENTAGE <- credit$BILL_AMT4/credit$LIMIT_BAL
credit$BILL_AMT5_PERCENTAGE <- credit$BILL_AMT5/credit$LIMIT_BAL
credit$BILL_AMT6_PERCENTAGE <- credit$BILL_AMT6/credit$LIMIT_BAL

#34 Average Unpaid Amount as a % of Credit Limit (Avg unpaid amount is FE #5 - run that code first to be able to use this code)
credit$PAY_DIFFERENCE_AMT1_PERCENTAGE <- credit$PAY_DIFFERENCE_AMT1/credit$LIMIT_BAL
credit$PAY_DIFFERENCE_AMT2_PERCENTAGE <- credit$PAY_DIFFERENCE_AMT2/credit$LIMIT_BAL
credit$PAY_DIFFERENCE_AMT3_PERCENTAGE <- credit$PAY_DIFFERENCE_AMT3/credit$LIMIT_BAL
credit$PAY_DIFFERENCE_AMT4_PERCENTAGE <- credit$PAY_DIFFERENCE_AMT4/credit$LIMIT_BAL
credit$PAY_DIFFERENCE_AMT5_PERCENTAGE <- credit$PAY_DIFFERENCE_AMT5/credit$LIMIT_BAL

#export new df with all features added in (may want to run through PyCaret as an experiment)
write.csv(credit, file = paste0("Queen's MMA\\MMA 867\\Assignment 3\\credit_data_features.csv"), row.names = FALSE, na = "")

#remove ID column
credit <- credit %>%
  dplyr::select(-ID)

#----- TEST LOGIT MODEL -----

#create random partition
inTrain <- createDataPartition(y = credit$default_0, #we want our random partition to be stratified - we want the proportion of retainment to be close to the same between the train and test set
                               p = 19199/24000, list = FALSE) #p splits the observations based on ~80/20 ratio
#inTrain vector tells us which rows will go into training (others will go into testing)
training <- credit[ inTrain,]
testing <- credit[ -inTrain,]

#define logit model (include all vars at first)
model_logistic <- glm(default_0 ~ ., data=training, family="binomial"(link="logit"))

summary(model_logistic)
  #observations: AIC = 16643

#attempt stepAIC (note how AIC decreases each step iteration)
model_logistic_stepwiseAIC<-stepAIC(model_logistic,direction = c("both"),trace = 1) #AIC stepwise, trace = 1 will display what the model is doing

summary(model_logistic_stepwiseAIC)
  #observations: AIC = 16619

#explore general model plots
par(mfrow=c(1,4))
plot(model_logistic_stepwiseAIC) #Error plots: similar nature to lm plots
par(mfrow=c(1,1))

#finding predicitons: probabilities and classification
logistic_probabilities<-predict(model_logistic_stepwiseAIC,newdata=testing,type="response") #Predict probabilities

#create vector of 1's that matches test set length
logistic_classification<-rep("1",4800)

#predict classification using 0.22104 threshold. Why 0.22104 - that's the average probability of defaulting
logistic_classification[logistic_probabilities<0.22104]="0"
logistic_classification<-as.factor(logistic_classification)

#confusion matrix  
confusionMatrix(logistic_classification, testing$default_0, positive = "1") #Display confusion matrix
  #observations: Accuracy = 76.17%

#ROC Curve
logistic_ROC_prediction <- prediction(logistic_probabilities, testing$default_0)
logistic_ROC <- performance(logistic_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(logistic_ROC) #Plot ROC curve

#AUC (area under curve)
auc.tmp <- performance(logistic_ROC_prediction,"auc") #Create AUC data
logistic_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
logistic_auc_testing #Display AUC value
  #observations: AUC = 0.7574

#lift chart
plotLift(logistic_probabilities, testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

#----- TEST RANDOM FOREST MODEL -----

#define random forest model
model_forest <- randomForest(default_0~ ., data=training, 
                             type="classification",
                             importance=TRUE,
                             ntree = 200,         # hyperparameter: number of trees in the forest
                             mtry = 10,           # hyperparameter: number of random columns to grow each tree
                             nodesize = 1,       # hyperparameter: min number of datapoints on the leaf of each tree
                             maxnodes = 400,       # hyperparameter: maximum number of leafs of a tree
                             cutoff = c(0.5, 0.5) # hyperparameter: how the voting works; (0.5, 0.5) means majority vote
) 

plot(model_forest) # plots error as a function of number of trees in the forest; use print(model_forest) to print the values on the plot
print(model_forest)

#check variable importance
varImpPlot(model_forest) # plots variable importances; use importance(model_forest) to print the values
importance(model_forest)

#finding predicitons: probabilities and classification
forest_probabilities<-predict(model_forest,newdata=testing,type="prob") #Predict probabilities -- an array with 2 columns: for not retained (class 0) and for retained (class 1)

#create vector of 1's that matches test set length
forest_classification<-rep("1",4800)

#Predict classification using 0.5 threshold. Why 0.5 and not 0.22104? Use the same as in cutoff above
forest_classification[forest_probabilities[,2]<0.22104]="0"
forest_classification<-as.factor(forest_classification)

#confusion matrix
confusionMatrix(forest_classification,testing$default_0, positive="1") #Display confusion matrix. Note, confusion matrix actually displays a better accuracy with threshold of 50%
  #observations: Accuracy = 80.98% (increase from logit model)

#ROC Curve
forest_ROC_prediction <- prediction(forest_probabilities[,2], testing$default_0) #Calculate errors
forest_ROC <- performance(forest_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(forest_ROC) #Plot ROC curve

#AUC (area under curve)
AUC.tmp <- performance(forest_ROC_prediction,"auc") #Create AUC data
forest_AUC <- as.numeric(AUC.tmp@y.values) #Calculate AUC
forest_AUC #Display AUC value
  #observations: AUC = 0.7021 (decrease from logit model)

# Lift chart
plotLift(forest_probabilities[,2],  testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

#----- TEST GRADIENT BOOSTING MODEL -----

credit_matrix <- model.matrix(default_0 ~ ., data = credit)[,-1]

x_train <- credit_matrix[ inTrain,]
x_test <- credit_matrix[ -inTrain,]

#grabbing the response var from original dataset
y_train <-training$default_0
y_test <-testing$default_0

#define gradient boosted model
model_XGboost<-xgboost(data = data.matrix(x_train), 
                       label = as.numeric(as.character(y_train)), 
                       eta = 0.03,       # hyperparameter: learning rate  (typically want lower...must increase rounds)
                       max_depth = 7,  # hyperparameter: size of a tree in each boosting iteration (lower to help prevent overfitting)
                       nround=500,       # hyperparameter: number of boosting iterations  
                       objective = "binary:logistic"
)
model_XGboost
  #observations: did 50 iterations (went from 0.1095 error rate to 0.0039 on 50th), with nfeatures = 77

#Predict classification (for confusion matrix)
XGboost_prediction<-predict(model_XGboost,newdata=x_test, type="response")

#confusion matrix
confusionMatrix(as.factor(ifelse(XGboost_prediction>0.22104,1,0)),y_test,positive="1")
  #observations: Accuracy = 76.60% (tiny increase from logit model but less than random forest)

#ROC Curve
XGboost_ROC_prediction <- prediction(XGboost_prediction, y_test) #Calculate errors
XGboost_ROC_testing <- performance(XGboost_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(XGboost_ROC_testing) #Plot ROC curve

#AUC
auc.tmp <- performance(XGboost_ROC_prediction,"auc") #Create AUC data
XGboost_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
XGboost_auc_testing #Display AUC value
  #observations: AUC = 0.7506 (better than random forest model but tiny bit worse than logit)

# Lift chart
plotLift(XGboost_prediction, y_test, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

#----- TEST SUPPORT VECTOR MACHINES MODEL -----

#define SVM model
model_svm <- svm(default_0 ~., data=training, probability=TRUE, gamma=0.01, cost=1)
summary(model_svm)
  #observations: # of support vectors = 8262

#predict classification
svm_probabilities<-attr(predict(model_svm,newdata=testing, probability=TRUE), "prob")
svm_prediction<-svm_probabilities[,1]

#create vector of 1's that matches test set length
svm_classification<-rep("1",4800)

#set threshold
svm_classification[svm_prediction<0.22104]="0" 
svm_classification<-as.factor(svm_classification)

#confusion matrix
confusionMatrix(svm_classification,testing$default_0,positive = "1")
  #observations: Accuracy = 81.25% (highest thus far, slightly better than random forest model)

#ROC Curve
svm_ROC_prediction <- prediction(svm_prediction, testing$default_0) #Calculate errors
svm_ROC_testing <- performance(svm_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(svm_ROC_testing) #Plot ROC curve

#AUC
auc.tmp <- performance(svm_ROC_prediction,"auc") #Create AUC data
svm_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
svm_auc_testing #Display AUC value
  #observations: AUC = 0.7015 (worst thus far, slightly worse than random forest)

# Lift chart
plotLift(svm_prediction, testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

#----- TEST NEURAL NET (ANN) MODEL -----

credit_ANNmatrix <- model.matrix(default_0 ~ ., data = credit)[,-1]

str(credit_ANNmatrix) #this will give us #columns

credit_ANNmatrix <- scale(credit_ANNmatrix) # scaling for X (like the z-score)

x_ANNtrain <- credit_ANNmatrix[ inTrain,]
x_ANNtest <- credit_ANNmatrix[ -inTrain,]

#grabbing the response var from original dataset
y_ANNtrain <-training$default_0
y_ANNtest <-testing$default_0

#Keras interprets data using row-major semantics (as opposed to R's default column-major semantics). Must "reshape" the matrices 
#**NOTE**: The columns parameter below is subject to change (check # of columns in x_ANNtrain/test and replace the integer below)
x_ANNtrain <- array_reshape(x_ANNtrain, c(nrow(x_ANNtrain), 211))
x_ANNtest <- array_reshape(x_ANNtest, c(nrow(x_ANNtest), 211))

#convert to categoricals
y_ANNtrain <- to_categorical(y_train, 2) #this is python code (leveraging keras package)
y_ANNtest <- to_categorical(y_test, 2)

#define keras model
model_keras <- keras_model_sequential()

model_keras %>% 
  layer_dense(units = 128, activation = 'sigmoid') %>% 
  layer_dropout(rate = 0.3) %>% 
  layer_dense(units = 64, activation = 'sigmoid') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 2, activation = 'softmax')

model_keras %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

#training / "fitting" the model
history <- model_keras %>% fit(
  x_ANNtrain, y_ANNtrain, # on what data to train
  epochs = 90, # how many repetitions to have
  batch_size = 256, # how many datapoints are fed to the network at a time 
  validation_split = 0.2  # percentage of training data to keep for cross-validation 
)

summary(model_keras)
  #observations: 18,370 parameters

plot(history)
  #observations: Need more epochs? They don't intersect within 30 epochs

#model %>% evaluate(x_test, y_test) # apply the model to testing data
#predict probabilities
TF_NN_probabilities <- model_keras %>% predict(x_ANNtest)
TF_NN_prediction <- TF_NN_probabilities[,2]

#create vector of 1's that matches test set length
TF_NN_classification<-rep("1",4800)
TF_NN_classification[TF_NN_prediction<0.22104]="0" 
TF_NN_classification<-as.factor(TF_NN_classification)

confusionMatrix(TF_NN_classification,testing$default_0,positive = "1")

#ROC Curve
TF_NN_ROC_prediction <- prediction(TF_NN_prediction, testing$default_0) #Calculate errors
TF_NN_ROC_testing <- performance(TF_NN_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(TF_NN_ROC_testing) #Plot ROC curve

#AUC
auc.tmp <- performance(TF_NN_ROC_prediction,"auc") #Create AUC data
TF_NN_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
TF_NN_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#Lift chart
plotLift(TF_NN_prediction, testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart


#----- TEST ON SOLUTION SET -----
solutions <- read.csv("Queen's MMA\\MMA 867\\Assignment 3\\new_applications_actuals.csv", header=TRUE, sep = ",")

#replicate all feature engineering
solutions <- solutions %>%
  dplyr::select(-CONCAT,-ID)

#change necessary data types
#we want to change these variables from integers to factor variables
solutions$SEX <- as.factor(solutions$SEX)
solutions$EDUCATION <- as.factor(solutions$EDUCATION)
solutions$MARRIAGE <- as.factor(solutions$MARRIAGE)
solutions$default.payment.next.month <- as.factor(solutions$default.payment.next.month)
solutions$BILL_AMT1 <- as.numeric(solutions$BILL_AMT1)
solutions$BILL_AMT2 <- as.numeric(solutions$BILL_AMT2)
solutions$BILL_AMT3 <- as.numeric(solutions$BILL_AMT3)
solutions$BILL_AMT4 <- as.numeric(solutions$BILL_AMT4)
solutions$BILL_AMT5 <- as.numeric(solutions$BILL_AMT5)
solutions$BILL_AMT6 <- as.numeric(solutions$BILL_AMT6)
solutions$PAY_AMT4 <- as.numeric(solutions$PAY_AMT4)
solutions$PAY_AMT6 <- as.numeric(solutions$PAY_AMT6)


solutions$PAY_1T <- as.factor(solutions$PAY_1)
solutions$PAY_2T <- as.factor(solutions$PAY_2)
solutions$PAY_3T <- as.factor(solutions$PAY_3)
solutions$PAY_4T <- as.factor(solutions$PAY_4)
solutions$PAY_5T <- as.factor(solutions$PAY_5)
solutions$PAY_6T <- as.factor(solutions$PAY_6)

str(solutions)

#---NOTE: There are a handful of '0' values for Marriage (42 records) and Education (11 records).
#         '0' is unaccounted for in the data dictionary for these variables.
#         After conferring with TA, We will leave them as is (to not potentially contaminate the other buckets).

#1 Pay_Amt/Age
solutions$PAY_AMT1_DIV_AGE <- solutions$PAY_AMT1/solutions$AGE
solutions$PAY_AMT2_DIV_AGE <- solutions$PAY_AMT2/solutions$AGE
solutions$PAY_AMT3_DIV_AGE <- solutions$PAY_AMT3/solutions$AGE
solutions$PAY_AMT4_DIV_AGE <- solutions$PAY_AMT4/solutions$AGE
solutions$PAY_AMT5_DIV_AGE <- solutions$PAY_AMT5/solutions$AGE
solutions$PAY_AMT6_DIV_AGE <- solutions$PAY_AMT6/solutions$AGE

#2 Pay_Amt/Education
#convert Education to numeric type, engineer new variable, convert Education back to factor
solutions$EDUCATION <- as.numeric(solutions$EDUCATION)

solutions$PAY_AMT1_DIV_EDU <- solutions$PAY_AMT1/solutions$EDUCATION
solutions$PAY_AMT2_DIV_EDU <- solutions$PAY_AMT2/solutions$EDUCATION
solutions$PAY_AMT3_DIV_EDU <- solutions$PAY_AMT3/solutions$EDUCATION
solutions$PAY_AMT4_DIV_EDU <- solutions$PAY_AMT4/solutions$EDUCATION
solutions$PAY_AMT5_DIV_EDU <- solutions$PAY_AMT5/solutions$EDUCATION
solutions$PAY_AMT6_DIV_EDU <- solutions$PAY_AMT6/solutions$EDUCATION

solutions$EDUCATION <- solutions$EDUCATION-1 #for whatever reason, converting to numeric above added +1
solutions$EDUCATION <- as.factor(solutions$EDUCATION)

#3 Bill_Amt/Age
#Creating 6 variables - 1 for each Bill_Amt
solutions$BILL_AMT1_DIV_AGE <- solutions$BILL_AMT1/solutions$AGE
solutions$BILL_AMT2_DIV_AGE <- solutions$BILL_AMT2/solutions$AGE
solutions$BILL_AMT3_DIV_AGE <- solutions$BILL_AMT3/solutions$AGE
solutions$BILL_AMT4_DIV_AGE <- solutions$BILL_AMT4/solutions$AGE
solutions$BILL_AMT5_DIV_AGE <- solutions$BILL_AMT5/solutions$AGE
solutions$BILL_AMT6_DIV_AGE <- solutions$BILL_AMT6/solutions$AGE

#4 Bill_Amt/Education
#Creating 6 variables - 1 for each Bill_Amt
#convert Education to numeric type, engineer new variable, convert Education back to factor
solutions$EDUCATION <- as.numeric(solutions$EDUCATION)

solutions$BILL_AMT1_DIV_EDU <- solutions$BILL_AMT1/solutions$EDUCATION
solutions$BILL_AMT2_DIV_EDU <- solutions$BILL_AMT2/solutions$EDUCATION
solutions$BILL_AMT3_DIV_EDU <- solutions$BILL_AMT3/solutions$EDUCATION
solutions$BILL_AMT4_DIV_EDU <- solutions$BILL_AMT4/solutions$EDUCATION
solutions$BILL_AMT5_DIV_EDU <- solutions$BILL_AMT5/solutions$EDUCATION
solutions$BILL_AMT6_DIV_EDU <- solutions$BILL_AMT6/solutions$EDUCATION

solutions$EDUCATION <- solutions$EDUCATION-1 #for whatever reason, converting to numeric above added +1
solutions$EDUCATION <- as.factor(solutions$EDUCATION)

#5 Bill_Amt - Pay_Amt
#Creating 5 variables - 1 for each pair of Bill-Pay period (Pay_Amt1 is payment for Bill_Amt2)
solutions$PAY_DIFFERENCE_AMT1 <- solutions$BILL_AMT2-solutions$PAY_AMT1
solutions$PAY_DIFFERENCE_AMT2 <- solutions$BILL_AMT3-solutions$PAY_AMT2
solutions$PAY_DIFFERENCE_AMT3 <- solutions$BILL_AMT4-solutions$PAY_AMT3
solutions$PAY_DIFFERENCE_AMT4 <- solutions$BILL_AMT5-solutions$PAY_AMT4
solutions$PAY_DIFFERENCE_AMT5 <- solutions$BILL_AMT6-solutions$PAY_AMT5

#6 (Bill_Amt-Pay_Amt)/Age
#Creating 5 variables - 1 for each Bill-Pay pair
solutions$PAY_DIFFERENCE_AMT1_DIV_AGE <- solutions$PAY_DIFFERENCE_AMT1/solutions$AGE
solutions$PAY_DIFFERENCE_AMT2_DIV_AGE <- solutions$PAY_DIFFERENCE_AMT2/solutions$AGE
solutions$PAY_DIFFERENCE_AMT3_DIV_AGE <- solutions$PAY_DIFFERENCE_AMT3/solutions$AGE
solutions$PAY_DIFFERENCE_AMT4_DIV_AGE <- solutions$PAY_DIFFERENCE_AMT4/solutions$AGE
solutions$PAY_DIFFERENCE_AMT5_DIV_AGE <- solutions$PAY_DIFFERENCE_AMT5/solutions$AGE

#7 Bill_Amt-Pay amt/Education
#Creating 5 variables - 1 for each Bill-pay pair
#convert Education to numeric type, engineer new variable, convert Education back to factor
solutions$EDUCATION <- as.numeric(solutions$EDUCATION)

solutions$PAY_DIFFERENCE_AMT1_DIV_EDU <- solutions$PAY_DIFFERENCE_AMT1/solutions$EDUCATION
solutions$PAY_DIFFERENCE_AMT2_DIV_EDU <- solutions$PAY_DIFFERENCE_AMT2/solutions$EDUCATION
solutions$PAY_DIFFERENCE_AMT3_DIV_EDU <- solutions$PAY_DIFFERENCE_AMT3/solutions$EDUCATION
solutions$PAY_DIFFERENCE_AMT4_DIV_EDU <- solutions$PAY_DIFFERENCE_AMT4/solutions$EDUCATION
solutions$PAY_DIFFERENCE_AMT5_DIV_EDU <- solutions$PAY_DIFFERENCE_AMT5/solutions$EDUCATION

solutions$EDUCATION <- solutions$EDUCATION-1 #for whatever reason, converting to numeric above added +1
solutions$EDUCATION <- as.factor(solutions$EDUCATION)

#8 Credit limit/Age
#Creating 1 variable
solutions$CREDIT_LIMIT_DIV_AGE <- solutions$LIMIT_BAL/solutions$AGE

#9 Credit Limit/Education
#Creating 1 variable
#convert Education to numeric type, engineer new variable, convert Education back to factor
solutions$EDUCATION <- as.numeric(solutions$EDUCATION)

solutions$CREDIT_LIMIT_DIV_EDU <- solutions$LIMIT_BAL/solutions$EDUCATION

solutions$EDUCATION <- solutions$EDUCATION-1 #for whatever reason, converting to numeric above added +1
solutions$EDUCATION <- as.factor(solutions$EDUCATION)

#12 Total Payments Satisfied
#can check for only 5 bill-pay pairs so this number will range from 0 to 5
solutions$TOTAL_PAYMENTS_SATISFIED <- 0

solutions$TOTAL_PAYMENTS_SATISFIED <- if_else(solutions$PAY_AMT1>=solutions$BILL_AMT2,1+solutions$TOTAL_PAYMENTS_SATISFIED, solutions$TOTAL_PAYMENTS_SATISFIED)
solutions$TOTAL_PAYMENTS_SATISFIED <- if_else(solutions$PAY_AMT2>=solutions$BILL_AMT3,1+solutions$TOTAL_PAYMENTS_SATISFIED, solutions$TOTAL_PAYMENTS_SATISFIED)
solutions$TOTAL_PAYMENTS_SATISFIED <- if_else(solutions$PAY_AMT3>=solutions$BILL_AMT4,1+solutions$TOTAL_PAYMENTS_SATISFIED, solutions$TOTAL_PAYMENTS_SATISFIED)
solutions$TOTAL_PAYMENTS_SATISFIED <- if_else(solutions$PAY_AMT4>=solutions$BILL_AMT5,1+solutions$TOTAL_PAYMENTS_SATISFIED, solutions$TOTAL_PAYMENTS_SATISFIED)
solutions$TOTAL_PAYMENTS_SATISFIED <- if_else(solutions$PAY_AMT5>=solutions$BILL_AMT6,1+solutions$TOTAL_PAYMENTS_SATISFIED, solutions$TOTAL_PAYMENTS_SATISFIED)

#13 Total Payments Missed - need to run code for FE #12 to run this
#exact inverse of Total Payments Satisfied
solutions$TOTAL_PAYMENTS_MISSED <- 5 - solutions$TOTAL_PAYMENTS_SATISFIED
#collinearity?

#16 Zero Bill
#Creating 6 variables, change to factors
solutions$ZERO_BILL1 <- if_else(solutions$BILL_AMT1==0,1,0)
solutions$ZERO_BILL2 <- if_else(solutions$BILL_AMT2==0,1,0)
solutions$ZERO_BILL3 <- if_else(solutions$BILL_AMT3==0,1,0)
solutions$ZERO_BILL4 <- if_else(solutions$BILL_AMT4==0,1,0)
solutions$ZERO_BILL5 <- if_else(solutions$BILL_AMT5==0,1,0)
solutions$ZERO_BILL6 <- if_else(solutions$BILL_AMT6==0,1,0)

#17 Zero Pay
#Creating 6 variables, change to factors
solutions$ZERO_PAY1 <- if_else(solutions$PAY_AMT1==0,1,0)
solutions$ZERO_PAY2 <- if_else(solutions$PAY_AMT2==0,1,0)
solutions$ZERO_PAY3 <- if_else(solutions$PAY_AMT3==0,1,0)
solutions$ZERO_PAY4 <- if_else(solutions$PAY_AMT4==0,1,0)
solutions$ZERO_PAY5 <- if_else(solutions$PAY_AMT5==0,1,0)
solutions$ZERO_PAY6 <- if_else(solutions$PAY_AMT6==0,1,0)

#14 Total Zero Bill Months - need FE #16 to run this
solutions$TOTAL_ZERO_BILL_MONTHS <- solutions$ZERO_BILL1+solutions$ZERO_BILL2+solutions$ZERO_BILL3+solutions$ZERO_BILL4+solutions$ZERO_BILL5+solutions$ZERO_BILL6

solutions$ZERO_BILL1 <- as.factor(solutions$ZERO_BILL1)
solutions$ZERO_BILL2 <- as.factor(solutions$ZERO_BILL2)
solutions$ZERO_BILL3 <- as.factor(solutions$ZERO_BILL3)
solutions$ZERO_BILL4 <- as.factor(solutions$ZERO_BILL4)
solutions$ZERO_BILL5 <- as.factor(solutions$ZERO_BILL5)
solutions$ZERO_BILL6 <- as.factor(solutions$ZERO_BILL6)

#15 Total Zero Pay Months - need FE #17 to run this
#change FE #17 back to numeric, then return to factor
solutions$TOTAL_ZERO_PAY_MONTHS <- solutions$ZERO_PAY1+solutions$ZERO_PAY2+solutions$ZERO_PAY3+solutions$ZERO_PAY4+solutions$ZERO_PAY5+solutions$ZERO_PAY6

solutions$ZERO_PAY1 <- as.factor(solutions$ZERO_PAY1)
solutions$ZERO_PAY2 <- as.factor(solutions$ZERO_PAY2)
solutions$ZERO_PAY3 <- as.factor(solutions$ZERO_PAY3)
solutions$ZERO_PAY4 <- as.factor(solutions$ZERO_PAY4)
solutions$ZERO_PAY5 <- as.factor(solutions$ZERO_PAY5)
solutions$ZERO_PAY6 <- as.factor(solutions$ZERO_PAY6)

#18 Bill Range
i <- 1
solutions$BILL_RANGE <- 0

for (i in 1:nrow(solutions)){
  solutions$BILL_RANGE[i] <- max(solutions$BILL_AMT1[i], solutions$BILL_AMT2[i], solutions$BILL_AMT3[i], solutions$BILL_AMT4[i], solutions$BILL_AMT5[i], solutions$BILL_AMT6[i]) - min(solutions$BILL_AMT1[i], solutions$BILL_AMT2[i], solutions$BILL_AMT3[i], solutions$BILL_AMT4[i], solutions$BILL_AMT5[i], solutions$BILL_AMT6[i])
}

#19 Pay Range
i <- 1
solutions$PAY_RANGE <- 0

for (i in 1:nrow(solutions)){
  solutions$PAY_RANGE[i] <- max(solutions$PAY_AMT1[i], solutions$PAY_AMT2[i], solutions$PAY_AMT3[i], solutions$PAY_AMT4[i], solutions$PAY_AMT5[i], solutions$PAY_AMT6[i]) - min(solutions$PAY_AMT1[i], solutions$PAY_AMT2[i], solutions$PAY_AMT3[i], solutions$PAY_AMT4[i], solutions$PAY_AMT5[i], solutions$PAY_AMT6[i])
}

#23 Cumulative % of Bill Paid
solutions$CUMU_PERCENT_BILL_PAID1 <- if_else(solutions$BILL_AMT1 == 0,1,solutions$PAY_AMT1/solutions$BILL_AMT1)
solutions$CUMU_PERCENT_BILL_PAID2 <- if_else(solutions$BILL_AMT2 == 0,1,solutions$PAY_AMT2/solutions$BILL_AMT2)
solutions$CUMU_PERCENT_BILL_PAID3 <- if_else(solutions$BILL_AMT3 == 0,1,solutions$PAY_AMT3/solutions$BILL_AMT3)
solutions$CUMU_PERCENT_BILL_PAID4 <- if_else(solutions$BILL_AMT4 == 0,1,solutions$PAY_AMT4/solutions$BILL_AMT4)
solutions$CUMU_PERCENT_BILL_PAID5 <- if_else(solutions$BILL_AMT5 == 0,1,solutions$PAY_AMT5/solutions$BILL_AMT5)
solutions$CUMU_PERCENT_BILL_PAID6 <- if_else(solutions$BILL_AMT6 == 0,1,solutions$PAY_AMT6/solutions$BILL_AMT6)

#24 Dummy: Bill Amount is 1 SD away
#creating 6 variables, 1 for each Bill Amt month, convert to factors
solutions$BILL_AMT1_1_SD_AWAY <- 0
solutions$BILL_AMT2_1_SD_AWAY <- 0
solutions$BILL_AMT3_1_SD_AWAY <- 0
solutions$BILL_AMT4_1_SD_AWAY <- 0
solutions$BILL_AMT5_1_SD_AWAY <- 0
solutions$BILL_AMT6_1_SD_AWAY <- 0

solutions$BILL_AMT1_1_SD_AWAY <- if_else(solutions$BILL_AMT1 >= mean(solutions$BILL_AMT1)+sqrt(var(solutions$BILL_AMT1)), 1, solutions$BILL_AMT1_1_SD_AWAY)
solutions$BILL_AMT1_1_SD_AWAY <- if_else(solutions$BILL_AMT1 <= mean(solutions$BILL_AMT1)-sqrt(var(solutions$BILL_AMT1)), 1, solutions$BILL_AMT1_1_SD_AWAY)

solutions$BILL_AMT2_1_SD_AWAY <- if_else(solutions$BILL_AMT2 >= mean(solutions$BILL_AMT2)+sqrt(var(solutions$BILL_AMT2)), 1, solutions$BILL_AMT2_1_SD_AWAY)
solutions$BILL_AMT2_1_SD_AWAY <- if_else(solutions$BILL_AMT2 <= mean(solutions$BILL_AMT2)-sqrt(var(solutions$BILL_AMT2)), 1, solutions$BILL_AMT2_1_SD_AWAY)

solutions$BILL_AMT3_1_SD_AWAY <- if_else(solutions$BILL_AMT3 >= mean(solutions$BILL_AMT3)+sqrt(var(solutions$BILL_AMT3)), 1, solutions$BILL_AMT3_1_SD_AWAY)
solutions$BILL_AMT3_1_SD_AWAY <- if_else(solutions$BILL_AMT3 <= mean(solutions$BILL_AMT3)-sqrt(var(solutions$BILL_AMT3)), 1, solutions$BILL_AMT3_1_SD_AWAY)

solutions$BILL_AMT4_1_SD_AWAY <- if_else(solutions$BILL_AMT4 >= mean(solutions$BILL_AMT4)+sqrt(var(solutions$BILL_AMT4)), 1, solutions$BILL_AMT4_1_SD_AWAY)
solutions$BILL_AMT4_1_SD_AWAY <- if_else(solutions$BILL_AMT4 <= mean(solutions$BILL_AMT4)-sqrt(var(solutions$BILL_AMT4)), 1, solutions$BILL_AMT4_1_SD_AWAY)

solutions$BILL_AMT5_1_SD_AWAY <- if_else(solutions$BILL_AMT5 >= mean(solutions$BILL_AMT5)+sqrt(var(solutions$BILL_AMT5)), 1, solutions$BILL_AMT5_1_SD_AWAY)
solutions$BILL_AMT5_1_SD_AWAY <- if_else(solutions$BILL_AMT5 <= mean(solutions$BILL_AMT5)-sqrt(var(solutions$BILL_AMT5)), 1, solutions$BILL_AMT5_1_SD_AWAY)

solutions$BILL_AMT6_1_SD_AWAY <- if_else(solutions$BILL_AMT6 >= mean(solutions$BILL_AMT6)+sqrt(var(solutions$BILL_AMT6)), 1, solutions$BILL_AMT6_1_SD_AWAY)
solutions$BILL_AMT6_1_SD_AWAY <- if_else(solutions$BILL_AMT6 <= mean(solutions$BILL_AMT6)-sqrt(var(solutions$BILL_AMT6)), 1, solutions$BILL_AMT6_1_SD_AWAY)

solutions$BILL_AMT1_1_SD_AWAY <- as.factor(solutions$BILL_AMT1_1_SD_AWAY)
solutions$BILL_AMT2_1_SD_AWAY <- as.factor(solutions$BILL_AMT2_1_SD_AWAY)
solutions$BILL_AMT3_1_SD_AWAY <- as.factor(solutions$BILL_AMT3_1_SD_AWAY)
solutions$BILL_AMT4_1_SD_AWAY <- as.factor(solutions$BILL_AMT4_1_SD_AWAY)
solutions$BILL_AMT5_1_SD_AWAY <- as.factor(solutions$BILL_AMT5_1_SD_AWAY)
solutions$BILL_AMT6_1_SD_AWAY <- as.factor(solutions$BILL_AMT6_1_SD_AWAY)

#25 Dummy: Paid Amount is 1 SD away
#creating 6 variables, 1 for each Pay Amt month, convert to factors
solutions$PAY_AMT1_1_SD_AWAY <- 0
solutions$PAY_AMT2_1_SD_AWAY <- 0
solutions$PAY_AMT3_1_SD_AWAY <- 0
solutions$PAY_AMT4_1_SD_AWAY <- 0
solutions$PAY_AMT5_1_SD_AWAY <- 0
solutions$PAY_AMT6_1_SD_AWAY <- 0

solutions$PAY_AMT1_1_SD_AWAY <- if_else(solutions$PAY_AMT1 >= mean(solutions$PAY_AMT1)+sqrt(var(solutions$PAY_AMT1)), 1, solutions$PAY_AMT1_1_SD_AWAY)
solutions$PAY_AMT1_1_SD_AWAY <- if_else(solutions$PAY_AMT1 <= mean(solutions$PAY_AMT1)-sqrt(var(solutions$PAY_AMT1)), 1, solutions$PAY_AMT1_1_SD_AWAY)

solutions$PAY_AMT2_1_SD_AWAY <- if_else(solutions$PAY_AMT2 >= mean(solutions$PAY_AMT2)+sqrt(var(solutions$PAY_AMT2)), 1, solutions$PAY_AMT2_1_SD_AWAY)
solutions$PAY_AMT2_1_SD_AWAY <- if_else(solutions$PAY_AMT2 <= mean(solutions$PAY_AMT2)-sqrt(var(solutions$PAY_AMT2)), 1, solutions$PAY_AMT2_1_SD_AWAY)

solutions$PAY_AMT3_1_SD_AWAY <- if_else(solutions$PAY_AMT3 >= mean(solutions$PAY_AMT3)+sqrt(var(solutions$PAY_AMT3)), 1, solutions$PAY_AMT3_1_SD_AWAY)
solutions$PAY_AMT3_1_SD_AWAY <- if_else(solutions$PAY_AMT3 <= mean(solutions$PAY_AMT3)-sqrt(var(solutions$PAY_AMT3)), 1, solutions$PAY_AMT3_1_SD_AWAY)

solutions$PAY_AMT4_1_SD_AWAY <- if_else(solutions$PAY_AMT4 >= mean(solutions$PAY_AMT4)+sqrt(var(solutions$PAY_AMT4)), 1, solutions$PAY_AMT4_1_SD_AWAY)
solutions$PAY_AMT4_1_SD_AWAY <- if_else(solutions$PAY_AMT4 <= mean(solutions$PAY_AMT4)-sqrt(var(solutions$PAY_AMT4)), 1, solutions$PAY_AMT4_1_SD_AWAY)

solutions$PAY_AMT5_1_SD_AWAY <- if_else(solutions$PAY_AMT5 >= mean(solutions$PAY_AMT5)+sqrt(var(solutions$PAY_AMT5)), 1, solutions$PAY_AMT5_1_SD_AWAY)
solutions$PAY_AMT5_1_SD_AWAY <- if_else(solutions$PAY_AMT5 <= mean(solutions$PAY_AMT5)-sqrt(var(solutions$PAY_AMT5)), 1, solutions$PAY_AMT5_1_SD_AWAY)

solutions$PAY_AMT6_1_SD_AWAY <- if_else(solutions$PAY_AMT6 >= mean(solutions$PAY_AMT6)+sqrt(var(solutions$PAY_AMT6)), 1, solutions$PAY_AMT6_1_SD_AWAY)
solutions$PAY_AMT6_1_SD_AWAY <- if_else(solutions$PAY_AMT6 <= mean(solutions$PAY_AMT6)-sqrt(var(solutions$PAY_AMT6)), 1, solutions$PAY_AMT6_1_SD_AWAY)

solutions$PAY_AMT1_1_SD_AWAY <- as.factor(solutions$PAY_AMT1_1_SD_AWAY)
solutions$PAY_AMT2_1_SD_AWAY <- as.factor(solutions$PAY_AMT2_1_SD_AWAY)
solutions$PAY_AMT3_1_SD_AWAY <- as.factor(solutions$PAY_AMT3_1_SD_AWAY)
solutions$PAY_AMT4_1_SD_AWAY <- as.factor(solutions$PAY_AMT4_1_SD_AWAY)
solutions$PAY_AMT5_1_SD_AWAY <- as.factor(solutions$PAY_AMT5_1_SD_AWAY)
solutions$PAY_AMT6_1_SD_AWAY <- as.factor(solutions$PAY_AMT6_1_SD_AWAY)

#26 Dummy: Bill amt - Paid amt 1 SD away - you need FE variable #5 to use code for this variable
#creating 5 variables, 1 for each Bill-Pay pair, convert to factors
solutions$PAY_DIFFERENCE_AMT1_1_SD_AWAY <- 0
solutions$PAY_DIFFERENCE_AMT2_1_SD_AWAY <- 0
solutions$PAY_DIFFERENCE_AMT3_1_SD_AWAY <- 0
solutions$PAY_DIFFERENCE_AMT4_1_SD_AWAY <- 0
solutions$PAY_DIFFERENCE_AMT5_1_SD_AWAY <- 0

solutions$PAY_DIFFERENCE_AMT1_1_SD_AWAY <- if_else(solutions$PAY_DIFFERENCE_AMT1 >= mean(solutions$PAY_DIFFERENCE_AMT1)+sqrt(var(solutions$PAY_DIFFERENCE_AMT1)), 1, solutions$PAY_DIFFERENCE_AMT1_1_SD_AWAY)
solutions$PAY_DIFFERENCE_AMT1_1_SD_AWAY <- if_else(solutions$PAY_DIFFERENCE_AMT1 <= mean(solutions$PAY_DIFFERENCE_AMT1)-sqrt(var(solutions$PAY_DIFFERENCE_AMT1)), 1, solutions$PAY_DIFFERENCE_AMT1_1_SD_AWAY)

solutions$PAY_DIFFERENCE_AMT2_1_SD_AWAY <- if_else(solutions$PAY_DIFFERENCE_AMT2 >= mean(solutions$PAY_DIFFERENCE_AMT2)+sqrt(var(solutions$PAY_DIFFERENCE_AMT2)), 1, solutions$PAY_DIFFERENCE_AMT2_1_SD_AWAY)
solutions$PAY_DIFFERENCE_AMT2_1_SD_AWAY <- if_else(solutions$PAY_DIFFERENCE_AMT2 <= mean(solutions$PAY_DIFFERENCE_AMT2)-sqrt(var(solutions$PAY_DIFFERENCE_AMT2)), 1, solutions$PAY_DIFFERENCE_AMT2_1_SD_AWAY)

solutions$PAY_DIFFERENCE_AMT3_1_SD_AWAY <- if_else(solutions$PAY_DIFFERENCE_AMT3 >= mean(solutions$PAY_DIFFERENCE_AMT3)+sqrt(var(solutions$PAY_DIFFERENCE_AMT3)), 1, solutions$PAY_DIFFERENCE_AMT3_1_SD_AWAY)
solutions$PAY_DIFFERENCE_AMT3_1_SD_AWAY <- if_else(solutions$PAY_DIFFERENCE_AMT3 <= mean(solutions$PAY_DIFFERENCE_AMT3)-sqrt(var(solutions$PAY_DIFFERENCE_AMT3)), 1, solutions$PAY_DIFFERENCE_AMT3_1_SD_AWAY)

solutions$PAY_DIFFERENCE_AMT4_1_SD_AWAY <- if_else(solutions$PAY_DIFFERENCE_AMT4 >= mean(solutions$PAY_DIFFERENCE_AMT4)+sqrt(var(solutions$PAY_DIFFERENCE_AMT4)), 1, solutions$PAY_DIFFERENCE_AMT4_1_SD_AWAY)
solutions$PAY_DIFFERENCE_AMT4_1_SD_AWAY <- if_else(solutions$PAY_DIFFERENCE_AMT4 <= mean(solutions$PAY_DIFFERENCE_AMT4)-sqrt(var(solutions$PAY_DIFFERENCE_AMT4)), 1, solutions$PAY_DIFFERENCE_AMT4_1_SD_AWAY)

solutions$PAY_DIFFERENCE_AMT5_1_SD_AWAY <- if_else(solutions$PAY_DIFFERENCE_AMT5 >= mean(solutions$PAY_DIFFERENCE_AMT5)+sqrt(var(solutions$PAY_DIFFERENCE_AMT5)), 1, solutions$PAY_DIFFERENCE_AMT5_1_SD_AWAY)
solutions$PAY_DIFFERENCE_AMT5_1_SD_AWAY <- if_else(solutions$PAY_DIFFERENCE_AMT5 <= mean(solutions$PAY_DIFFERENCE_AMT5)-sqrt(var(solutions$PAY_DIFFERENCE_AMT5)), 1, solutions$PAY_DIFFERENCE_AMT5_1_SD_AWAY)

solutions$PAY_DIFFERENCE_AMT1_1_SD_AWAY <- as.factor(solutions$PAY_DIFFERENCE_AMT1_1_SD_AWAY)
solutions$PAY_DIFFERENCE_AMT2_1_SD_AWAY <- as.factor(solutions$PAY_DIFFERENCE_AMT2_1_SD_AWAY)
solutions$PAY_DIFFERENCE_AMT3_1_SD_AWAY <- as.factor(solutions$PAY_DIFFERENCE_AMT3_1_SD_AWAY)
solutions$PAY_DIFFERENCE_AMT4_1_SD_AWAY <- as.factor(solutions$PAY_DIFFERENCE_AMT4_1_SD_AWAY)
solutions$PAY_DIFFERENCE_AMT5_1_SD_AWAY <- as.factor(solutions$PAY_DIFFERENCE_AMT5_1_SD_AWAY)

#27 Age Buckets
#convert to factor
solutions$AGE_BUCKET <- "a"

solutions$AGE_BUCKET <- if_else(solutions$AGE < 18,"< 18 yrs",solutions$AGE_BUCKET)
solutions$AGE_BUCKET <- if_else(solutions$AGE >= 18 & solutions$AGE < 25,"18-24 yrs",solutions$AGE_BUCKET)
solutions$AGE_BUCKET <- if_else(solutions$AGE >= 25 & solutions$AGE < 35,"25-34 yrs",solutions$AGE_BUCKET)
solutions$AGE_BUCKET <- if_else(solutions$AGE >= 35 & solutions$AGE < 45,"35-44 yrs",solutions$AGE_BUCKET)
solutions$AGE_BUCKET <- if_else(solutions$AGE >= 45 & solutions$AGE < 55,"45-54 yrs",solutions$AGE_BUCKET)
solutions$AGE_BUCKET <- if_else(solutions$AGE >= 55,"> 55 yrs",solutions$AGE_BUCKET)

solutions$AGE_BUCKET <- as.factor((solutions$AGE_BUCKET))

#28 Limit_Bal Buckets i.e. Credit Limit Buckets
#convert to factor
solutions$CREDIT_LIMIT_BUCKET <- "a"

solutions$CREDIT_LIMIT_BUCKET <- if_else(solutions$LIMIT_BAL < 25000,"< 25K",solutions$CREDIT_LIMIT_BUCKET)
solutions$CREDIT_LIMIT_BUCKET <- if_else(solutions$LIMIT_BAL >= 25000 & solutions$LIMIT_BAL < 50000,"25-50K",solutions$CREDIT_LIMIT_BUCKET)
solutions$CREDIT_LIMIT_BUCKET <- if_else(solutions$LIMIT_BAL >= 50000 & solutions$LIMIT_BAL < 75000,"50-75K",solutions$CREDIT_LIMIT_BUCKET)
solutions$CREDIT_LIMIT_BUCKET <- if_else(solutions$LIMIT_BAL >= 75000 & solutions$LIMIT_BAL < 100000,"75-100K",solutions$CREDIT_LIMIT_BUCKET)
solutions$CREDIT_LIMIT_BUCKET <- if_else(solutions$LIMIT_BAL >= 100000 & solutions$LIMIT_BAL < 125000,"100-125K",solutions$CREDIT_LIMIT_BUCKET)
solutions$CREDIT_LIMIT_BUCKET <- if_else(solutions$LIMIT_BAL >= 125000 & solutions$LIMIT_BAL < 150000,"125-150K",solutions$CREDIT_LIMIT_BUCKET)
solutions$CREDIT_LIMIT_BUCKET <- if_else(solutions$LIMIT_BAL >= 150000 & solutions$LIMIT_BAL < 175000,"150-175K",solutions$CREDIT_LIMIT_BUCKET)
solutions$CREDIT_LIMIT_BUCKET <- if_else(solutions$LIMIT_BAL >= 175000 & solutions$LIMIT_BAL < 200000,"175-200K",solutions$CREDIT_LIMIT_BUCKET)
solutions$CREDIT_LIMIT_BUCKET <- if_else(solutions$LIMIT_BAL >= 200000 & solutions$LIMIT_BAL < 225000,"200-225K",solutions$CREDIT_LIMIT_BUCKET)
solutions$CREDIT_LIMIT_BUCKET <- if_else(solutions$LIMIT_BAL >= 225000 & solutions$LIMIT_BAL < 250000,"225-250K",solutions$CREDIT_LIMIT_BUCKET)
solutions$CREDIT_LIMIT_BUCKET <- if_else(solutions$LIMIT_BAL >= 250000 & solutions$LIMIT_BAL < 300000,"250-300K",solutions$CREDIT_LIMIT_BUCKET)
solutions$CREDIT_LIMIT_BUCKET <- if_else(solutions$LIMIT_BAL >= 300000 & solutions$LIMIT_BAL < 400000,"300-400K",solutions$CREDIT_LIMIT_BUCKET)
solutions$CREDIT_LIMIT_BUCKET <- if_else(solutions$LIMIT_BAL >= 400000 & solutions$LIMIT_BAL < 500000,"400-500K",solutions$CREDIT_LIMIT_BUCKET)
solutions$CREDIT_LIMIT_BUCKET <- if_else(solutions$LIMIT_BAL >= 500000 & solutions$LIMIT_BAL < 700000,"500-700K",solutions$CREDIT_LIMIT_BUCKET)
solutions$CREDIT_LIMIT_BUCKET <- if_else(solutions$LIMIT_BAL >= 700000 & solutions$LIMIT_BAL < 900000,"700-900K",solutions$CREDIT_LIMIT_BUCKET)
solutions$CREDIT_LIMIT_BUCKET <- if_else(solutions$LIMIT_BAL >= 900000 & solutions$LIMIT_BAL < 1000000,"900K-1Mn",solutions$CREDIT_LIMIT_BUCKET)
solutions$CREDIT_LIMIT_BUCKET <- if_else(solutions$LIMIT_BAL >= 1000000,"> 1Mn",solutions$CREDIT_LIMIT_BUCKET)

solutions$CREDIT_LIMIT_BUCKET <- as.factor(solutions$CREDIT_LIMIT_BUCKET)

#29a Frequency of Late Payments - capturing count of payments that are 3 or more months late
solutions$LATE_PAYMENT_FREQUENCY3 <- 0

solutions$LATE_PAYMENT_FREQUENCY3 <- if_else(solutions$PAY_1 >= 3,solutions$LATE_PAYMENT_FREQUENCY3+1,solutions$LATE_PAYMENT_FREQUENCY3)
solutions$LATE_PAYMENT_FREQUENCY3 <- if_else(solutions$PAY_2 >= 3,solutions$LATE_PAYMENT_FREQUENCY3+1,solutions$LATE_PAYMENT_FREQUENCY3)
solutions$LATE_PAYMENT_FREQUENCY3 <- if_else(solutions$PAY_3 >= 3,solutions$LATE_PAYMENT_FREQUENCY3+1,solutions$LATE_PAYMENT_FREQUENCY3)
solutions$LATE_PAYMENT_FREQUENCY3 <- if_else(solutions$PAY_4 >= 3,solutions$LATE_PAYMENT_FREQUENCY3+1,solutions$LATE_PAYMENT_FREQUENCY3)
solutions$LATE_PAYMENT_FREQUENCY3 <- if_else(solutions$PAY_5 >= 3,solutions$LATE_PAYMENT_FREQUENCY3+1,solutions$LATE_PAYMENT_FREQUENCY3)
solutions$LATE_PAYMENT_FREQUENCY3 <- if_else(solutions$PAY_6 >= 3,solutions$LATE_PAYMENT_FREQUENCY3+1,solutions$LATE_PAYMENT_FREQUENCY3)

#29b Frequency of Late Payments - capturing count of payments that are 5 or more months late
solutions$LATE_PAYMENT_FREQUENCY5 <- 0

solutions$LATE_PAYMENT_FREQUENCY5 <- if_else(solutions$PAY_1 >= 5,solutions$LATE_PAYMENT_FREQUENCY5+1,solutions$LATE_PAYMENT_FREQUENCY5)
solutions$LATE_PAYMENT_FREQUENCY5 <- if_else(solutions$PAY_2 >= 5,solutions$LATE_PAYMENT_FREQUENCY5+1,solutions$LATE_PAYMENT_FREQUENCY5)
solutions$LATE_PAYMENT_FREQUENCY5 <- if_else(solutions$PAY_3 >= 5,solutions$LATE_PAYMENT_FREQUENCY5+1,solutions$LATE_PAYMENT_FREQUENCY5)
solutions$LATE_PAYMENT_FREQUENCY5 <- if_else(solutions$PAY_4 >= 5,solutions$LATE_PAYMENT_FREQUENCY5+1,solutions$LATE_PAYMENT_FREQUENCY5)
solutions$LATE_PAYMENT_FREQUENCY5 <- if_else(solutions$PAY_5 >= 5,solutions$LATE_PAYMENT_FREQUENCY5+1,solutions$LATE_PAYMENT_FREQUENCY5)
solutions$LATE_PAYMENT_FREQUENCY5 <- if_else(solutions$PAY_6 >= 5,solutions$LATE_PAYMENT_FREQUENCY5+1,solutions$LATE_PAYMENT_FREQUENCY5)

#29c Frequency of Late Payments - capturing count of payments that are 7 or more months late
solutions$LATE_PAYMENT_FREQUENCY7 <- 0

solutions$LATE_PAYMENT_FREQUENCY7 <- if_else(solutions$PAY_1 >= 7,solutions$LATE_PAYMENT_FREQUENCY7+1,solutions$LATE_PAYMENT_FREQUENCY7)
solutions$LATE_PAYMENT_FREQUENCY7 <- if_else(solutions$PAY_2 >= 7,solutions$LATE_PAYMENT_FREQUENCY7+1,solutions$LATE_PAYMENT_FREQUENCY7)
solutions$LATE_PAYMENT_FREQUENCY7 <- if_else(solutions$PAY_3 >= 7,solutions$LATE_PAYMENT_FREQUENCY7+1,solutions$LATE_PAYMENT_FREQUENCY7)
solutions$LATE_PAYMENT_FREQUENCY7 <- if_else(solutions$PAY_4 >= 7,solutions$LATE_PAYMENT_FREQUENCY7+1,solutions$LATE_PAYMENT_FREQUENCY7)
solutions$LATE_PAYMENT_FREQUENCY7 <- if_else(solutions$PAY_5 >= 7,solutions$LATE_PAYMENT_FREQUENCY7+1,solutions$LATE_PAYMENT_FREQUENCY7)
solutions$LATE_PAYMENT_FREQUENCY7 <- if_else(solutions$PAY_6 >= 7,solutions$LATE_PAYMENT_FREQUENCY7+1,solutions$LATE_PAYMENT_FREQUENCY7)

#29d Frequency of Late Payments - capturing count of payments that are 1 or more months late
solutions$LATE_PAYMENT_FREQUENCY1 <- 0

solutions$LATE_PAYMENT_FREQUENCY1 <- if_else(solutions$PAY_1 >= 1,solutions$LATE_PAYMENT_FREQUENCY1+1,solutions$LATE_PAYMENT_FREQUENCY1)
solutions$LATE_PAYMENT_FREQUENCY1 <- if_else(solutions$PAY_2 >= 1,solutions$LATE_PAYMENT_FREQUENCY1+1,solutions$LATE_PAYMENT_FREQUENCY1)
solutions$LATE_PAYMENT_FREQUENCY1 <- if_else(solutions$PAY_3 >= 1,solutions$LATE_PAYMENT_FREQUENCY1+1,solutions$LATE_PAYMENT_FREQUENCY1)
solutions$LATE_PAYMENT_FREQUENCY1 <- if_else(solutions$PAY_4 >= 1,solutions$LATE_PAYMENT_FREQUENCY1+1,solutions$LATE_PAYMENT_FREQUENCY1)
solutions$LATE_PAYMENT_FREQUENCY1 <- if_else(solutions$PAY_5 >= 1,solutions$LATE_PAYMENT_FREQUENCY1+1,solutions$LATE_PAYMENT_FREQUENCY1)
solutions$LATE_PAYMENT_FREQUENCY1 <- if_else(solutions$PAY_6 >= 1,solutions$LATE_PAYMENT_FREQUENCY1+1,solutions$LATE_PAYMENT_FREQUENCY1)

#30 Average Months Delayed - run #29d (late payment frequency of 1 or more months) before running this code
#convert PAY_x variables to numeric, create FE variable, convert PAY_x back to factor
solutions$PAY_1 <- as.numeric(solutions$PAY_1)
solutions$PAY_2 <- as.numeric(solutions$PAY_2)
solutions$PAY_3 <- as.numeric(solutions$PAY_3)
solutions$PAY_4 <- as.numeric(solutions$PAY_4)
solutions$PAY_5 <- as.numeric(solutions$PAY_5)
solutions$PAY_6 <- as.numeric(solutions$PAY_6)

i=1
solutions$AVERAGE_MONTHS_DELAYED <- 0

for (i in 1:nrow(solutions)) {
  solutions$AVERAGE_MONTHS_DELAYED[i] <- if_else(solutions$PAY_1[i] >= 1,solutions$PAY_1[i],solutions$AVERAGE_MONTHS_DELAYED[i])
  solutions$AVERAGE_MONTHS_DELAYED[i] <- if_else(solutions$PAY_2[i] >= 1,solutions$PAY_2[i]+solutions$AVERAGE_MONTHS_DELAYED[i],solutions$AVERAGE_MONTHS_DELAYED[i])
  solutions$AVERAGE_MONTHS_DELAYED[i] <- if_else(solutions$PAY_3[i] >= 1,solutions$PAY_3[i]+solutions$AVERAGE_MONTHS_DELAYED[i],solutions$AVERAGE_MONTHS_DELAYED[i])
  solutions$AVERAGE_MONTHS_DELAYED[i] <- if_else(solutions$PAY_4[i] >= 1,solutions$PAY_4[i]+solutions$AVERAGE_MONTHS_DELAYED[i],solutions$AVERAGE_MONTHS_DELAYED[i])
  solutions$AVERAGE_MONTHS_DELAYED[i] <- if_else(solutions$PAY_5[i] >= 1,solutions$PAY_5[i]+solutions$AVERAGE_MONTHS_DELAYED[i],solutions$AVERAGE_MONTHS_DELAYED[i])
  solutions$AVERAGE_MONTHS_DELAYED[i] <- if_else(solutions$PAY_6[i] >= 1,solutions$PAY_6[i]+solutions$AVERAGE_MONTHS_DELAYED[i],solutions$AVERAGE_MONTHS_DELAYED[i])
  
  solutions$AVERAGE_MONTHS_DELAYED[i] <- if_else(solutions$LATE_PAYMENT_FREQUENCY1[i] > 0, solutions$AVERAGE_MONTHS_DELAYED[i]/solutions$LATE_PAYMENT_FREQUENCY1[i], 0)
}

#31 Dummy: Average months delayed 1 SD away - you need FE variable #30 to use code for this variable
#creating 5 variables, 1 for each Bill-Pay pair, convert to factor
solutions$AVERAGE_MONTHS_DELAYED_1_SD_AWAY <- 0

solutions$AVERAGE_MONTHS_DELAYED_1_SD_AWAY <- if_else(solutions$AVERAGE_MONTHS_DELAYED >= mean(solutions$AVERAGE_MONTHS_DELAYED)+sqrt(var(solutions$AVERAGE_MONTHS_DELAYED)), 1, solutions$AVERAGE_MONTHS_DELAYED_1_SD_AWAY)
solutions$AVERAGE_MONTHS_DELAYED_1_SD_AWAY <- if_else(solutions$AVERAGE_MONTHS_DELAYED <= mean(solutions$AVERAGE_MONTHS_DELAYED)-sqrt(var(solutions$AVERAGE_MONTHS_DELAYED)), 1, solutions$AVERAGE_MONTHS_DELAYED_1_SD_AWAY)

solutions$AVERAGE_MONTHS_DELAYED_1_SD_AWAY <- as.factor(solutions$AVERAGE_MONTHS_DELAYED_1_SD_AWAY)

#32 Month on month percentage change in Bill Amount
#creating 5 new variables
#BILL_PERCENTAGE_CHANGE1 = (BILL_AMT1-BILL_AMT2)/BILL_AMT2
#same logic applies for months 2-3, 3-4, 4-5, 5-6 pairs
#if BILL_AMT2 = 0 but BILL_AMT1 > 0, then change will be captured as 100%
solutions$BILL_PERCENTAGE_CHANGE1 <- if_else(solutions$BILL_AMT2 != 0, (solutions$BILL_AMT1-solutions$BILL_AMT2)/solutions$BILL_AMT2, 1)
solutions$BILL_PERCENTAGE_CHANGE2 <- if_else(solutions$BILL_AMT3 != 0, (solutions$BILL_AMT2-solutions$BILL_AMT3)/solutions$BILL_AMT3, 1)
solutions$BILL_PERCENTAGE_CHANGE3 <- if_else(solutions$BILL_AMT4 != 0, (solutions$BILL_AMT3-solutions$BILL_AMT4)/solutions$BILL_AMT4, 1)
solutions$BILL_PERCENTAGE_CHANGE4 <- if_else(solutions$BILL_AMT5 != 0, (solutions$BILL_AMT4-solutions$BILL_AMT5)/solutions$BILL_AMT5, 1)
solutions$BILL_PERCENTAGE_CHANGE5 <- if_else(solutions$BILL_AMT6 != 0, (solutions$BILL_AMT5-solutions$BILL_AMT6)/solutions$BILL_AMT6, 1)

#33 Average Bill Amount as a % of Credit Limit (LIMIT_BAL field in table)
solutions$BILL_AMT1_PERCENTAGE <- solutions$BILL_AMT1/solutions$LIMIT_BAL
solutions$BILL_AMT2_PERCENTAGE <- solutions$BILL_AMT2/solutions$LIMIT_BAL
solutions$BILL_AMT3_PERCENTAGE <- solutions$BILL_AMT3/solutions$LIMIT_BAL
solutions$BILL_AMT4_PERCENTAGE <- solutions$BILL_AMT4/solutions$LIMIT_BAL
solutions$BILL_AMT5_PERCENTAGE <- solutions$BILL_AMT5/solutions$LIMIT_BAL
solutions$BILL_AMT6_PERCENTAGE <- solutions$BILL_AMT6/solutions$LIMIT_BAL

#34 Average Unpaid Amount as a % of Credit Limit (Avg unpaid amount is FE #5 - run that code first to be able to use this code)
solutions$PAY_DIFFERENCE_AMT1_PERCENTAGE <- solutions$PAY_DIFFERENCE_AMT1/solutions$LIMIT_BAL
solutions$PAY_DIFFERENCE_AMT2_PERCENTAGE <- solutions$PAY_DIFFERENCE_AMT2/solutions$LIMIT_BAL
solutions$PAY_DIFFERENCE_AMT3_PERCENTAGE <- solutions$PAY_DIFFERENCE_AMT3/solutions$LIMIT_BAL
solutions$PAY_DIFFERENCE_AMT4_PERCENTAGE <- solutions$PAY_DIFFERENCE_AMT4/solutions$LIMIT_BAL
solutions$PAY_DIFFERENCE_AMT5_PERCENTAGE <- solutions$PAY_DIFFERENCE_AMT5/solutions$LIMIT_BAL

solutions <- rename(solutions, default_0 = default.payment.next.month)

##RANDOM FOREST
#generate predictions
levels(solutions$PAY_1T) <- levels(training$PAY_1T)
levels(solutions$PAY_2T) <- levels(training$PAY_2T)
levels(solutions$PAY_3T) <- levels(training$PAY_3T)
levels(solutions$PAY_4T) <- levels(training$PAY_4T)
levels(solutions$PAY_5T) <- levels(training$PAY_5T)
levels(solutions$PAY_6T) <- levels(training$PAY_6T)
levels(solutions$CREDIT_LIMIT_BUCKET) <- levels(training$CREDIT_LIMIT_BUCKET)

#finding predictions: probabilities and classification
forest_probabilities2<-predict(model_forest,newdata=solutions,type="prob") #Predict probabilities -- an array with 2 columns: for not retained (class 0) and for retained (class 1)

#create vector of 1's that matches test set length
forest_classification2<-rep("1",4800)

#Predict classification
forest_classification2[forest_probabilities2[,2]<0.22104]="0"
forest_classification2<-as.factor(forest_classification2)

#Add predictions to the solution set
solutions$pred <- predict(model_forest, solutions)

#reorder
solutions <- solutions[,c(1:23,25:136,24,137)]

#score
solutions <- solutions %>%
  mutate(score = if_else(default_0 == pred,1,0))
sum(solutions$score)
#Score = 831/1000

#consider bank's profit
solutions <- solutions %>%
  mutate(profit = case_when(pred == 0 & default_0 == 0 ~ 1500,
                            pred == 0 & default_0 == 1 ~ -5000,
                            pred == 1 ~ 0))
sum(solutions$profit)
#Profit = $441,500

