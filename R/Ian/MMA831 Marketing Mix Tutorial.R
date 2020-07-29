#Purpose: Examine different linear regressions to understand and interpret impact of advertising on sales
  #Also see corresponding word file with model summary and tutorial Q&A

#Cleansing/Manipulation Packages:
pacman::p_load("dplyr","tidyr","stringr","lubridate","janitor")
#Graphing Packages:
pacman::p_load("ggplot2","esquisse","gridExtra")
#ML Packages:
pacman::p_load("car","MLmetrics","estimatr","fpp","forecast","caret","ROCR","lift","randomForest","glmnet","MASS",
               "e1071","partykit","rpart","ROCR","lift","randomForest","xgboost","tensorflow", "keras")
#Other Packages:
pacman::p_load("sqldf","readxl","geosphere")

#import datasets
sales <- read.csv("Queen's MMA\\MMA 831\\Week 2\\adv_sales.csv", header=TRUE, sep = ",")

#preview
str(sales)

summary(sales)
  #observations: no missing data

#----- EDA -----

#scatterplot of in-store advertising and sales
ggplot(sales) +
 aes(x = store, y = sales) +
 geom_point(size = 1L, colour = "#0c4c8a") +
 geom_smooth(span = 0.75) +
 theme_minimal()

#scatterplot of billboard advertising and sales
ggplot(sales) +
  aes(x = billboard, y = sales) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()

#scatterplot of printout advertising and sales
ggplot(sales) +
  aes(x = printout, y = sales) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: printout advertising doesn't appear to be associated with an increase in sales

#scatterplot of satisfaction level and sales
ggplot(sales) +
  aes(x = sat, y = sales) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()

#scatterplot of competitor ad spend and sales
ggplot(sales) +
  aes(x = comp, y = sales) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: maybe a slight negative relationship although doesn't appear too significant

#scatterplot of price and sales
ggplot(sales) +
  aes(x = price, y = sales) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  geom_smooth(span = 0.75) +
  theme_minimal()
  #observations: pretty clear negative relationship

#----- TRAIN/TEST SPLIT -----

#split sales data into train and test sets 
sample <- sample.int(n = nrow(sales), size = floor(0.75*nrow(sales)), replace = F)
sales_train <- sales[sample, ]
sales_test <- sales[-sample, ]

#----- LINEAR REGRESSION MODELS -----

#price only
mod1 <- lm(sales ~ price, sales_train)

summary(mod1)

mod1t <- lm(sales ~ price, sales_test)

summary(mod1t)

sales_test$pred <- predict(mod1, sales_test)

RMSLE(sales_test$pred, sales_test$sales)
  #observations: R^2 = 0.065 and RMSLE = 0.218

#price and store
mod2 <- lm(sales ~ price + store, sales_train)

summary(mod2)

mod2t <- lm(sales ~ price + store, sales_test)

summary(mod2t)

sales_test$pred <- predict(mod2, sales_test)

RMSLE(sales_test$pred, sales_test$sales)
  #observations: R^2 = 0.329 and RMSLE = 0.187

#price, store, and billboard

mod3 <- lm(sales ~ price + store + billboard, sales_train)

summary(mod3)

mod3t <- lm(sales ~ price + store + billboard, sales_test)

summary(mod3t)

sales_test$pred <- predict(mod3, sales_test)

RMSLE(sales_test$pred, sales_test$sales)
  #observations: R^2 = 0.842 and RMSLE = 0.087

#price, store, billboard, and printout

mod4 <- lm(sales ~ price + store + billboard + printout, sales_train)

summary(mod4)

mod4t <- lm(sales ~ price + store + billboard + printout, sales_test)

summary(mod4t)

sales_test$pred <- predict(mod4, sales_test)

RMSLE(sales_test$pred, sales_test$sales)
  #observations: R^2 = 0.842 and RMSLE = 0.087...basically no difference with printout (makes sense given our finding in EDA)

#price, store, billboard, printout, and satisfaction

mod5 <- lm(sales ~ price + store + billboard + printout + sat, sales_train)

summary(mod5)

mod5t <- lm(sales ~ price + store + billboard + printout + sat, sales_test)

summary(mod5t)

sales_test$pred <- predict(mod5, sales_test)

RMSLE(sales_test$pred, sales_test$sales)
  #observations: R^2 = 0.912 and RMSLE = 0.064

#price, store, billboard, printout, satisfaction, and competitor ad spend

mod6 <- lm(sales ~ price + store + billboard + printout + sat + comp, sales_train)

summary(mod6)

mod6t <- lm(sales ~ price + store + billboard + printout + sat + comp, sales_test)

summary(mod6t)

sales_test$pred <- predict(mod6, sales_test)

RMSLE(sales_test$pred, sales_test$sales)
  #observations: R^2 = 0.912 and RMSLE = 0.062...very small improvement here

#add in interactions (3 pairwise interactions for ad channels)

mod7 <- lm(sales ~ price*store + billboard*store + printout + sat*price + comp, sales_train)

summary(mod7)

mod7t <- lm(sales ~ price*store + billboard*store + printout + sat*price + comp, sales_test)

summary(mod7t)

sales_test$pred <- predict(mod7, sales_test)

RMSLE(sales_test$pred, sales_test$sales)


