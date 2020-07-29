#Purpose: 

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(sqldf)
library(readxl)
library(car)
library(estimatr)
library(caret)
library(janitor)
library(glmnet)

#import dataset
diamonds <- read_excel("Queen's MMA\\MMA 867\\Lecture 1\\Sarah Gets a Diamond.xlsx", 2)

#split into test and train sets
diamonds_train <- diamonds %>%
  filter(ID <= 6000)

diamonds_test <- diamonds %>%
  filter(ID > 6000)

#test linear regression model
lin_mod <- lm(Price ~ `Carat Weight`, diamonds_train)

summary(lin_mod)

#test log-linear regression model
loglin_mod <- lm(log(Price) ~ `Carat Weight`, diamonds_train)

summary(loglin_mod)
  #Comments: R^2 and t-stat has increased (this seems to be an improved model)

#test log-log regression model
loglog_mod <- lm(log(Price) ~ log(`Carat Weight`), diamonds_train)

summary(loglog_mod)
  #Comments: Again, we see improvements

#OPTION A: 70/30% split between the 6000 DONE RANDOMLY
sample <- sample.int(n = nrow(diamonds_train), size = floor(0.7*nrow(diamonds_train)), replace = F)
train <- diamonds_train[sample, ]
test <- diamonds_train[-sample, ]

#OPTION B: split sequentially (following lecture video)
test<-subset(diamonds_train, (ID>=5001 & ID<=6000)) #withold 1000 datapoints into a "testing" data
train<-subset(diamonds_train, ID<=5000) #redefine the training data

#change data types
train$Cut <- as.factor(train$Cut)
train$Color <- as.factor(train$Color)
train$Clarity <- as.factor(train$Clarity)
train$Polish <- as.factor(train$Polish)
train$Symmetry <- as.factor(train$Symmetry)
train$Report <- as.factor(train$Report)

#test model
mod1 <- lm(log(Price) ~ log(`Carat Weight`) + Cut + Color + Clarity + Polish + Symmetry + Report, train)

summary(mod1)

  #test
pred <- predict(mod1, test)

#can add to test dataset as a variable
test$pred <- predict(mod1,test)

test <- test %>%
  mutate(pred = exp(pred))

#apply to truely unknown diamonds_test
pred2 <- predict(mod1, diamonds_test)

diamonds_test <- diamonds_test %>%
  mutate(pred = exp(pred2))

#export prediction results
write.csv(diamonds_test, file = paste0("Queen's MMA\\MMA 867\\Lecture 1\\Sarah Gets a Diamond Predictions.csv"), row.names = FALSE, na = "")

#Evaluate model accuracy with MAPE
percent.errors <- abs((test$Price-test$pred)/test$Price)*100

mean(percent.errors) #would typically have multiple models to compare MAPE for (want the model with the lowest)
  #Observation: MAPE = 7.74

#Improved model using interactions
mod2 <- lm(log(Price) ~ log(`Carat Weight`)*Color + Cut + Color + Clarity + Polish + Symmetry + Report, train)
summary(mod2)

test$pred <- predict(mod2,test)
test <- test %>%
  mutate(pred = exp(pred))

percent.errors2 <- abs((test$Price-test$pred)/test$Price)*100
mean(percent.errors2)
  #Comments: MAPE is 7.28 (lower and improved)

#Improved model more interactions
mod3 <- lm(log(Price) ~ log(`Carat Weight`)*Color*Cut + Cut*Color*Clarity + Cut*Color*Clarity*Polish + Symmetry 
           + Report, train)
summary(mod3)

test$pred <- predict(mod3,test)
test <- test %>%
  mutate(pred = exp(pred))

percent.errors3 <- abs((test$Price-test$pred)/test$Price)*100
mean(percent.errors3)

  #Comments: MAPE is 6.31 (lower and improved)

#Improved model using stepwise approach (backwards)
mod4 <- step(lm(log(Price) ~ log(`Carat Weight`)*Color*Cut + Cut*Color*Clarity + Cut*Color*Clarity*Polish + Symmetry 
                + Report, train), direction = "backward")
summary(mod4)

test$pred <- predict(mod4,test)
test <- test %>%
  mutate(pred = exp(pred))

percent.errors4 <- abs((test$Price-test$pred)/test$Price)*100
mean(percent.errors4)

  #Comments: MAPE is 6.00 (lower and improved)

#Try forward stepwise model
mod5 <- step(lm(log(Price) ~ log(`Carat Weight`)*Color*Cut + Cut*Color*Clarity + Cut*Color*Clarity*Polish + Symmetry
                + Report, train))
summary(mod5)

test$pred <- predict(mod5,test)
test <- test %>%
  mutate(pred = exp(pred))

percent.errors5 <- abs((test$Price - test$pred)/test$Price)*100
mean(percent.errors5)

  #Comments: MAPE is 6.00 (same as using stepwise w/ backwords direction)

#Regularizations with glmnet package (will require a model matrix)
#create the y variable and matrix (capital X) of x variables (will make the code below easier to read + will ensure that all interactions exist)
y<-log(train$Price)

#create a "dumb" model with random interactions for the matrix (first with reg carat weight, then sqrt of carat weight, then log of carat weight, then random interactions between categoricals)
#regressed on ID instead of price, since we don't have price for the full data
#we don't care that the model structure doesn't make sense here as the purpose of this feature engineering is to create all hypothetically necessary variable combinations
X<-model.matrix(ID~`Carat Weight`*Color*Cut+sqrt(`Carat Weight`)*Color*Cut+log(`Carat Weight`)*Color*Cut + Cut*Color*Clarity+Cut*Color*Clarity*Polish+Symmetry+Report, diamonds)[,-1]
X<-cbind(diamonds$ID,X)

# split X into testing, trainig/holdout and prediction as before
X.training<-subset(X,X[,1]<=5000)
X.testing<-subset(X, (X[,1]>=5001 & X[,1]<=6000))
X.prediction<-subset(X,X[,1]>=6001)

##LASSO (alpha = 1)
lasso.fit <- glmnet(x = X.training, y = y, alpha = 1)
plot(lasso.fit, xvar = "lambda")
  #Observations: As lambda gets larger, number of variables are decreasing (and coefficients are supressing)

#selecting the best penalty lambda
crossval <- cv.glmnet(x = X.training, y = y, alpha = 1) #create cross-validation data
plot(crossval)
penalty.lasso <- crossval$lambda.min #determine optimal penalty parameter, lambda
log(penalty.lasso) #see where it was on the graph
plot(crossval,xlim=c(-8,-6),ylim=c(0.006,0.008)) # lets zoom-in

#recreate the model applying our new optimal lambda penalty value
lasso.opt.fit <-glmnet(x = X.training, y = y, alpha = 1, lambda = penalty.lasso)
coef(lasso.opt.fit) #resultant model coefficients
  #Observations: Notice that many variables (of the 935 total created) are 0 (since lasso is only concentrating on the most important vars)

#apply the LASSO model for predictions and evaluate
lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.testing))
mean(abs(lasso.testing-test$Price)/test$Price*100)
  #Comments: We observe a MAPE = 6.32 (actually slightly worse than our last stepwise OLS model at 5.98)

##Ridge (alpha = 0)
ridge.fit <- glmnet(x = X.training, y = y, alpha = 0)
plot(ridge.fit, xvar = "lambda")
  #Observations: As lambda gets larger, the coefficient magnitude decreases (same number of coefficients, however)

#selecting the best penalty lambda
crossval2 <- cv.glmnet(x = X.training, y = y, alpha = 0) #create cross-validation data
plot(crossval2)
penalty.ridge <- crossval2$lambda.min #determine optimal penalty parameter, lambda
log(penalty.ridge) #see where it was on the graph
plot(crossval,xlim=c(-3,-2),ylim=c(0.006,0.008)) # lets zoom-in

#recreate the model applying our new optimal lambda penalty value w/ Ridge
ridge.opt.fit <-glmnet(x = X.training, y = y, alpha = 0, lambda = penalty.ridge)
coef(ridge.opt.fit) #resultant model coefficients

#apply the Ridge model for predictions and evaluate
ridge.testing <- exp(predict(ridge.opt.fit, s = penalty.ridge, newx = X.testing))
mean(abs(ridge.testing-test$Price)/test$Price*100)
  #Comments: We observe a MAPE = 7.25 (not as good of an improvement compared to LASSO here)

#FINAL OBSERVATION: In this context we can see that concentrating the weights on some variables
#(and throwing the rest out) is more beneficial that redistributing the weight upon all variables.

#to write out predictions (use more optimal LASSO model)
predicted.prices.log.i.lasso <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.prediction))
write.csv(predicted.prices.log.i.lasso, file = paste0("Queen's MMA\\MMA 867\\Lecture 1\\Sarah Gets a Diamond Log Interaction LASSO Predictions.csv"), row.names = FALSE, na = "")