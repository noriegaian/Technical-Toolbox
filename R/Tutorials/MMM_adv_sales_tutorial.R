?# install packages needed for analysis
install.packages("car")
library(car) #compaion to applied regression
# install.packages("glmnet")
library(glmnet)
# install.packages("lmtest")
library("lmtest")

# Remove scientific notation
options(scipen=999)

# Clear environment
rm(list=ls())

#import data
sales <- read.csv('/Users/mm679j/Documents/R_Tutorial/MMM_adv_sales/adv_sales.csv')

#Understanding the data
summary(sales)
str(sales)
head(sales)
tail(sales)

#looking at the correleation between the interaction variables and sales
library(gpairs)
gpairs(sales)

corrplot.mixed(cor(sales[ , c(2:8)]), upper="ellipse") #looking at if any interaction variables are highly correlated - they are not


#looking at the distribution of all interaction variables to determine if any transformation is needed 
par(mfrow=c(3,3))# all look normally dist. good
hist(sales$store)
hist(sales$billboard) 
hist(sales$printout)
hist(sales$sat)
hist(sales$comp)
hist(sales$price)

#check for missing data
library(mice)
md.pattern(sales) #no missing data

# Add pairwise channel interaction variables, mean-centered
sales$int.store.billboard <- (sales$store-mean(sales$store))*(sales$billboard-mean(sales$billboard))
sales$int.store.printout <- (sales$store-mean(sales$store))*(sales$printout-mean(sales$printout))
sales$int.billboard.printout <- (sales$billboard-mean(sales$billboard))*(sales$printout-mean(sales$printout))

glimpse(sales)

# Split dataset into Train (75%) and Test (25%)
set.seed(7525)
training <- sample(1:nrow(sales),floor(0.75*nrow(sales)))
# Create two separate data frames
train <- sales[training,]
test <- sales[-training,]

# create data frame for regression model results (8 models in total)
models <- data.frame(formula = rep(NA,8), R2.train = NA, R2.test = NA)

# list of models to use
models$formula <- c("sales ~ price",
                    "sales ~ price+store",
                    "sales ~ price+store+billboard",
                    "sales ~ price+store+billboard+printout",
                    "sales ~ price+store+billboard+printout+sat",
                    "sales ~ price+store+billboard+printout+sat+comp",
                    "sales ~ price+store+billboard+printout+sat+comp+int.store.billboard+int.store.printout+int.billboard.printout",
                    "sales ~ price+store+billboard+sat+comp+int.store.billboard")

# Build models reporting R-square for train and test datasets
for (i in 1:nrow(models))
{
   LM <- lm(models$formula[i], data=train)
   print(summary(LM))
   models$R2.train[i] <- round((1 - sum((predict(LM,train)-train$sales)^2) / sum((train$sales-mean(train$sales))^2))*100, digits=2) # train dataset R-square
   if (i>1) {print(vif(LM))}
   models$R2.test[i] <- round((1 - sum((predict(LM,test)-test$sales)^2) / sum((test$sales-mean(test$sales))^2))*100, digits=2) # test dataset R-square
}

# Show model results
models

# Comparing two models to see if there is significant improvement (p<0.05 indicate improvement in the model)
anova(lm(models$formula[6], data=train), lm(models$formula[7], data=train))

# Code to explore the best model
i <- 8 # choosing model #8
LM <- lm(models$formula[i], data=train)
summary(LM)
vif(LM)   # examine VIF values to test for multicollinearity 
anova(LM) # residuals mean square (MSE) = 960310
# calculating RMSE as square root of MSE and express as proportion of average sales
RMSE <- sqrt(960310) / mean(sales$sales)
RMSE

?vif

# Perform model diagnostics - ensure meets linear regression model criteria
par(mfrow=c(2,2)) # 4 diagnostic charts in 1 panel
plot(LM)

# Histogram and QQ plot for residuals
par(mfrow=c(1,2))
hist(resid(LM)) # Histogram 
qqnorm(resid(LM)) # Quantile normal plot - good for checking normality

# Breusch Pagan Test for heteroskedasticity (p > 0.05 indicate no problems = homoskedastic)
bptest(LM)

