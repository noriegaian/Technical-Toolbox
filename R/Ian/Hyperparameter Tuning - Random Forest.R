#Purpose: Exploring hyperparameter tuning (grid search and random search) with a Random Forest model

#Cleansing/Manipulation Packages:
pacman::p_load("dplyr","tidyr","stringr","lubridate","janitor")
#Graphing Packages:
pacman::p_load("ggplot2","esquisse","gridExtra")
#ML Packages:
pacman::p_load("car","MLmetrics","estimatr","fpp","forecast","caret","ROCR","lift","randomForest","glmnet","MASS",
               "e1071","partykit","rpart","ROCR","lift","randomForest","xgboost","tensorflow", "keras")
#Other Packages:
pacman::p_load("sqldf","readxl","geosphere")

#----- DATA IMPORT -----

##import datasets
library(mlbench)
data(Sonar) #imports dataset from the library
dataset <- Sonar
x <- dataset[,1:60]
y <- dataset[,61] #note we have two levels - M and R

#----- GENERAL MODEL WITH DEFAULT HYPERPARAMETERS -----

##creating a random forest model, first with default hyperparameters (note: could also use RandomForest library)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 1024
metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt(ncol(x)) #it is pretty standard to initially set mtry to the square root of #features
tunegrid <- expand.grid(.mtry=mtry)

#now the random forest model
rf_default <- caret::train(Class~., data=dataset, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
rf_default #see estimated results
  #observations: Resampling = 10-Fold CV x 3; Accuracy = 0.844, Kappa = 0.685
  #Note that tuning parameter 'mtry' was held constant at our default value (7.74)

#----- TUNING WITH RANDOM SEARCH -----

##now let's try tuning using random search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(seed)
mtry <- sqrt(ncol(x))

rf_random <- caret::train(Class~., data=dataset, method="rf", metric=metric, tuneLength=15, trControl=control)
#notice we've set tuneLength to 15 (this will cap the total combinations attempted to 15)
rf_random
#observations: we see that the random search has tried 15 different mtry values (randomly) ranging from 7-57...mtry
#of 11 resulted in the highest accuracy; also observe this visually in the plot below; Accuracy = 0.854
plot(rf_random)

#----- TUNING WITH GRID SEARCH -----
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(seed)
tunegrid <- expand.grid(.mtry=c(1:15)) #here we are providing a set of mtry values (1-15) to evaluate

rf_gridsearch <- caret::train(Class~., data=dataset, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
rf_gridsearch
#observations: observe that grid search has evaluated the values we've asked for...here we see that mtry = 3
#resulted in the highest accuracy = 0.859
plot(rf_gridsearch)

#----- TUNING ANOTHER HYPERPARAMETER -----

#we've tuned mtry...now let's look at ntree (while holding mtry constant)
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=c(sqrt(ncol(x))))
modellist <- list()
for (ntree in c(1000, 1500, 2000, 2500)) { #list out values to test for ntree here
  set.seed(seed)
  fit <- caret::train(Class~., data=dataset, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
results <- resamples(modellist)
summary(results)
#observations: ntree = 2000 results in the highest accuracy; could also mean an optimum exists between 2000 and 2500
dotplot(results) #not all that useful

#----- TUNING MULTIPLE HYPERPARAMETERS -----

#create custom random forest algorithm for use with caret that will tune both mtry and ntree hyperparameters
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes


control <- trainControl(method="repeatedcv", number=10, repeats=3) #same settings here as above
tunegrid <- expand.grid(.mtry=c(2:11), .ntree=c(1500, 2000)) #specify search values for both
set.seed(seed)
custom <- caret::train(Class~., data=dataset, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
custom
#observations: here the combo of mtry = 4 and ntree = 2000 maximizes our accuracy
plot(custom)



