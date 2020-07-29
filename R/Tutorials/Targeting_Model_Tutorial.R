### MMA831 - Marketing Analytics
### Tutorial: Propensity model
### Dataset Source: UCI ML Repository
### Dataset Link: https://archive.ics.uci.edu/ml/datasets/bank+marketing
### Overview: Dataset contains historical campaign response data for a Portuguese bank based on outbound telemarketing campaigns.
             # Often, more than one contact to the same client was required, in order to access if the product (bank term deposit) would be ('yes') or not ('no') subscribed.


### Overview of packages used in this Tutorial:

#caret:
  # Package that help streamline model training; this of this as similar to sklearn in python.
  # Contains many functions for data splitting, preprocessing, feature selection, model training/runing, and variable importance estimate

#Tidyverse:
  # is a collection of R packages for preparing, wrangling and visualizing data
  # things you'll need for almost any analytics or ML project
  # it includes ggplot (for visualization), dply (for data manipulation),
               #tidyr (for data cleaning), readr (for data ingestion),tibble (an alternative to dataframes)

#fastDummies
  # although many other packages can do this, fastDummies makes it easy to quicly 1-hot encode categorial variables

#DataExplorer
  # provides quick & easy way to explore a dataset
  # alternative to using str() and summary()


########################################################################
##### STEP 1: LOADING DATASET
########################################################################
#install.packages("tidyverse")
#install.packages("DataExplorer")

library(tidyverse)
library(DataExplorer)

df <- read.table("/Users/mm679j/Documents/R_Tutorial/TargetingModel/bank/bank-full.csv", sep=";", header=T)
small <- read.table("/Users/mm679j/Documents/R_Tutorial/TargetingModel/bank/bank.csv", sep=";", header=T)


########################################################################
##### STEP 2: DATA EXPLORATION / VISUALIZATION
########################################################################
glimpse(df)
glimpse(small)

#explore discrete features
plot_bar(df$job)

#plot(df)


########################################################################
##### STEP 3: ONE HOT ENCODE
########################################################################
#install.packages("fastDummies")
library(fastDummies)
dummy_df <- fastDummies::dummy_cols(df,remove_first_dummy = TRUE)

#see how it looks
glimpse(dummy_df)

#keep only columns we'll use to train the model
#identify list of numeric columns as TRUE/FALSE
num_cols = unlist(lapply(dummy_df, is.numeric))

#subset
final_df = dummy_df[ , num_cols]

glimpse(final_df)

########################################################################
##### STEP 3: MODEL TRAINING
########################################################################
#install.packages("caret")
library(caret)

#test/train split

#create and index that will split dataset based on the labels (70% train, 30% test)
index = createDataPartition(final_df$y_yes, p=0.7, list=FALSE)

#subset test & train using index above
str(final_df)
X_train <- final_df[, 1:42]
y_train <-  as.factor(final_df[, 43])

X_test <- final_df[, 1:42]
y_test <- as.factor(final_df[, 43])


#train a classifiers
model_classy <- train(X_train, y_train, method='LogitBoost',preProcess=c("center", "scale"))

#to see all types of ML algorithims in caret
names(getModelInfo())


########################################################################
##### STEP 4: BASIC MODEL OUTPUTS
########################################################################
#see feature importantce
feature_importance <- varImp(model_classy, scale=FALSE)
plot(feature_importance)

#what is the #1 predictor? does that make sense?

#Predidct on test set & see performance
predictions<-predict(object=model_classy,X_test)
table(predictions)
confusionMatrix(predictions,y_test)

########################################################################
##### STEP 5: LIFE & GAINS CHARTS
########################################################################
lift <- function(depvar, predcol, groups=10) {
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)}
  if(is.factor(depvar)) depvar <- as.integer(as.character(depvar))
  if(is.factor(predcol)) predcol <- as.integer(as.character(predcol))
  helper = data.frame(cbind(depvar, predcol))
  helper[,"bucket"] = ntile(-helper[,"predcol"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(depvar), funs(total = n(),
                                    totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

decile_metrics = lift(y_test , predictions, groups = 10)
decile_metrics

#cumulative lift chart
graphics::plot(decile_metrics$bucket, decile_metrics$Cumlift, type="l", ylab="Cumulative lift", xlab="Decile")






