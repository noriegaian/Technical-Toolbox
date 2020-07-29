#uplift model

################################################################################
#use uplift package
#install.packages("uplift")
library(uplift)
library(dplyr)

url = "http://www.minethatdata.com/Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv"
hc = read.csv(file=url)
summary(hc)
str(hc)
glimpse(hc)

#Go here to see descp of features:
# https://blog.minethatdata.com/2008/03/minethatdata-e-mail-analytics-and-data.html
#Segment describes whether the register corresponds to either any of the two campaigns, or the control group that has not been target.
#Let’s create a new column to indicate if the register corresponds to the control or not, and convert num featurs to factors (cat) to factors some features with logical values 0/1.

################################################################################
# Simple data clean up / transofmrations
hc$treat <- ifelse(as.character(hc$segment) != "No E-Mail", TRUE, FALSE) #create treatment / no treatment flag
hc$mens <- as.factor(hc$mens) #convert to factor
hc$womens <- as.factor(hc$womens) #convert to factor
hc$newbie <- as.factor(hc$newbie) #convert to factor
 

#learn about a package
??uplift

#check a/b/c splits (roughly 1/3rd each)
table(hc$segment)
table(hc$treat)


################################################################################
################################################################################
#The package uplift contains several useful functions for uplift modeling;
# Recall Transformed Outcome Approach from last week - rvtu func transforms the dataset to create a new
# variable that will represent, when fitted by a classifier, the incremental probability of conversion
#(i.e. the probability that the customer does convert when they have been targeted, minus the probability that the customer does convert when they have not been targeted)

#to learn more about response variable transformation read help;
#?rvtu;

hc_rvtu = rvtu(visit~recency+history_segment+history+mens+womens+zip_code+newbie+channel+trt(as.numeric(treat)),
                data=hc[hc$segment != "Mens E-Mail",],
                method="none")
names(hc)
names(hc_rvtu)
summary(hc_rvtu)
#notice new columns y, z, and ct instead of treat

glimpse(hc)
glimpse(hc_rvtu)
#understand packages transformation steps
#y is the original target, i.e. the conversion (the original column was visit) - we can use this for the baseline propensity model
#z is the transformed target for the uplift classifier, used to estimate the incremental probability of conversion
#ct same as treat col before, ct=1 if contacted/treated, 0 is not contracted/treated (i.e. control)

#some of the EDA (exploratory analysis) functionality of this package -- where is uplift coming from, diagnostics etc.
#be careful about over-doing this, starts to get into p-hacking territory
explore(y~recency+history_segment+history+mens+womens+zip_code+newbie+channel+trt(ct),
        data=hc_rvtu)
#overall uplift by various cuts for those treated vs. not

################################################################################
################################################################################
#visualization
#; Plots comparing the campaign vs. control based on 2 types of conv
#Here we look at impact of recency, but could do EDA on all of these...
par(mfrow=c(2,2))
boxplot(recency~visit, data=hc[hc$treat,],
        ylab="recency", xlab="visit")
boxplot(recency~conversion, data=hc[hc$treat,],
        ylab="recency", xlab="conversion")
mtext("recency target", side=3, line=-3, outer=TRUE, cex=2, font=2)
boxplot(recency~visit, data=hc[! hc$treat,],
        ylab="recency", xlab="visit")
boxplot(recency~conversion, data=hc[hc$treat,],
        ylab="recency", xlab="conversion")
mtext("recency control", side=3, line=-39, outer=TRUE, cex=2, font=2)

#looks the same b/w treated and control...suggest recency is not that help not that helful if target variable is either visits or conversion...

################################################################################
################################################################################
#fit classifier using logistic regression - simple, fast, easy to CV

logit.formula = ~recency+mens+womens+zip_code+newbie+channel
#a simple logisitc regression may be sufficient for traditional baseline prop model, but we'll most likely need more complex interaction effects to capture informational vaule to explain why some cust generate uplift & others dont

#The “traditional” classifier with try to estimate the probability of conversion (y as response) using the features of the formula.
# All the registers that have converted, regardless of the fact that they were targeted or not, will have y == 1, and they have exactly that in common: they have converted.#
#This is likely quite a good enough “signal” for fitting the model.

#However, the uplift classifier will try to estimate the incremental probability of conversion, using z as response.
#z is 1 for the customers that were targeted and converted, and also for the customers that were not targeted and did not convert.
#Those customers will likely have less in common, less “signal”, compared to the “traditional” setup.


#need to add interaction effects often vs. a simple base line conv model, our outcome will be Z
#you will probably try many combinations, use business intuition etc, or most likely switch to a tree-based method
logit.formula.interactions = as.formula(paste("~history_segment*mens+history_segment*womens",
                                               "history_segment*newbie",
                                               "zip_code*mens+zip_code*womens+zip_code*newbie",
                                               "channel*mens+channel*womens+channel*newbie",
                                               "channel*history_segment",
                                               sep="+"))

#to illustrate the value of uplift models, we'll use CV to train both traditional classifier + uplift model & measure the right metric
set.seed(1024)
require(glmnet)
logit.x.interactions <- model.matrix(logit.formula.interactions, data=hc_rvtu)
logit.z <- hc_rvtu$z #transformed variable, what we use for uplift model
logit.y <- hc_rvtu$y #original variable = i.e. visits to site

# traditional classifier, y as response
logit.cv.lasso.y.interactions <- cv.glmnet(logit.x.interactions, logit.y, alpha=1, family="binomial")
plot(logit.cv.lasso.y.interactions)

# uplift classifier, z as response
logit.cv.lasso.z.interactions <- cv.glmnet(logit.x.interactions, logit.z, alpha=1, family="binomial")
plot(logit.cv.lasso.z.interactions)

################################################################################
################################################################################
#First compare models based on ROC using each model corresponding feature
#install.packages("ROCR")
library(ROCR)
preds.y.interactions.1se = predict(logit.cv.lasso.y.interactions, logit.x.interactions,
                                    s=logit.cv.lasso.y.interactions$lambda.1se, type="response")
pred.y <- prediction(preds.y.interactions.1se, hc_rvtu$y)
auc.y <- ROCR:::performance(pred.y, "auc")
as.numeric(auc.y@y.values)

#First, some numeric measures, basically the AUC; in a strictly technical assessment there are no values for costs and profits, and there is no way to determine how many customers to target. The model orders the customers by the probability of converting, and the decision to target them or not depends on a threshold for that probability.
preds.z.interactions.1se <- predict(logit.cv.lasso.z.interactions, logit.x.interactions,
                                    s=logit.cv.lasso.z.interactions$lambda.1se, type="response")
pred.z <- prediction(preds.z.interactions.1se, hc_rvtu$z)
auc.z <- ROCR:::performance(pred.z, "auc")

#compare AUCs
as.numeric(auc.y@y.values)
as.numeric(auc.z@y.values)
#looks like traditional classifier is better at predictor customer who'll visit...

#Does it mean that the “traditional” is better than the uplift? Possibly; however, for the uplift the AUC is not necessarily a good measure.
#used this guys functions to get Qini curves (need to run copy-pasted code below first)
#this function gets you a Qini curve
https://github.com/lrnzcig/sisifoR/blob/master/R/uplift.R

# uplift model - ratio of incr conv in target group as function of depth (area of uplift = 0.177)
qini_from_data(preds.z.interactions.1se,
               hc_rvtu,
               plotit=TRUE,
               x_axis_ratio=TRUE)

#--propensity model (area of uplift = 0.082)
qini_from_data(preds.y.interactions.1se,
               hc_rvtu,
               plotit=TRUE,
               x_axis_ratio=TRUE)

#The area under the Qini curve is a good way to compare different uplift classifiers
#As for the comparison between the “traditional” and the uplift classifier, they just correspond to different scenarios. If you plot a qini curve for the “traditional”, it performs worse than the uplift, and the opposite if you plot a cumulative response. You can try the code below.

#Run below to create qini_from_data() function
#######################################################################################
#######################################################################################
#source code for function#######
require(zoo)

#
# get qini value for all lambdas in s_range
# - model_object: fitted model to predict the data
# - newdata: data for the predictions
# - s_range: values for lambda
# - results_column_name: target; default value is ok when using uplift::rvtu
#
qini_for_lambda_range <- function(model_object,
                                  newdata,
                                  s_range,
                                  input_data,
                                  plotit=FALSE) {
  preds.z.interactions <- predict(model_object, newdata,
                                  s=s_range, type="response")
  if (length(s_range) > 1 & plotit == TRUE) {
    warning("Vector of lambdas provided, the qini curves will not be plotted")
    plotit=FALSE
  }
  qini_per_lambda <- sapply(1:(length(s_range)), function (index) {
    qini_from_data(preds.z.interactions[,index], input_data, plotit=plotit)
  })
  return(qini_per_lambda)
}

#
# get qini value and plot it from input data and vector of predictions
# - columns names by default are ok if using uplift::rvtu
# - ideal_order_column_name & treat_column_name are used for ordering the ideal response
# - input data is ordered by predictions only
# - results_column_name & treat_column_name are used to sum/penalize each conversion
# - by default does not plot
#
qini_from_data <- function(preds,
                           input_data,
                           ideal_order_column_name="z",
                           results_column_name="y",
                           treat_column_name="ct",
                           x_axis_ratio=FALSE,
                           y_axis_ratio=TRUE,
                           plotit=FALSE,
                           print_auc=TRUE,
                           main_title=NULL,
                           ylim=NULL) {
  # ideal
  ordered_data <- input_data[order(input_data[,ideal_order_column_name], input_data[,treat_column_name], decreasing=TRUE),]
  q.curve.ideal <- get_qini_curve_values(ordered_data, results_column_name, treat_column_name)
  
  if (q.curve.ideal[length(q.curve.ideal)] == 0) {
    warning("O conversions in the data. The function returns 0 but actually it should be 'unknown'")
    return(0)
  }
  
  ordered_data <- input_data[order(preds, decreasing=TRUE),]
  q.curve.real <- get_qini_curve_values(ordered_data, results_column_name, treat_column_name)
  
  if (length(q.curve.real) != length(q.curve.ideal)) {
    stop("Check that length of input data corresponds to length of predictions")
  }
  
  # area of the square with heigh equals to the the number of conversions
  square_area <- abs(length(q.curve.ideal)*q.curve.ideal[length(q.curve.ideal)])
  # Simpson's rule for area
  result <- (sum(diff(1:length(q.curve.real))*rollmean(q.curve.real, 2))-square_area/2)/square_area
  
  if (plotit == TRUE) {
    x_max <- length(q.curve.ideal)
    x_seq <- 0:x_max
    if (y_axis_ratio) {
      # times 2, assuming that the size of control & treatment groups are equal,
      # in order to scale the ratios to the size of the group, instead of the total
      q.curve.ideal <- q.curve.ideal * 2/ length(q.curve.ideal)
      q.curve.real <- q.curve.real * 2 / length(q.curve.ideal)
    }
    if (x_axis_ratio) {
      x_seq <- x_seq / length(q.curve.ideal)
      x_max <- x_max / length(q.curve.ideal)
    }
    xlab = paste("customers", ifelse(x_axis_ratio == TRUE, "(ratio)", "(total)"))
    ylab = paste("conversions", ifelse(y_axis_ratio == TRUE, "(ratio of customers in targeted group)", "(total)"))
    if (is.null(ylim)) {
      plot(x_seq, c(0, q.curve.ideal), type="l", col="black", xlab=xlab, ylab=ylab)
    } else {
      plot(x_seq, c(0, q.curve.ideal), type="l", col="black", xlab=xlab, ylab=ylab, ylim=ylim)
    }
    points(x_seq, c(0, q.curve.real), type="l", col="blue")
    lines(c(0,x_max), c(0, q.curve.ideal[length(q.curve.ideal)]), col="red")
    if (! is.null(main_title)) {
      title(main_title)
    }
    if (print_auc == TRUE) {
      legend("bottomright", paste("area of uplift", 
                                  round(result, 4), 
                                  "     "), cex=0.75)
    }
  }
  return(result)
}

# support function: calculates values of the curve, penalizing if control element converts
get_qini_curve_values <- function(ordered_data,
                                  results_column_name="y",
                                  treat_column_name="ct") {
  q.plus <- cumsum(ifelse(ordered_data[,treat_column_name] == 1 & ordered_data[,results_column_name] == 1, 1, 0))
  q.minus <- cumsum(ifelse(ordered_data[,treat_column_name] == 0 & ordered_data[,results_column_name] == 1, 1, 0))
  q.interactions <- q.plus - q.minus
  return(q.interactions)
}



#http://sisifospage.tech/2016-04-01-uplift-evaluation.html
