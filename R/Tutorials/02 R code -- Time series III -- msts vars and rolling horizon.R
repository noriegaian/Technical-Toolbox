library(forecast)

WFdata <- read.csv("Queen's MMA\\MMA 867\\Lecture 2\\02 Excel data -- Solar Output and Power Consumption.csv", header=TRUE, sep = ",")
#load the data

str(WFdata) #check the structure of the data

# fix incorrectly classified data types
WFdata$DOW <- as.factor(WFdata$DOW)
WFdata$X15min_interval <- as.factor(WFdata$X15min_interval)

summary(WFdata) #examine the descriptive statistics

#define multiple-seasonality time series (time of day (15mins) and day of week)
WFdemand_msts <- msts (WFdata$Electricity.Demand.for.the.Branch..kw., seasonal.periods=c(96,672)) 
#####
##### TBATS
#####

WFdemand_tbats <- tbats(WFdemand_msts)
plot(WFdemand_tbats) #plot decomposition
WFdemand_tbats_pred <- forecast(WFdemand_tbats, h=1344, level=c(0.8, 0.95)) #predict 2 weeks out (672 15min intervals x 2)
plot(WFdemand_tbats_pred, xlab="Time", ylab="Predicted Electricity Demand, kW")

#####
##### Exponential smoothing 
#####

WFdemand_AAN <- ets(WFdemand_msts, model="AAN") #AAA cannot handle this, "Error in ets(WFdemand_msts, model = "AAA") : Frequency too high"
plot(WFdemand_AAN)
WFdemand_AAN_pred <- forecast(WFdemand_AAN, h=1344, level=c(0.8, 0.95))
plot(WFdemand_AAN_pred, xlab="Time", ylab="Predicted Electricity Demand, kW")
  #Comments: This is not good, as we can see the ETS model captures none of the seasonality

#WFdemand_MMN <- ets(WFdemand_msts, model="MMN") #MZZ cannot handle this, "Inappropriate model for data with negative or zero values"

#####
##### A "plain vanilla" ARIMA
#####

WFdemand_arima <- auto.arima(WFdemand_msts,seasonal=TRUE)
WFdemand_arima_pred <- forecast(WFdemand_arima, h=1344, level=c(0.8, 0.95))
plot(WFdemand_arima_pred, xlab="Time", ylab="Predicted Electricity Demand, kW")
  #Comments: Similar to ETS approach, regular ARIMA fails to capture any of the necessary seasonality

#####
##### ARIMA with regressors
#####

#create dummies for each day-of-week
weekdayMatrix <- cbind(Weekday=model.matrix(~as.factor(WFdata$DOW)))
#remove "intercept" (7th day) dummy
weekdayMatrix <- weekdayMatrix[,-1]
colnames(weekdayMatrix) <- c("Mon","Tue","Wed","Thu","Fri","Sat") # Rename columns

matrix_of_regressors <- weekdayMatrix

#This code adds in time of day regressors (onto the previously completed day of week regressors)
#X15minMatrix <- cbind(X15min=model.matrix(~as.factor(WFdata$X15min_interval)))
#X15minMatrix <- X15minMatrix[,-1]
#matrix_of_regressors <- cbind(weekdayMatrix,X15minMatrix)

WFdemand_arima <- auto.arima(WFdata$Electricity.Demand.for.the.Branch..kw., xreg=matrix_of_regressors) # Train a model 
WFdemand_arima # See what it is

xreg.pred<-matrix_of_regressors[-c(1345:5664),] # Build a 2-weeks-out prediction matrix

WFdemand_arima_pred <- forecast(WFdemand_arima, h=1344, xreg = xreg.pred, level=c(0.8, 0.95))
plot(WFdemand_arima_pred, xlab="Time", ylab="Predicted Electricity Demand, kW", ylim=c(0,20))

#####
##### ARIMA on residuals
#####

WFlm_msts <- tslm(WFdemand_msts ~ trend + season) # Build a linear model for trend and seasonality
summary(WFlm_msts)

residarima1 <- auto.arima(WFlm_msts$residuals) # Build ARIMA on it's residuals
residarima1
residualsArimaForecast <- forecast(residarima1, h=1344) #forecast from ARIMA
residualsF <- as.numeric(residualsArimaForecast$mean)

regressionForecast <- forecast(WFlm_msts,h=1344) #forecast from lm
regressionF <- as.numeric(regressionForecast$mean)

forecastR <- regressionF+residualsF # Total prediction
print(forecastR)
for (i in 1:1344){points(i+5665,forecastR[i],col="red",pch=19, cex=0.5)}

#compare with TBATS
plot(WFdemand_tbats_pred, xlab="Time", ylab="Predicted Electricity Demand, kW")
for (i in 1:1344){points((i+5665+672)/(7*24*4),forecastR[i],col="red",pch=19, cex=0.5)}

#####
##### Rolling-horizon holdout: TBATS
##### 

accuracy.tbats=0 # we will check average 1-day-out accuracy for 7 days
for (i in 1:7)
{ 
nTest <- 96*i  
nTrain <- length(WFdemand_msts)- nTest - 1
train <- window(WFdemand_msts, start=1, end=1+(nTrain)/(7*24*4))
test <- window(WFdemand_msts, start=1+(nTrain+1)/(7*24*4), end=1+(nTrain+96)/(7*24*4))

s <- tbats(train)
sp<- predict(s,h=96)

cat("----------------------------------
    
    Data Partition",i,"
    
    Training Set includes",nTrain," time periods. Observations 1 to", nTrain, "
    Test Set includes 96 time periods. Observations", nTrain+1, "to", nTrain+96,"
    
    ")
print(accuracy(sp,test))

accuracy.tbats<-rbind(accuracy.tbats,accuracy(sp,test)[2,5])

#print(sp$model)
}
accuracy.tbats<-accuracy.tbats[-1]

#####
##### Rolling-horizon holdout: ARIMA on residuals
##### 

accuracy.arima=0 # we will check average 1-day-out accuracy for 7 days
for (i in 1:7)
{ 
  nTest <- 96*i  
  nTrain <- length(WFdemand_msts)- nTest -1
  train <- window(WFdemand_msts, start=1, end=1+(nTrain)/(7*24*4))
  test <- window(WFdemand_msts, start=1+(nTrain+1)/(7*24*4), end=1+(nTrain+96)/(7*24*4))
  
  trainlm <- tslm(train ~ trend + season)
  trainlmf <- forecast(trainlm,h=96)
  
  residauto <- auto.arima(trainlm$residuals)
  residf <- forecast(residauto,h=96)
  
  y <- as.numeric(trainlmf$mean)
  x <- as.numeric(residf$mean)
  sp <- x+y
  
  cat("----------------------------------
      
      Data Partition",i,"
      
      Training Set includes",nTrain," time periods. Observations 1 to", nTrain, "
      Test Set includes 96 time periods. Observations", nTrain+1, "to", nTrain+96,"
      
      ")
  
  print(accuracy(sp,test))
  #  print(residauto)
  
  accuracy.arima<-rbind(accuracy.arima,accuracy(sp,test)[1,5])
  
  #print(sp$model)
}
accuracy.arima<-accuracy.arima[-1]

#compare mean accuracies of the rolling holdout
mean(accuracy.tbats)
mean(accuracy.arima)

sd(accuracy.tbats)
sd(accuracy.arima)
  #Observations: ARIMA has both the lower mean and SD...it is the better TS model choice for this specific task

#####
##### Correlated time-series -- Vectorized Auto-regressions (VARS)
#####

#install.packages("vars") #install.packages("strucchange")
library(vars) # Load package

#check correlation between demand and output
cor(WFdata[2],WFdata[3])
  #Observations: Negative relationship and not very high (-0.36)

series <- ts(cbind(WFdata[2],WFdata[3]))
plot(series)

# Estimate and summarize the model
model.VAR <- VAR(series,96,type="none") 
summary(model.VAR)

# Impulse responses (if one series changes, what happens to the other?)
impulse.response <- irf(model.VAR,impulse="Solar.System.Output..kWh.",response="Electricity.Demand.for.the.Branch..kw.",n.ahead = 96,ortho = FALSE, cumulative = FALSE)
plot(impulse.response)

# Predict next week and plot the results
predicted.values.VAR<-predict(model.VAR, n.ahead=672,ci=0.8)
plot(predicted.values.VAR,xlim=c(5000,6500))

  

