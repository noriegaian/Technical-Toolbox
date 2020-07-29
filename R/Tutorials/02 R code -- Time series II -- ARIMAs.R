#install.packages("forecast") #-- do this only once 
#install.packages("fpp") #-- do this only once 
#These packages are from the "Forecasting Principles and Practice" - excellent and free book: https://www.otexts.org/fpp
library(forecast)
library(fpp)

# DIFFERENCING and ARIMA

# we will use a10 dataset (comes with fpp - sales of antidiabetic drug in Australia)
par(mfrow=c(1,3))
View(a10)
plot(a10, xlab="Year",
     ylab="A10 sales")
plot(log(a10), xlab="Year",
     ylab="log A10 sales")
#takes this year's december vs last year's equiv
plot(diff(log(a10),12), xlab="Year",
     ylab="Annual change in monthly log A10 sales")

fit <- stl(log(a10), t.window=12, s.window="periodic", robust=TRUE)
plot(fit)
#Observations: very clear seasonality and upwards trend

# auto-correlation function
Acf(a10,main="") # data "as is"
Acf(log(a10),main="") # log-transformed data
Acf(diff(log(a10),12),main="") # difference-12 log data
#Observations: The autocorrelations for the differences 

# partial auto-correlation function
par(mfrow=c(1,2))
Acf(diff(log(a10),12),main="")
Pacf(diff(log(a10),12),main="") 

fit <- auto.arima(a10,seasonal=FALSE)
fit
#Observations: ARIMA(1,1,1); AIC is a form of regularization and is essentially error + the # of variables

fit <- auto.arima(a10,seasonal=TRUE)
fit

par(mfrow=c(1,1))
Acf(residuals(fit))
plot(forecast(fit,60)) #60 stands for 60 months = 5 years

# ARIMA with regressors ("dynamic regression")

# with insurance and advertising data (also part of FPP)
plot(insurance, main="Insurance advertising and quotations", xlab="Year")
View(insurance)

# Lagged predictors. Test 0, 1, 2 or 3 lags.
  #first take only ad column and manually create period lags
Advert <- cbind(insurance[,2],
                c(NA,insurance[1:39,2]),
                c(NA,NA,insurance[1:38,2]),
                c(NA,NA,NA,insurance[1:37,2]))
colnames(Advert) <- paste("AdLag",0:3,sep="")
Advert
#Comments: A matrix of regressors has been created
#these can be used to train the model

# Choose optimal lag length for advertising based on AIC
# Restrict data so models use same fitting period
fit1 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1], d=0) #only direct effect
fit2 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:2], d=0) #includes direct effect + 1 month carryover
fit3 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:3], d=0) # " + 2 month carryover
fit4 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:4], d=0) # " + 3 month carryover

AIC(fit1)
AIC(fit2)
AIC(fit3)
AIC(fit4)
#Comments: note that we can only compare AIC's with common d = 0
  #if this changes, we are comparing apples to oranges

#Best fit (as per AIC) is with all data (1:2), so the final model becomes
fit <- auto.arima(insurance[,1], xreg=Advert[,1:2], d=0)
fit

# forecast insurance quotes with advertising = 10
fc10 <- forecast(fit, xreg=cbind(rep(10,20),c(Advert[40,1],rep(10,19))), h=20)
plot(fc10, main="Forecast quotes with advertising set to 10", ylab="Quotes")

# see how forecasts with advertising = 8 will differ from advertising = 2
par(mfrow=c(1,2))
fc8 <- forecast(fit, xreg=cbind(rep(8,20),c(Advert[40,1],rep(8,19))), h=20)
plot(fc8, main="Forecast quotes with advertising set to 8", ylab="Quotes")

fc2 <- forecast(fit, xreg=cbind(rep(2,20),c(Advert[40,1],rep(2,19))), h=20)
plot(fc2, main="Forecast quotes with advertising set to 2", ylab="Quotes")

