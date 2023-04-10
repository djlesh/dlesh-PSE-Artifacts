# Final Project - Apple (AAPL) Stock Price Prediction
# Date: 05/08/2021
# Course: ECON 3313 Spring 2021
# Author: Daniel Lesh

# Clear memory
rm(list = ls())

# Mac
setwd("/Users/daniellesh/Downloads/")

##Packages-----------------------------------
library(xts)
library(quantmod)
library(tseries)
library(forecast)
library(stats)
library(plotrix)
library(lmtest)
## -------------------------------------------


  ## Convert AAPL data to a  Time Series data set / Summary Stats
####--------------------------------------------
# Import Data - AAPL Data Set (2010.1 - 2020.12)
aapl = read.csv("AAPL copy.csv",header=TRUE)
aapl

# Rename Column 1 header 
aapl=na.omit(aapl) # Omit original column header 
names(aapl)[1] <- "Closing_Price" # Changing variable name to a shorter one

# Transform AAPL historical price data into a time series data set
tsaapl = ts(aapl[,"Closing_Price"],start=c(2010,1),frequency=12)
summary(tsaapl) # Summary stats for time series AAPL data
tsaapl # Show tsaapl

# Build time series plot for the data
par(mfrow=c(1,1)) # Set plot scale

# Time series plot for AAPL historical price data from 2010.1 to 2020.12
plot(tsaapl, main="AAPL Historical Price Data", ylab='Price')

# Statistic Characteristics for Linear Pricing Scale 
var(tsaapl) # 636.1989
sd(tsaapl)  # 25.22298
####--------------------------------------------


  ## Model Selection with ACF / PACFL and Statics Testing (Ljung-Box and Box-Pierce)
####--------------------------------------------
## ACF & PCF and Plots 
par(mfrow=c(1,1)) # Set plot scale
acf(tsaapl, main = "Autocorrelation of AAPL Closing Prices") # ACF of AAPL Closing Price
pacf(tsaapl, main = "Partial Autocorrelation of AAPL Closing Prices") # PACF of AAPL Closing Price

# Ljung-Box Statistics
Q.stats.1 <- Box.test(tsaapl, lag = 1, type = c("Ljung-Box"))
Q.stats.1$statistic # 114.9924
Q.stats.1$p.value # 0 

#Box-Pierce
Q.stats <- Box.test(tsaapl, lag = 1, type = c("Box-Pierce"))
Q.stats$statistic # 112.4179
Q.stats$p.value # 0
####--------------------------------------------


  ## Generate MA(4), AR(1), AR(2), ARMA(3,1), and Auto-Arima Graphs
####--------------------------------------------
  ## AAPL Closing Prices: MA4 Model / Diagnostic Tests
# Build out MA(4) model
tsaapl.ma4.model <- arma(tsaapl, order = c(0,4)) # Call ARMA function to begin building MA(4) model
fitted.ma4 <- tsaapl.ma4.model$fitted.values # Fitted Values for MA(4) function
resid.ma4 <- tsaapl.ma4.model$residuals # Residual Values for MA(4) function 
summary(fitted.ma4) # Summarize fitted.ma4 data
summary(resid.ma4) # Summarize resid.ma4 data

# PLOT MA(4) RESIDUAL MODEL 
plot(tsaapl,type="l", yaxt='n',ylim=c(-30,150), col="red", ylab='Residuals, Fitted Values, and Closing Price', main="AAPL Closing Prices: MA(4) Model Residual Plot Model")
lines(fitted.ma4,col="green")
axis(side=4,at=c(25,50,75,100,125,150),lab=c(25,50,75,100,125,150),cex.axis=0.8)
par(new=T)
lines(resid.ma4,col="blue",ylim=c(-30,150))
abline(h=0,col="black")
abline(h=45,col="black",lty="dashed")
abline(h=-30,col="black",lty="dashed")
axis(side=2,at=c(-30,-10,0,25,45),lab=c(-30,-10,0,25,45), cex.axis=0.8)

# DIAGNOSTIC STATS --> MODEL SELECTION
tsaapl.ma4 <- arima(tsaapl, order = c(0,0,4))
BIC(tsaapl.ma4) # 891.4646
AIC(tsaapl.ma4) # 874.1678####--------

####--------
  ## AAPL Closing Prices: AR(1) Model / Diagnostic Tests 
# BUILD OUT AR(1) MODEL
tsaapl.ar1.model <- arma(tsaapl, order = c(1,0,0)) # Call ARMA function to begin building AR(1) model
fitted.ar1 <- tsaapl.ar1.model$fitted.values # Fitted Values for AR(1) function
resid.ar1 <- tsaapl.ar1.model$residuals # Residual Values for AR(1) function 
summary(fitted.ar1) # Summarize fitted.ar1 data
summary(resid.ar1) # Summarize resid.ar1 data

# PLOT AR(1) RESIDUAL MODEL 
plot(tsaapl,type="l", yaxt='n', ylim=c(-20,150),col="red", ylab='Residuals, Fitted Values, and Closing Price', main="AAPL Closing Prices: AR(1) Model Residual Plot")
lines(fitted.ar1,col="green")
axis(side=4, at=c(25,50,75,100,125,150),lab=c(25,50,75,100,125,150),cex.axis=0.8)
par(new=T)
lines(resid.ar1,col="blue", ylim=c(-20,150))
abline(h=0,col="black")
abline(h=18,col="black",lty="dashed")
abline(h=-18,col="black",lty="dashed")
axis(side=2,at=c(-25,-10,0,10,25),lab=c(-25,-10,0,10,25) ,cex.axis=0.8)

# DIAGNOSTIC STATS --> MODEL SELECTION
tsaapl.ar1 <- arima(tsaapl, order = c(0,1,0))
BIC(tsaapl.ar1) # 757.3741
AIC(tsaapl.ar1) # 754.4989
####--------

####--------
  ## AAPL Closing Prices: AR(2) Model / Diagnostic Tests 
# BUILD OUT AR(2) MODEL
tsaapl.ar2.model <- arma(tsaapl, order = c(2,0,0)) # Call ARMA function to begin building AR(2) model
fitted.ar2 <- tsaapl.ar2.model$fitted.values # Fitted Values for AR(2) function
resid.ar2 <- tsaapl.ar2.model$residuals # Residual Values for AR(2) function 
summary(fitted.ar2) # Summarize fitted.ar2 data
summary(resid.ar2) # Summarize resid.ar2 data

# PLOT AR(2) RESIDUAL MODEL 
plot(tsaapl,type="l",yaxt='n', ylim=c(-25,150), col="red", ylab='Residuals, Fitted Values, and Closing Price', main="AAPL Closing Prices: AR(2) Residual Plot Model")
lines(fitted.ar2,col="green")
axis(side=4, at=c(25,50,75,100,125,150),lab=c(25,50,75,100,125,150),cex.axis=0.8)
par(new=T)
lines(resid.ar2,col="blue", ylim=c(-25,150))
abline(h=0,col="black")
abline(h=17,col="black",lty="dashed")
abline(h=-22,col="black",lty="dashed")
axis(side=2,at=c(-25,-10,0,10,25),lab=c(-25,-10,0,10,25) ,cex.axis=0.8)

# DIAGNOSTIC STATS --> MODEL SELECTION
tsaapl.ar2 <- arima(tsaapl, order=c(0,2,0))
BIC(tsaapl.ar2) # 797.0116
AIC(tsaapl.ar2) # 794.1441
####--------

####--------
  ## AAPL Closing Prices: ARMA(3,1) Model / Diagnostic Tests 
# BUILD OUT ARMA(3,1) MODEL
tsaapl.arma.model <- arma(tsaapl, order = c(3,0,1)) # Call ARMA function to begin building ARMA(3,1) model
fitted.arma <- tsaapl.arma.model$fitted.values # Fitted Values for ARMA(3,1) function
resid.arma <- tsaapl.arma.model$residuals # Residual Values for ARMA(3,1) function
summary(fitted.arma) # Summarize fitted.arma data
summary(resid.arma) # Summarize resid.arma data

# PLOT ARMA(3,1) RESIDUAL MODEL 
plot(tsaapl,type="l", yaxt='n', ylim=c(-20,150), ylab='Residuals, Fitted Values, and Closing Price', col="red", main="AAPL Closing Prices: ARMA(3,1) Model Residual Plot")
lines(fitted.arma,col="green")
axis(side=4, at=c(25,50,75,100,125,150),lab=c(25,50,75,100,125,150),cex.axis=0.8)
par(new=T)
lines(resid.arma,col="blue", ylim=c(-20,150))
abline(h=0,col="black")
abline(h=19,col="black",lty="dashed")
abline(h=-19,col="black",lty="dashed")
axis(side=2,at=c(-25,-10,0,10,25),lab=c(-25,-10,0,10,25) ,cex.axis=0.8)

# DIAGNOSTIC STATS --> MODEL SELECTION
tsaapl.arma <- arima(tsaapl, order = c(0,3,1))
BIC(tsaapl.arma) # 801.5827
AIC(tsaapl.arma) # 795.8631
####--------

####--------
  ## AAPL Closing Prices: AUTO ARIMA Model / Diagnostic Tests 
# BUILD OUT AUTO ARIMA MODEL
auto.arima(tsaapl, seasonal = FALSE ) # AIC=728.46, BIC=742.8
auto.arima_tsaapl = auto.arima(tsaapl, seasonal = FALSE)
tsdisplay(residuals(auto.arima_tsaapl),lag.max = 40, main = "ARIMA (1,1,1) Residuals")
### -----------------------------------------------------------------------------


### -----------------------------------------------------------------------------
  ## AAPL Closing Price History and Forecast 
# 2-Month Point Forecast for AR(1) MODEL --> 2 Months (2021.1 - 2021.2) Forecast
tsaapl.forecast.2m <- forecast(tsaapl.ar1, h = 2)
plot(tsaapl.forecast.2m)  
summary(tsaapl.forecast.2m)

# 6-Month Point Forecast for AR(2) MODEL --> 6 Months (2021.1 - 2021.6) Forecast
tsaapl.forecast.6m <- forecast(tsaapl.ar2, h = 6)
plot(tsaapl.forecast.6m)
summary(tsaapl.forecast.6m)

# 1 Year Point Forecast for AR(2) MODEL --> 1 year (2021.1 and 2022.1) Forecast
tsaapl.forecast.1y <- forecast(tsaapl.ar2, h = 12)
plot(tsaapl.forecast.1y)
summary(tsaapl.forecast.1y)
### -----------------------------------------------------------------------------


### -----------------------------------------------------------------------------
  # BUILD OUT LOG-LINEAR MODEL
log_aapl=log(tsaapl) # Log AAPL
summary(log_aapl)
plot(log_aapl, axes = FALSE, xlim=c(2010.1,2020.12), ylim=c(1.5,5),xlab="Time",ylab="Log Closing Prices",main='Log AAPL Closing Prices', col="blue") # Plot log_aapl 
box()
axis(side=1,at=2010:2020, lab=c(2010:2020),cex.axis=0.9)
axis(side=2,at=c(1.5,2,2.5,3.0,3.5,4,4.5,5),lab=c(1.5,2,2.5,3.0,3.5,4,4.5,5),cex.axis=0.9)

# LINEAR REGRESSION ON TIME AND LOG_AAPL
time=data.frame(c=1:132) # Build out time function in data frame 
logtsaapl = data.frame(time,log_aapl) # Log AAPL ts
names(logtsaapl)[1] <- "time"
log_aapl_reg = lm(log_aapl ~ time, data=logtsaapl)
summary(log_aapl_reg)
AIC(log_aapl_reg) # -58.22402
BIC(log_aapl_reg) # -49.57562
dwtest(log_aapl_reg) # 0.16577

  # BUILD OUT ACTUAL, PREDICITION, AND RESIDUAL VALUES
# Actual Values 
logtsaapl.actual = as.vector(logtsaapl$log_aapl)
summary(logtsaapl.actual)
logtsaapl.actual # Actual Values 

# Prediction Valyes 
logtsaapl.prediction = predict(log_aapl_reg, logtsaapl)
logtsaapl.prediction # Prediction Values 

# Residual Values 
logtsaapl.residual=logtsaapl.actual-logtsaapl.prediction
summary(logtsaapl.residual)
logtsaapl.residual
logtsaapl.residual = 1.5*logtsaapl.residual+2
summary(logtsaapl.residual) # Residual Values 

  # BUILD OUT TIME SERIES PLOT WITH LOGTSAAPL 
plot(logtsaapl.actual,type="l", axes=FALSE,ylim=c(0,5),xlab="", ylab="", col="red", main="Log - AAPL Closing Prices on Time Residual" )
lines(logtsaapl.prediction,col="green")
lines(logtsaapl.residual, col="blue")
abline(h=1.5, col="black",lty="dashed")
abline(h=2.25, col="black",)
abline(h=2.9, col="black",lty="dashed")
box()
axis(side=1,at=c(seq(1,132,by=12)),lab=c(2010:2020),cex.axis=0.9)
axis(side=2,at=c(1.5,2,2.5,3,3.5,4,4.5,5),lab=c(1.5,2,2.5,3,3.5,4,4.5,5),cex.axis=0.7)
axis(side=4,at=c(2,2.5,3,3.5,4,4.5,5),lab=c(2,2.5,3,3.5,4,4.5,5),cex.axis=0.9)

  # BUILD OUT ACF AND PACF PLOTS 
par(mfrow=c(1,1)) # Separate Plots
acf(log_aapl, main = "Autocorrelation of Log AAPL Closing Prices")
pacf(log_aapl, main = "Partial Autocorrelation of Log AAPL Closing Prices")
### -----------------------------------------------------------------------------


### -----------------------------------------------------------------------------
  ## BUILD OUT CYCLE TREND MODEL 
# COMBINE ARMA AND LOG_AAPL MODELS
log_aapl.arma = data.frame(cbind(logtsaapl,logtsaapl.residual)) #log_aapl in ARMA model 
log_aapl.arma
head(log_aapl.arma)
log_aapl.arma_regression = lm(log_aapl~.-1, data = log_aapl.arma) # Run Cycle Trend model regression 
summary(log_aapl.arma_regression)
AIC(log_aapl.arma_regression) # -172.9873
BIC(log_aapl.arma_regression) # -164.3389
dwtest(log_aapl.arma_regression) # 0.15366
