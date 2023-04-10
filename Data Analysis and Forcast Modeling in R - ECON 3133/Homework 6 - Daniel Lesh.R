# Homework 6
# Date: 04/20/2021
# Course: ECON 3313 Spring 2021
# Author: Daniel Lesh

# Clear memory
rm(list = ls())

# Mac
setwd("/Users/daniellesh/Desktop/ECON3313/Homeworks")

# Load Packages
library(car)
library(tseries)
library(forecast)
library(dynlm)

# Load Data 
data1 = read.csv("DataEmployment1.csv",header=TRUE)

# Format a Time Series to the data
caemp = ts(data1[,"CAEMP"],start=c(1961,1),frequency=4)
caempsamp = ts(caemp[5:132],start=c(1962,1),frequency=4)
par(mfrow=c(1,1))
# Built Time Series Plot 
ts.plot(caemp, main="Canadian Employment Index",ylab = "Canadian Employment Index", xlim=c(1962,1993.75),ylim=c(80,115)) 
#---------------------------------------------------------------

# Question 5 
# Problem 1-----------------------------------------------------
# MA(4) Model --> 4 quarters ahead (1994.1 - 1994.4)
caemp.ma4.pred <- predict(arima(caemp[1:132], order=c(0,0,4)), n.ahead=4)
caemp.ma4.pred.forecast <- caemp.ma4.pred$pred

# Set interval values null
caemp.ma4.pred.se.lower <- NULL
caemp.ma4.pred.se.upper <- NULL
for (i in 1:4) {
  caemp.ma4.pred.se.lower[16+i] <- caemp.ma4.pred.forecast[i]-(caemp.ma4.pred$se[i]*1.96)
  caemp.ma4.pred.se.upper[16+i] <- caemp.ma4.pred.forecast[i]+(caemp.ma4.pred$se[i]*1.96)
}

caemp.ma4.history <- caemp[116:132]
caemp.ma4.history <- as.ts(caemp.ma4.history, frequency=4)

caemp.ma4.forecast <- NULL
for (j in seq_along(caemp.ma4.pred.forecast)) {
  caemp.ma4.forecast[16+j] <- caemp.ma4.pred.forecast[j]
}
caemp.ma4.forecast <- as.ts(caemp.ma4.forecast, frequency=4)

# Rebuild time series plot and input MA(4) forecast data
plot(caemp.ma4.history, type="l",xlim=c(1,20),ylim=c(80,120), ylab="History and Forecast",xlab="Time",axes=F, main="Employment History and Forecast MA(4) Forecast Model")
lines(caemp.ma4.forecast, col="red")
lines(caemp.ma4.pred.se.lower,col="blue")
lines(caemp.ma4.pred.se.upper,col="blue")
box()
axis(side=1, at=seq(1,20), lab=c(90.1,90.2,90.3,90.4,91.1,91.2,91.3,91.4,92.1,92.2,92.3,92.4,93.1,93.2,93.3,93.4,94.1,94.2,94.3,94.4))
axis(side=2, at=seq(80,120,by=5))
abline(v=17,lty="dashed")

# Realization Forecast for MA(4) Model
plot(caemp[116:136], type="l",xlim=c(1,20),ylim=c(80,120), ylab="History, Forecast, and Realization",xlab="Time",axes=F, main="Employment History, Forecast, and Realization MA(4) Forecast Model")
lines(caemp.ma4.forecast, col="red")
lines(caemp.ma4.pred.se.lower,col="blue")
lines(caemp.ma4.pred.se.upper,col="blue")
box()
axis(side=1, at=seq(1,20), lab=c(90.1,90.2,90.3,90.4,91.1,91.2,91.3,91.4,92.1,92.2,92.3,92.4,93.1,93.2,93.3,93.4,94.1,94.2,94.3,94.4))
axis(side=2, at=seq(80,120,by=5)) # summarize caemp.history --> get min and max values. Use these for the scale on y axis
abline(v=17,lty="dashed")
#------------

# Problem 2-----------------------------------------------------
# AR(2) Model --> 4 quarters ahead (1994.1 - 1994.4)
caemp.ar2.pred <- predict(arima(caemp[1:132], order=c(2,0,0)), n.ahead=4)
caemp.ar2.pred.forecast <- caemp.ar2.pred$pred

# Set interval values null
caemp.ar2.pred.se.lower <- NULL
caemp.ar2.pred.se.upper <- NULL
for (i in 1:4) {
  caemp.ar2.pred.se.lower[16+i] <- caemp.ar2.pred.forecast[i]-(caemp.ar2.pred$se[i]*1.96)
  caemp.ar2.pred.se.upper[16+i] <- caemp.ar2.pred.forecast[i]+(caemp.ar2.pred$se[i]*1.96)
}

caemp.ar2.history <- caemp[116:132]
caemp.ar2.history <- as.ts(caemp.ar2.history, frequency=4)

caemp.ar2.forecast <- NULL
for (j in seq_along(caemp.ar2.pred.forecast)) {
  caemp.ar2.forecast[16+j] <- caemp.ar2.pred.forecast[j]
}
caemp.ar2.forecast <- as.ts(caemp.ar2.forecast, frequency=4)

# Rebuild time series plot and input AR(2) forecast data
plot(caemp.ar2.history, type="l",xlim=c(1,20),ylim=c(80,110), ylab="History and Forecast",xlab="Time",axes=F, main="Employment History and Forecast AR(2) Forecast Model")
lines(caemp.ar2.forecast, col="red")
lines(caemp.ar2.pred.se.lower,col="blue")
lines(caemp.ar2.pred.se.upper,col="blue")
box()
axis(side=1, at=seq(1,20), lab=c(90.1,90.2,90.3,90.4,91.1,91.2,91.3,91.4,92.1,92.2,92.3,92.4,93.1,93.2,93.3,93.4,94.1,94.2,94.3,94.4))
axis(side=2, at=seq(80,110,by=5))
abline(v=17,lty="dashed")

# Realization Forecast for AR(2) Model
plot(caemp[116:136], type="l",xlim=c(1,20),ylim=c(80,110), ylab="History, Forecast, and Realization",xlab="Time",axes=F, main="Employment History, Forecast, and Realization AR(2) Forecast Model")
lines(caemp.ar2.forecast, col="red")
lines(caemp.ar2.pred.se.lower,col="blue")
lines(caemp.ar2.pred.se.upper,col="blue")
box()
axis(side=1, at=seq(1,20), lab=c(90.1,90.2,90.3,90.4,91.1,91.2,91.3,91.4,92.1,92.2,92.3,92.4,93.1,93.2,93.3,93.4,94.1,94.2,94.3,94.4))
axis(side=2, at=seq(80,110,by=5)) # summarize caemp.history --> get min and max values. Use these for the scale on y axis
abline(v=17,lty="dashed")
#------------

# Problem 3-----------------------------------------------------
# ARMA(3,1) Model --> 4 quarters ahead (1994.1 - 1994.4)
caemp.armaSH.pred <- predict(arima(caemp[1:132], order=c(3,0,1)), n.ahead=4)
caemp.armaSH.pred.forecast <- caemp.armaSH.pred$pred

# Set interval values null
caemp.armaSH.pred.se.lower <- NULL
caemp.armaSH.pred.se.upper <- NULL
for (i in 1:4) {
  caemp.armaSH.pred.se.lower[16+i] <- caemp.armaSH.pred.forecast[i]-(caemp.armaSH.pred$se[i]*1.96)
  caemp.armaSH.pred.se.upper[16+i] <- caemp.armaSH.pred.forecast[i]+(caemp.armaSH.pred$se[i]*1.96)
}

caemp.armaSH.history <- caemp[116:132]
caemp.armaSH.history <- as.ts(caemp.armaSH.history, frequency=4)

caemp.armaSH.forecast <- NULL
for (j in seq_along(caemp.armaSH.pred.forecast)) {
  caemp.armaSH.forecast[16+j] <- caemp.armaSH.pred.forecast[j]
}
caemp.armaSH.forecast <- as.ts(caemp.armaSH.forecast, frequency=4)

# Rebuild time series plot and input ARMA(3,1) forecast data
plot(caemp.armaSH.history, type="l",xlim=c(1,20),ylim=c(80,110), ylab="History and Forecast",xlab="Time",axes=F, main="Employment History and Forecast ARMA Short Horizon Forecast Model")
lines(caemp.armaSH.forecast, col="red")
lines(caemp.armaSH.pred.se.lower,col="blue")
lines(caemp.armaSH.pred.se.upper,col="blue")
box()
axis(side=1, at=seq(1,20), lab=c(90.1,90.2,90.3,90.4,91.1,91.2,91.3,91.4,92.1,92.2,92.3,92.4,93.1,93.2,93.3,93.4,94.1,94.2,94.3,94.4))
axis(side=2, at=seq(80,110,by=5))
abline(v=17,lty="dashed")

# Realization Forecast for ARMA(3,1) Model
plot(caemp[116:136], type="l",xlim=c(1,20),ylim=c(80,110), ylab="History, Forecast, and Realization",xlab="Time",axes=F, main="Employment History, Forecast, and Realization ARMA Short Horizon Forecast Model")
lines(caemp.armaSH.forecast, col="red")
lines(caemp.armaSH.pred.se.lower,col="blue")
lines(caemp.armaSH.pred.se.upper,col="blue")
box()
axis(side=1, at=seq(1,20), lab=c(90.1,90.2,90.3,90.4,91.1,91.2,91.3,91.4,92.1,92.2,92.3,92.4,93.1,93.2,93.3,93.4,94.1,94.2,94.3,94.4))
axis(side=2, at=seq(80,110,by=5)) # summarize caemp.history --> get min and max values. Use these for the scale on y axis
abline(v=17,lty="dashed")
#------------

# Problem 4-----------------------------------------------------
# ARMA(3,1) Model --> 4 quarters ahead (1994.1 - 1994.4)
caemp.armaLH.pred <- predict(arima(caemp[1:132], order=c(3,0,1)), n.ahead=16) # Forecasting 16 steps ahead 
caemp.armaLH.pred.forecast <- caemp.armaLH.pred$pred

# Set interval values null
caemp.armaLH.pred.se.lower <- NULL
caemp.armaLH.pred.se.upper <- NULL
for (i in seq_along(caemp.armaLH.pred.forecast)) {
  caemp.armaLH.pred.se.lower[16+i] <- caemp.armaLH.pred.forecast[i]-(caemp.armaLH.pred$se[i]*1.96)
  caemp.armaLH.pred.se.upper[16+i] <- caemp.armaLH.pred.forecast[i]+(caemp.armaLH.pred$se[i]*1.96)
}

caemp.armaLH.history <- caemp[116:132]
caemp.armaLH.history <- as.ts(caemp.armaLH.history, frequency=4)

caemp.armaLH.forecast <- NULL
for (j in seq_along(caemp.armaLH.pred.forecast)) {
  caemp.armaLH.forecast[16+j] <- caemp.armaLH.pred.forecast[j]
}
caemp.armaLH.forecast <- as.ts(caemp.armaLH.forecast, frequency=4)

# Rebuild time series plot and input ARMA(3,1) forecast data
plot(caemp.armaLH.history, type="l",xlim=c(1,26),ylim=c(65,110), ylab="History and Forecast",xlab="Time",axes=F, main="Employment History and Forecast ARMA Long Horizon Forecast Model")
lines(caemp.armaLH.forecast, col="red")
lines(caemp.armaLH.pred.se.lower,col="blue")
lines(caemp.armaLH.pred.se.upper,col="blue")
box()
axis(side=1, at=seq(1,26), lab=c(90.1,90.2,90.3,90.4,91.1,91.2,91.3,91.4,92.1,92.2,92.3,92.4,93.1,93.2,93.3,93.4,94.1,94.2,94.3,94.4,95.1,95.2,95.3,95.4,96.1,96.2))
axis(side=2, at=seq(65,110,by=5))
abline(v=17,lty="dashed")

# Realization Forecast for ARMA(3,1) Model
plot(caemp[116:136], type="l",xlim=c(1,26),ylim=c(65,110), ylab="History, Forecast, and Realization",xlab="Time",axes=F, main="Employment History, Forecast, and Realization ARMA Long Horizon Forecast Model")
lines(caemp.armaLH.forecast, col="red")
lines(caemp.armaLH.pred.se.lower,col="blue")
lines(caemp.armaLH.pred.se.upper,col="blue")
box()
axis(side=1, at=seq(1,26), lab=c(90.1,90.2,90.3,90.4,91.1,91.2,91.3,91.4,92.1,92.2,92.3,92.4,93.1,93.2,93.3,93.4,94.1,94.2,94.3,94.4,95.1,95.2,95.3,95.4,96.1,96.2))
axis(side=2, at=seq(65,110,by=5))
abline(v=17,lty="dashed")
#------------
