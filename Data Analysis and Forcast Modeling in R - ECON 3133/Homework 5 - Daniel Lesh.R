# Homework 5
# Date: 04/08/2021
# Course: ECON 3313 Spring 2021
# Author: Zhenhao Gong/Daniel Lesh

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

# Question 5 
# Problem 1-----------------------------------------------------
# Format a Time Series to the data
caemp = ts(data1[,"CAEMP"],start=c(1961,1),frequency=4)
caempsamp = ts(caemp[5:132],start=c(1962,1),frequency=4)
par(mfrow=c(1,1))
    # Built Time Series Plot 
ts.plot(caemp, main="Canadian Employment Index",ylab = "Canadian Employment Index", xlim=c(1962,1993.75),ylim=c(80,115)) 
#---------------------------------------------------------------

# Problem 2-----------------------------------------------------
# Autocorrelation and plot: gradual decay, AR models
caemp.acf <- acf(caemp,type="correlation", plot=FALSE)
plot(acf(caempsamp,plot=F),xlim=c(0,3),ylim=c(-1,1),main="Autocorrelation")
# Exhibits gradual decay --> preference towards AR Model 

# Partial Autocorrelation and plot: sharp cut off, AR models
caemp.pacf <- acf(caempsamp,plot=F,type="p")
plot(acf(caempsamp,plot=F,type="p"),xlim=c(0,3),ylim=c(-1,1), main="Partial Autocorrelation")
# Exhibits sharp cut off --> preference towards AR Model 

## Diagnostic statistics for MA(2) Model:
# Using 'arima' function for MA model 
caemp.ma <- arima(caemp,order=c(0,0,2)) # MA(2) Model 
print(caemp.ma)
AIC(caemp.ma) # 686.9732
BIC(caemp.ma) # 698.6238

## Diagnostic statistics for AR(2) Model: 
caemp.ar <- arima(caemp,order=c(2,0,0)) # AR(2) Model 
print(caemp.ar)
AIC(caemp.ar) # 493.5718
BIC(caemp.ar) # 505.2224

## Diagnostic statistics for ARMA(2,2) Model: 
caemp.arma <- arima(caemp,order=c(2,0,2)) # ARMA(2,2) Model 
print(caemp.arma)
AIC(caemp.arma) # 496.6441
BIC(caemp.arma) # 514.12
#---------------------------------------------------------------

# Problem 3-----------------------------------------------------
# AR(1) Model --> 4 quarters ahead 
caemp.ar1.pred <- predict(arima(caemp[1:132], order=c(1,0,0)), n.ahead=4)
caemp.ar1.pred.forecast <- caemp.ar1.pred$pred

# Set interval values null
caemp.ar1.pred.se.lower <- NULL
caemp.ar1.pred.se.upper <- NULL
for (i in seq_along(caemp.ar1.pred.forecast)) {
  caemp.ar1.pred.se.lower[16+i] <- caemp.ar1.pred.forecast[i]-(caemp.ar1.pred$se[i]*1.96)
  caemp.ar1.pred.se.upper[16+i] <- caemp.ar1.pred.forecast[i]+(caemp.ar1.pred$se[i]*1.96)
}

caemp.ar1.history <- caemp[116:132]
caemp.ar1.history <- as.ts(caemp.ar1.history, frequency=4)

caemp.ar1.forecast <- NULL
for (j in seq_along(caemp.ar1.pred.forecast)) {
  caemp.ar1.forecast[16+j] <- caemp.ar1.pred.forecast[j]
}
caemp.ar1.forecast <- as.ts(caemp.ar1.forecast, frequency=4)

plot(caemp.ar1.history, type="l",xlim=c(1,20),ylim=c(80,105), ylab="History and Forecast",xlab="Time",axes=F, main="Employment History and Forecast AR(1) Model")
lines(caemp.ar1.forecast, col="red")
lines(caemp.ar1.pred.se.lower,col="blue")
lines(caemp.ar1.pred.se.upper,col="blue")
box()
axis(side=1, at=seq(1,20), lab=c(90.1,90.2,90.3,90.4,91.1,91.2,91.3,91.4,92.1,92.2,92.3,92.4,93.1,93.2,93.3,93.4,94.1,94.2,94.3,94.4))
axis(side=2, at=seq(80,105,by=5))
abline(v=17,lty="dashed")

# Realization Forecast for AR(1) Model
plot(caemp[116:136], type="l",xlim=c(1,20),ylim=c(80,105), ylab="History, Forecast and Realization",xlab="Time",axes=F, main="Employment History, Forecast and Realization AR(1) Model")
lines(caemp.ar1.forecast, col="red")
lines(caemp.ar1.pred.se.lower,col="blue")
lines(caemp.ar1.pred.se.upper,col="blue")
box()
axis(side=1, at=seq(1,20), lab=c(90.1,90.2,90.3,90.4,91.1,91.2,91.3,91.4,92.1,92.2,92.3,92.4,93.1,93.2,93.3,93.4,94.1,94.2,94.3,94.4))
axis(side=2, at=seq(80,105,by=5)) # summarize caemp.history --> get min and max values. Use these for the scale on y axis
abline(v=17,lty="dashed")
#------------

#------------
# AR(2) Model --> 4 quarters ahead 
caemp.ar2.pred <- predict(arima(caemp[1:132], order=c(2,0,0)), n.ahead=4)
caemp.ar2.pred.forecast <- caemp.ar2.pred$pred

# Set interval values null
caemp.ar2.pred.se.lower <- NULL
caemp.ar2.pred.se.upper <- NULL
for (i in seq_along(caemp.ar2.pred.forecast)) {
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

plot(caemp.ar2.history, type="l",xlim=c(1,20),ylim=c(80,105), ylab="History and Forecast",xlab="Time",axes=F, main="Employment History and Forecast AR(2) Model")
lines(caemp.ar2.forecast, col="red")
lines(caemp.ar2.pred.se.lower,col="blue")
lines(caemp.ar2.pred.se.upper,col="blue")
box()
axis(side=1, at=seq(1,20), lab=c(90.1,90.2,90.3,90.4,91.1,91.2,91.3,91.4,92.1,92.2,92.3,92.4,93.1,93.2,93.3,93.4,94.1,94.2,94.3,94.4))
axis(side=2, at=seq(80,105,by=5))
abline(v=17,lty="dashed")

# Realization Forecast for AR(2) Model
plot(caemp[116:136], type="l",xlim=c(1,20),ylim=c(80,105), ylab="History, Forecast and Realization",xlab="Time",axes=F, main="Employment History, Forecast and Realization AR(2) Model")
lines(caemp.ar2.forecast, col="red")
lines(caemp.ar2.pred.se.lower,col="blue")
lines(caemp.ar2.pred.se.upper,col="blue")
box()
axis(side=1, at=seq(1,20), lab=c(90.1,90.2,90.3,90.4,91.1,91.2,91.3,91.4,92.1,92.2,92.3,92.4,93.1,93.2,93.3,93.4,94.1,94.2,94.3,94.4))
axis(side=2, at=seq(80,105,by=5)) # summarize caemp.history --> get min and max values. Use these for the scale on y axis
abline(v=17,lty="dashed")
#------------

#------------
# AR(3) Model --> 4 quarters ahead 
caemp.ar3.pred <- predict(arima(caemp[1:132], order=c(3,0,0)), n.ahead=4)
caemp.ar3.pred.forecast <- caemp.ar3.pred$pred

# Set interval values null
caemp.ar3.pred.se.lower <- NULL
caemp.ar3.pred.se.upper <- NULL
for (i in seq_along(caemp.ar3.pred.forecast)) {
  caemp.ar3.pred.se.lower[16+i] <- caemp.ar3.pred.forecast[i]-(caemp.ar3.pred$se[i]*1.96)
  caemp.ar3.pred.se.upper[16+i] <- caemp.ar3.pred.forecast[i]+(caemp.ar3.pred$se[i]*1.96)
}

caemp.ar3.history <- caemp[116:132]
caemp.ar3.history <- as.ts(caemp.ar3.history, frequency=4)

caemp.ar3.forecast <- NULL
for (j in seq_along(caemp.ar3.pred.forecast)) {
  caemp.ar3.forecast[16+j] <- caemp.ar3.pred.forecast[j]
}
caemp.ar3.forecast <- as.ts(caemp.ar3.forecast, frequency=4)

plot(caemp.ar3.history, type="l",xlim=c(1,20),ylim=c(80,105), ylab="History and Forecast",xlab="Time",axes=F, main="Employment History and Forecast AR(3) Model")
lines(caemp.ar3.forecast, col="red")
lines(caemp.ar3.pred.se.lower,col="blue")
lines(caemp.ar3.pred.se.upper,col="blue")
box()
axis(side=1, at=seq(1,20), lab=c(90.1,90.2,90.3,90.4,91.1,91.2,91.3,91.4,92.1,92.2,92.3,92.4,93.1,93.2,93.3,93.4,94.1,94.2,94.3,94.4))
axis(side=2, at=seq(80,105,by=5))
abline(v=17,lty="dashed")

# Realization Forecast for AR(3) Model
plot(caemp[116:136], type="l",xlim=c(1,20),ylim=c(80,105), ylab="History, Forecast and Realization",xlab="Time",axes=F, main="Employment History, Forecast and Realization AR(3) Model")
lines(caemp.ar3.forecast, col="red")
lines(caemp.ar3.pred.se.lower,col="blue")
lines(caemp.ar3.pred.se.upper,col="blue")
box()
axis(side=1, at=seq(1,20), lab=c(90.1,90.2,90.3,90.4,91.1,91.2,91.3,91.4,92.1,92.2,92.3,92.4,93.1,93.2,93.3,93.4,94.1,94.2,94.3,94.4))
axis(side=2, at=seq(80,105,by=5)) # summarize caemp.history --> get min and max values. Use these for the scale on y axis
abline(v=17,lty="dashed")
#------------

#------------
# AR(4) Model --> 4 quarters ahead 
caemp.ar4.pred <- predict(arima(caemp[1:132], order=c(4,0,0)), n.ahead=4)
caemp.ar4.pred.forecast <- caemp.ar4.pred$pred

# Set interval values null
caemp.ar4.pred.se.lower <- NULL
caemp.ar4.pred.se.upper <- NULL
for (i in seq_along(caemp.ar4.pred.forecast)) {
  caemp.ar4.pred.se.lower[16+i] <- caemp.ar4.pred.forecast[i]-(caemp.ar4.pred$se[i]*1.96)
  caemp.ar4.pred.se.upper[16+i] <- caemp.ar4.pred.forecast[i]+(caemp.ar4.pred$se[i]*1.96)
}

caemp.ar4.history <- caemp[116:132]
caemp.ar4.history <- as.ts(caemp.ar4.history, frequency=4)

caemp.ar4.forecast <- NULL
for (j in seq_along(caemp.ar4.pred.forecast)) {
  caemp.ar4.forecast[16+j] <- caemp.ar4.pred.forecast[j]
}
caemp.ar4.forecast <- as.ts(caemp.ar4.forecast, frequency=4)

plot(caemp.ar4.history, type="l",xlim=c(1,20),ylim=c(80,105), ylab="History and Forecast",xlab="Time",axes=F, main="Employment History and Forecast AR(4) Model")
lines(caemp.ar4.forecast, col="red")
lines(caemp.ar4.pred.se.lower,col="blue")
lines(caemp.ar4.pred.se.upper,col="blue")
box()
axis(side=1, at=seq(1,20), lab=c(90.1,90.2,90.3,90.4,91.1,91.2,91.3,91.4,92.1,92.2,92.3,92.4,93.1,93.2,93.3,93.4,94.1,94.2,94.3,94.4))
axis(side=2, at=seq(80,105,by=5))
abline(v=17,lty="dashed")

# Realization Forecast for AR(4) Model
plot(caemp[116:136], type="l",xlim=c(1,20),ylim=c(80,105), ylab="History, Forecast and Realization",xlab="Time",axes=F, main="Employment History, Forecast and Realization AR(4) Model")
lines(caemp.ar4.forecast, col="red")
lines(caemp.ar4.pred.se.lower,col="blue")
lines(caemp.ar4.pred.se.upper,col="blue")
box()
axis(side=1, at=seq(1,20), lab=c(90.1,90.2,90.3,90.4,91.1,91.2,91.3,91.4,92.1,92.2,92.3,92.4,93.1,93.2,93.3,93.4,94.1,94.2,94.3,94.4))
axis(side=2, at=seq(80,105,by=5)) # summarize caemp.history --> get min and max values. Use these for the scale on y axis
abline(v=17,lty="dashed")
#------------

#------------
# ARMA(3,1) Model --> 4 quarters ahead 
caemp.arma31.pred <- predict(arima(caemp[1:132], order=c(3,1,0)), n.ahead=4)
caemp.arma31.pred.forecast <- caemp.arma31.pred$pred

# Set interval values null
caemp.arma31.pred.se.lower <- NULL
caemp.arma31.pred.se.upper <- NULL
for (i in seq_along(caemp.arma31.pred.forecast)) {
  caemp.arma31.pred.se.lower[16+i] <- caemp.arma31.pred.forecast[i]-(caemp.arma31.pred$se[i]*1.96)
  caemp.arma31.pred.se.upper[16+i] <- caemp.arma31.pred.forecast[i]+(caemp.arma31.pred$se[i]*1.96)
}

caemp.arma31.history <- caemp[116:132]
caemp.arma31.history <- as.ts(caemp.arma31.history, frequency=4)

caemp.arma31.forecast <- NULL
for (j in seq_along(caemp.arma31.pred.forecast)) {
  caemp.arma31.forecast[16+j] <- caemp.arma31.pred.forecast[j]
}
caemp.arma31.forecast <- as.ts(caemp.arma31.forecast, frequency=4)

plot(caemp.arma31.history, type="l",xlim=c(1,20),ylim=c(80,105), ylab="History and Forecast",xlab="Time",axes=F, main="Employment History and Forecast ARMA(3,1) Model")
lines(caemp.arma31.forecast, col="red")
lines(caemp.arma31.pred.se.lower,col="blue")
lines(caemp.arma31.pred.se.upper,col="blue")
box()
axis(side=1, at=seq(1,20), lab=c(90.1,90.2,90.3,90.4,91.1,91.2,91.3,91.4,92.1,92.2,92.3,92.4,93.1,93.2,93.3,93.4,94.1,94.2,94.3,94.4))
axis(side=2, at=seq(80,105,by=5))
abline(v=17,lty="dashed")

# Realization Forecast for ARMA(3,1) Model
plot(caemp[116:136], type="l",xlim=c(1,20),ylim=c(80,105), ylab="History, Forecast and Realization",xlab="Time",axes=F, main="Employment History, Forecast and Realization ARMA(3,1) Model")
lines(caemp.arma31.forecast, col="red")
lines(caemp.arma31.pred.se.lower,col="blue")
lines(caemp.arma31.pred.se.upper,col="blue")
box()
axis(side=1, at=seq(1,20), lab=c(90.1,90.2,90.3,90.4,91.1,91.2,91.3,91.4,92.1,92.2,92.3,92.4,93.1,93.2,93.3,93.4,94.1,94.2,94.3,94.4))
axis(side=2, at=seq(80,105,by=5)) # summarize caemp.history --> get min and max values. Use these for the scale on y axis
abline(v=17,lty="dashed")
#------------

#------------
# ARMA(2,2) Model --> 4 quarters ahead 
caemp.arma22.pred <- predict(arima(caemp[1:132], order=c(2,0,2)), n.ahead=4)
caemp.arma22.pred.forecast <- caemp.arma22.pred$pred

# Set interval values null
caemp.arma22.pred.se.lower <- NULL
caemp.arma22.pred.se.upper <- NULL
for (i in seq_along(caemp.arma22.pred.forecast)) {
  caemp.arma22.pred.se.lower[16+i] <- caemp.arma22.pred.forecast[i]-(caemp.arma22.pred$se[i]*1.96)
  caemp.arma22.pred.se.upper[16+i] <- caemp.arma22.pred.forecast[i]+(caemp.arma22.pred$se[i]*1.96)
}

caemp.arma22.history <- caemp[116:132]
caemp.arma22.history <- as.ts(caemp.arma22.history, frequency=4)

caemp.arma22.forecast <- NULL
for (j in seq_along(caemp.arma22.pred.forecast)) {
  caemp.arma22.forecast[16+j] <- caemp.arma22.pred.forecast[j]
}
caemp.arma22.forecast <- as.ts(caemp.arma22.forecast, frequency=4)

plot(caemp.arma22.history, type="l",xlim=c(1,20),ylim=c(80,105), ylab="History and Forecast",xlab="Time",axes=F, main="Employment History and Forecast ARMA(2,2) Model")
lines(caemp.arma22.forecast, col="red")
lines(caemp.arma22.pred.se.lower,col="blue")
lines(caemp.arma22.pred.se.upper,col="blue")
box()
axis(side=1, at=seq(1,20), lab=c(90.1,90.2,90.3,90.4,91.1,91.2,91.3,91.4,92.1,92.2,92.3,92.4,93.1,93.2,93.3,93.4,94.1,94.2,94.3,94.4))
axis(side=2, at=seq(80,105,by=5))
abline(v=17,lty="dashed")

# Realization Forecast for ARMA(2,2) Model
plot(caemp[116:136], type="l",xlim=c(1,20),ylim=c(80,105), ylab="History, Forecast and Realization",xlab="Time",axes=F, main="Employment History, Forecast and Realization ARMA(2,2) Model")
lines(caemp.arma22.forecast, col="red")
lines(caemp.arma22.pred.se.lower,col="blue")
lines(caemp.arma22.pred.se.upper,col="blue")
box()
axis(side=1, at=seq(1,20), lab=c(90.1,90.2,90.3,90.4,91.1,91.2,91.3,91.4,92.1,92.2,92.3,92.4,93.1,93.2,93.3,93.4,94.1,94.2,94.3,94.4))
axis(side=2, at=seq(80,105,by=5)) # summarize caemp.history --> get min and max values. Use these for the scale on y axis
abline(v=17,lty="dashed")
#---------------------------------------------------------------

# Problem 4-----------------------------------------------------
# Employment ARMA(2,2) Model Residual Plot-------------
caemp.arma22.model <- arma(caemp, order=c(2,2))
coef.arma <- caemp.arma22.model$coef
fitted.arma <- caemp.arma22.model$fitted.values
resid.arma <- caemp.arma22.model$residuals
summary(caemp.arma22.model)

# diagnostic statistics:
diag.arma11.model <- arima(caemp,order=c(2,0,2))
AIC(diag.arma11.model) # 497.0403
BIC(diag.arma11.model) # 514.5162

par(mfrow=c(1,1))
plot(caemp,type="l", yaxt='n', ylim=c(50,120), xlab="",ylab="",col="red", main="Employment ARMA(2,2) Model Residual Plot")
lines(fitted.arma,col="green")
axis(side=4,at=c(80,90,100,110,120),lab=c(80,90,100,110,120),cex.axis=0.8)
par(new=T)
plot(resid.arma,col="blue", axes=FALSE, ylim=c(-4,20), xlab="",ylab="")
abline(h=0,col="black")
abline(h=1.4,col="black",lty="dashed")
abline(h=-1.4,col="black",lty="dashed")
axis(side=2,at=c(-4,-2,0,2,4,6,8),lab=c(-4,-2,0,2,4,6,8) ,cex.axis=0.6)