# Homework 3 - Question #5
# Date: 03/03/2021
# Course: ECON 3313
# Author: Daniel Lesh    

# Clear memory
rm(list = ls())

# Mac
setwd("/Users/daniellesh/Desktop/ECON3313/Homeworks")

# --------------------------
## Install Packages
library("moments") 
library(sandwich)
library(ggplot2)
library(gridExtra)
library(GGally)
library(forecast)
library(plotrix)
library(lmtest)
library(zoo) 
# --------------------------

liquor=read.csv("DataLiquor.csv",header=TRUE)

# drop the NA terms in the data set (Changing name)
liquor=na.omit(liquor)
names(liquor)[1] <- "liquor"
summary(liquor)

# Label functions into each year 
# Transforming data set into Time Series data 
lsts=ts(data=liquor,start=c(1987,1),frequency=12)
lsts

# Need to tell R how to start data set and put it into time series plot 
# Par(mfrow... is to show one graph)
par(mfrow=c(1,1),xaxs="i",yaxs="i")

# Plotting actual Time series data
plot(lsts,axes=FALSE,xlim=c(1988,2016),ylim=c(0,3000),xlab="Time",ylab="Liquor Sales",col="blue")

# Add a box around plotted data 
box()

# Axis (Side = 1) is the X axis 
axis(side=1,at=1987:2016,lab=c(1987:2016),cex.axis=0.7)

# Axis (Side = 2) is the Y axis - only using 6 values to describe liquor sale volumes  
axis(side=2,at=c(0,500,1000,1500,2000,2500,3000),lab=c(0,500,1000,1500,2000,2500,3000),cex.axis=0.75)
# --------------------------


# --------------------------
## #2 - Fit log-linear and log-quadratic models to time series
time=data.frame(c(1:336)) # Change name to time 
names(time)[1] <- "time" # rename to time 
liquor_time =data.frame(time,liquor) # combine liquor sales and time

# Run linear trend regression on liquor_time 
linear.lm = lm(liquor~time,data=liquor_time)
print(summary(linear.lm))

#  Forecast the liquor sales on 2015.01 based on the linear trend model
# We create a new data frame that set the time on 2015.01-2015.12.
liquorsales2015 = data.frame(time=337:348)

# We now apply the predict function and set the predictor variable in the newdata argument. We also set the interval type as "confidence", 
# and use the default 0.95 confidence level.
# This is how to create point and interval forecasts 
liquor.pred.out <- predict(linear.lm, liquorsales2015, interval="confidence", level = 0.95) 
liquor.pred.out
row.names(liquor.pred.out) <- c("2015.01", "2015.02", "2015.03","2015.04","2015.05","2015.06",
                                "2015.07","2015.08","2015.09", "2015.10","2015.11", "2015.12")
liquor.pred.out

# diagnostic statistics:
# Standard error of the regression: 203 on 334 degrees of freedom
# Adjusted R-squared:  0.8184
# Akaike information criterion (AIC): 4527.944
AIC(linear.lm)
# Schwarz information criterion (SIC): 4539.395
BIC(linear.lm)
# Durbin-Watson Statistic: 1.6257
dwtest(liquor~time,data=liquor_time)

# How to draw Residual plot
# Actual values of liquor sales  
liquor.actual <- liquor$liquor
summary(liquor.actual)

# Predicted values of liquor sales using linear trend regression model within the sample set  
liquor.pred.in <- predict(linear.lm, data=liquor_time)  
summary(liquor.pred.in)

# Residuals in prediction
liquor.resid <- liquor.actual - liquor.pred.in
summary(liquor.resid)

# Residual plot
plot(liquor.actual, type="l", axes=FALSE, ylim=c(-500,2740),xlab="Time",ylab="Liquor Sale Residuals", col="red")
lines(liquor.pred.in, col="green")
lines(liquor.resid,col="blue")
abline(h=0,col="black")
abline(h=203,col="black", lty="dashed")
abline(h=-203,col="black", lty="dashed")
box()
axis(side=1,at=c(seq(1,336,by=12)),lab=c(1987:2014),cex.axis=0.7)
axis(side=2,at=c(-203,-100,0,100,203),lab=c(-203,-100,0,100,203),cex.axis=0.7)
axis(side=4,at=c(460,1060,1660,2260,2860),lab=c(460,1060,1660,2260,2860),cex.axis=0.9)

## Log Liquor Sales
logliquor=log(liquor)
names(logliquor)[1] <- "logliquor"
loglsts=ts(data=logliquor,start=c(1987,1),frequency=12)

## Log linear Trend Estimation
time=data.frame(c(1:336))
names(time)[1] <- "time"
logls=data.frame(time,logliquor)
linear.mod=lm(logliquor~time,data=logls)
print(summary(linear.mod))

# diagnostic statistics:
# Residual standard error: 0.1597 on 334 degrees of freedom
# Adjusted R-squared:  0.8428
# Akaike information criterion (AIC): -275.0525
AIC(linear.mod)
# Schwarz information criterion (SIC): -263.6011
BIC(linear.mod)
# Durbin-Watson Statistic: 1.0786
dwtest(linear.mod)

# Residual Plot, Linear Trend Estimation
logls.act=logls$logliquor
logls.pred=predict(linear.mod,logls)
logls.resid=logls.act-logls.pred
logls.resid.plot=1.5*logls.resid+5.8

summary(logls.act)
summary(logls.pred)
summary(logls.resid)
summary(logls.resid.plot)

plot(logls.act,type="l",axes=FALSE,ylim=c(5.2,8.0),xlab="Time",ylab="Log-Linear Liquor Sales",col="red")
lines(logls.pred,col="green")
lines(logls.resid.plot,col="blue")
abline(h=5.8,col="black")
abline(h=6.0,col="black",lty="dashed")
abline(h=5.6,col="black",lty="dashed")
box()
axis(side=1,at=c(seq(1,336,by=12)),lab=c(1987:2014),cex.axis=0.7)
axis(side=2,at=c(5.2,5.5,5.8,6.1,6.4,6.7),lab=c(-0.4,-0.2,0,0.2,0.4,0.6),cex.axis=0.7)
axis(side=4,at=c(6.0,6.5,7.0,7.5,8.0),lab=c(6.0,6.5,7.0,7.5,8.0),cex.axis=0.9)
axis.break(axis=4,breakpos=5.91)  

## Log-Quadratic Trend Estimation
quad.mod=lm(logliquor~time+I(time^2),data=logls)
summary(quad.mod)

# diagnostic statistics:
# Residual standard error: 0.1254 on 333 degrees of freedom
# Adjusted R-squared:  0.9031
# Akaike information criterion (AIC): -436.5158 
AIC(quad.mod)
# Schwarz information criterion (SIC): -421.2473
BIC(quad.mod)
# Durbin-Watson Statistic: 1.7544
dwtest(quad.mod)

##Residual Plot, Log-Quadratic Trend Estimation
logqls.act=logls$logliquor
logqls.pred=predict(quad.mod,logls)
logqls.resid=logqls.act-logqls.pred
logqls.resid.plot=1.5*logqls.resid+5.5

plot(logqls.act,type="l",axes=FALSE,ylim=c(4.9,8.0),xlab="Time",ylab="Log-Quadratic Liquor Sales",col="red")
lines(logqls.pred,col="green")
lines(logqls.resid.plot,col="blue")
abline(h=5.5,col="black")
abline(h=5.7,col="black",lty="dashed")
abline(h=5.3,col="black",lty="dashed")

box()
axis(side=1,at=c(seq(1,336,by=12)),lab=c(1987:2014),cex.axis=0.7)
axis(side=2,at=c(4.9,5.2,5.5,5.8,6.1,6.4),lab=c(-0.4,-0.2,0,0.2,0.4,0.6),cex.axis=0.7)
axis(side=4,at=c(6.0,6.5,7.0,7.5,8.0),lab=c(6.0,6.5,7.0,7.5,8.0),cex.axis=0.9)
axis.break(axis=4,breakpos=5.85)
# --------------------------


# --------------------------

## #4 - Use your preferred model to forecast liquor sales on 2015.01 and construct a 95% forecasting interval for the estimation
newdata = data.frame(time=337:338)

## Create prediction function for Log-Quadratic model 
# quad.mod is regression function for Log-Quad model on Liquor Sales (our preferred model)
liquorquad.pred.out <- predict(quad.mod, newdata, interval="confidence", level = 0.95) 
liquorquad.pred.out
row.names(liquorquad.pred.out) <- c("2015.01", "2015.02")
liquorquad.pred.out
# --------------------------


