# Homework 3 - Question #4
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


# Q4. 
# --------------------------
## #1 - Take Logs and produce a time series plot of the log of the $/Ft exchange rate

## Open Data 
ftusdrate=read.csv("FT_USD_ExchangeRate.csv",header=TRUE)
ftusdrate=na.omit(ftusdrate)
summary(ftusdrate)

## Log FT.USD Rate 
log_ftusdrate = log(ftusdrate$USD.FT)
names(log_ftusdrate)[2] <- "log ftusdrate"
summary(log_ftusdrate)

## Create Time Series Plot for Data 
tslogftusd=ts(data=log_ftusdrate, start=c(1), end=c(502))

par(mfrow=c(1,1),xaxs="i",yaxs="i")
plot(tslogftusd, axes=FALSE, xlim=c(1,502), ylim=c(0.000, 1.000),main="FT vs USD",xlab="Time",ylab="log FT.USD Exhange Rate",col="blue") 
box()
axis(side=1,at=c(0,102,202,302,402,502), lab=c(0,102,202,302,402,502),cex.axis = 0.7)
axis(side=2,at=c(0.2,0.4,0.6,0.8,1), lab=c(0.2,0.4,0.6,0.8,1),cex.axis = 0.9)
# --------------------------


# --------------------------
## #2 - Construct empty data matrix to normalize data 
par(xaxs="i",yaxs="i")
Y1change = ts(start=c(1),end=c(502))
for (i in c(2:length(Y1change))) {
  Y1change[i]=tslogftusd[i]-tslogftusd[i-1]
}
summary(Y1change)
ts.plot(Y1change, main="FT vs USD", ylab="Change in Exchange Rate", xlim=c(1,502),ylim=c(-0.5,0.5))

## Create histogram for Y1change 
hist(Y1change, main="FT vs USD", ylab="Frequncey of Change in Exchange Rate", xlim=c(-.10,.10),ylim=c(0,100))
