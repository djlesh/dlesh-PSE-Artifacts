# Homework 4
# Date: 03/23/2021
# Course: ECON 3313
# Author: Daniel Lesh    

# Clear memory
rm(list = ls())

# Mac
setwd("/Users/daniellesh/Desktop/ECON3313/Homeworks")

########------------------------------------------------------------------------
##Packages
library(forecast)
library(plotrix)
library(lmtest)
library(stats)
########------------------------------------------------------------------------

########------------------------------------------------------------------------
# Liquor Sales 1987.01 - 2014.12
liquor=read.csv("DataLiquor.csv",header=TRUE)

# Drop the NA (null values) in the data set
liquor=na.omit(liquor)

# Build time variable to regress on log_liquor
names(liquor)[1] <- "liquor"
lsts=ts(data=liquor,start=c(1987,1),frequency=12)
lsts
summary(lsts)

# Log Liquor Sales variable
log_liquor=log(liquor)

# Change names of vars in summary stats to log_liquor
names(log_liquor)[1] <- "log_liquor"
loglsts=ts(data=log_liquor,start=c(1987,1),frequency=12)
summary(loglsts)

# Buid linear trend estimation variable 'time'
time=data.frame(c(1:336))
names(time)[1] <- "time"

# Combine time-trend regressor with log_liquor variable
loglsts=data.frame(time,log_liquor)

# Run regression for log-linear trend model (log_liquor on time) 
log.linear=lm(log_liquor~time,data=loglsts)
summary(log.linear)

# Akaike information criterion (AIC): -275.0525
AIC(log.linear)
# Schwarz information criterion (SIC): -263.6011
BIC(log.linear)
# Durbin-Watson Statistic: 1.0786
dwtest(log.linear)
########------------------------------------------------------------------------

########------------------------------------------------------------------------
## Build pure seasonal model: Regress log_liquor on twelve seasonal dummies.
loglsts=ts(data=log_liquor,start=c(1987,1),frequency=12)
month = seasonaldummy(loglsts)

# Twelve seasonal dummies without an intercept
Dec = matrix(data=rep(c(rep(0,11),1),28),nrow=336,ncol=1)

# Combine December dummies into data frame 
logls.seasonal = data.frame(cbind(log_liquor,month,Dec))

# Run regression with log_liquor on twelve seasonal dummies
log.seasonal.mod1=lm(log_liquor~.-1,data=logls.seasonal)
summary(log.seasonal.mod1)

# Akaike information criterion (AIC): 
AIC(log.seasonal.mod1)
# Schwarz information criterion (SIC): 
BIC(log.seasonal.mod1)
# Durbin-Watson Statistic: 
dwtest(log.seasonal.mod1)
########------------------------------------------------------------------------

########------------------------------------------------------------------------
## Build linear seasonal model: Regress log_liquor on time, twelve seasonal dummies.

# Rebuild twelve seasonal dummies without an intercept
Dec = matrix(data=rep(c(rep(0,11),1),28),nrow=336,ncol=1)

# Recombine December dummies into data frame 
logls.seasonal = data.frame(cbind(log_liquor,time,month,Dec))

# Run regression with log_liquor on time, twelve seasonal dummies
log.seasonal.mod2=lm(log_liquor~.-1,data=logls.seasonal)
summary(log.seasonal.mod2)

# Akaike information criterion (AIC): 
AIC(log.seasonal.mod2)
# Schwarz information criterion (SIC): 
BIC(log.seasonal.mod2)
# Durbin-Watson Statistic: 
dwtest(log.seasonal.mod2)
########------------------------------------------------------------------------

########------------------------------------------------------------------------
## Build quadratic seasonal model: Regress log_liquor on time,time^2, twelve seasonal dummies.

# Square variable 'time' 
time2 <- time^2

# Add 'time2' to logqls.seasonal data fram 
logqls.seasonal = data.frame(cbind(log_liquor,time,time2,month,Dec))
head(logqls.seasonal)

# Run regression with log_liquor on time, time^2, twelve seasonal dummies
quad.seasonal.mod=lm(log_liquor~.-1, data = logqls.seasonal)
summary(quad.seasonal.mod)

# Akaike information criterion (AIC): 
AIC(quad.seasonal.mod)
# Schwarz information criterion (SIC): 
BIC(quad.seasonal.mod)
# Durbin-Watson Statistic: 
dwtest(quad.seasonal.mod)
########------------------------------------------------------------------------

########------------------------------------------------------------------------
## Residual Plot --> Log-Quadratic Trend Estimation (Fourth Regression) 
logqls.seasonal.act = logls.seasonal$log_liquor
logqls.seasonal.pred = predict(quad.seasonal.mod,logqls.seasonal)
logqls.seasonal.resid = logqls.seasonal.act - logqls.seasonal.pred
logqls.seasonal.resid.plot = 7*logqls.seasonal.resid+5.55

# Run summary for each logqls.seasonal act, pred, resid
summary(logqls.seasonal.act)
summary(logqls.seasonal.pred)
summary(logqls.seasonal.resid)

# Build Residual Plot for Log-Quadratic Trend Estimation with Seasonal Dummies
plot(logqls.seasonal.act,type="l",axes=FALSE,ylim=c(4.5,8.0),main="Residual Plot for Log-Quadratic Trend Estimation",xlab="Years",ylab="Residual Values",col="red")
lines(logqls.seasonal.pred,col="green")
lines(logqls.seasonal.resid.plot,col="blue")
abline(h=5.55,col="black")
abline(h=5.9,col="black",lty="dashed")
abline(h=5.2,col="black",lty="dashed")
box()
axis(side=1,at=c(seq(1,336,by=12)),lab=c(1987:2014),cex.axis=0.7)
axis(side=2,at=c(4.5,4.85,5.2,5.55,5.9,6.25,6.6),lab=c(-0.15,-0.10,-0.05,0,0.05,0.10,0.15),cex.axis=0.7)
axis(side=4,at=c(6.0,6.5,7.0,7.5,8.0),lab=c(6.0,6.5,7.0,7.5,8.0),cex.axis=0.9)
axis.break(axis=4,breakpos=5.8)
########------------------------------------------------------------------------

########------------------------------------------------------------------------
## Forecast the liquor sales on 2015.01-2015.12 based on the linear trend and seasonal model

## Seasonal Pattern
moncoef = data.frame(coef(log.seasonal.mod2))
moncoef <- moncoef[-1,]
summary(moncoef)

# Seasonal Pattern
plot(moncoef,type="l",axes=FALSE,main="Seasonal Patterns Factor Plot",xlab="Estimated Seasonal Factors",ylab="Factor",col="blue")
box()
axis(side=1,at=c(1:12),lab=c(paste("M",1:12)),cex.axis=0.75)
axis(side=2,at=c(6.3,6.4,6.5,6.6,6.7,6.8,6.9),lab=c(6.3,6.4,6.5,6.6,6.7,6.8,6.9),cex.axis=1)

# We create a new data frame that set the time on 2015.01-2015.12. (out-sample-forecasting)
time.new <- data.frame(c(337:348))
data.frame(cbind(time.new,month,Dec))
newdata = data.frame(cbind(time.new,month[1:12,],Dec[1:12,]))
names(newdata)[1] <- "time"
names(newdata)[13] <- "Dec"
newdata

# We now apply the predict function and set the predictor variable in the newdata argument. We also set the interval type as "confidence", 
# and use the default 0.95 confidence level. 
liquor.pred.out <- predict(log.seasonal.mod2, newdata, interval="confidence", level = 0.95) 
row.names(liquor.pred.out) <- c("2015.01", "2015.02", "2015.03","2015.04","2015.05","2015.06",
                                "2015.07","2015.08","2015.09", "2015.10","2015.11", "2015.12")
liquor.pred.out
















