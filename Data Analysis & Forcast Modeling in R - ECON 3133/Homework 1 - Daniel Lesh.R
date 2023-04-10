# Homework 1 
# Date: 02/11/2021
# Course: ECON 3313
# Author: Daniel Lesh

# Clear memory
rm(list = ls())

# Mac
setwd("/Users/daniellesh/Desktop/ECON3313/Homeworks")

##Packages------------------------------
library(lmtest)
library(sandwich)
library(readxl)
## -------------------------------------------

# Open insurance.xlsx
insurance_data <- read_excel("insurance.xlsx")


# 1) 
# Rename Vars: 
Charges <- insurance_data$charges
Age <- insurance_data$age
Children <- insurance_data$children

# Run MLR and set-up CI: 
reg1 <- lm(Charges~Age+Children, data = insurance_data)
summary(reg1)
newdata = data.frame(Age=30, Children=2)
predict(reg1, newdata, interval="confidence", level = 0.95) 


# 2)
# Create quadratic w/ Age
Age_2 <- Age^2

# Run MLR and set-up CI: 
reg2 <- lm(Charges~Age+Age_2+Children, data = insurance_data)
summary(reg2)
newdata2 = data.frame(Age=30, Age_2=30^2, Children=2)
predict(reg2, newdata2, interval="confidence", level = 0.95) 


# 3)
# Log insurance_data
insurance_data$log_age <- log(insurance_data$age)
  
# Run MLR and set-up CI: 
reg3 <- lm(Charges~Age+log_age+Children, data = insurance_data)
summary(reg3)
newdata3 = data.frame(Age=30, log_age=30, Children=2)
predict(reg3, newdata3, interval="confidence", level = 0.95) 






