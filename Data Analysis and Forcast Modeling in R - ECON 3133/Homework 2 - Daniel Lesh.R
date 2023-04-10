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
## -------------------------------------

# Open Sao Paulo urban traffic database 
traffic_data <- read.table(file.choose(), sep=";", header = TRUE, stringsAsFactors =
                             FALSE)

##Data Manipulation---------------------

# Extract the dependent variable from the data set
# take out this comma and name it "slow_traffic"
slow_traffic <- traffic_data$Slowness.in.traffic....

# Transfering "chr" to "num" and replacing commas to dots in R
# Replace COMMA --> DOT
slow_traffic <- as.numeric(gsub(",", ".", gsub("\\.", "", slow_traffic)))
slow_traffic # these numbers are a % / also our target in the problem
summary(slow_traffic)

# Extract the independent variables from the data set
immobi_bus <- traffic_data$Immobilized.bus
broken_truck <- traffic_data$Broken.Truck
vehicle_excess <- traffic_data$Vehicle.excess
acc_vic <- traffic_data$Accident.victim
run_over <- traffic_data$Running.over
fire_vehicle <- traffic_data$Fire.vehicles
occinv_frieght <- traffic_data$Occurrence.involving.freight
##--------------------------------------

##Regression Analysis---------------------

# Regression 1 ---
reg1 <- lm(slow_traffic~immobi_bus + broken_truck + vehicle_excess + acc_vic + run_over)
summary(reg1)

# Adjusted R-Squared
summary(reg1)$adj.r.squared 

# Standard Error of the Regression
  # Found in regression output

# Durbin-Watson Statistic
dwtest(slow_traffic~immobi_bus + broken_truck + vehicle_excess + acc_vic + run_over)

# Akaike information criterion 
AIC(reg1)

# Schwarz information criterion (SIC) 
BIC(reg1)
# ------------


# Regression 2 ---
reg2 <- lm(slow_traffic~broken_truck + vehicle_excess + acc_vic + run_over + fire_vehicle)
summary(reg2)

# Adjusted R-Squared
summary(reg2)$adj.r.squared 

# Standard Error of the Regression
# Found in regression output

# Durbin-Watson Statistic
dwtest(slow_traffic~broken_truck + vehicle_excess + acc_vic + run_over + fire_vehicle)

# Akaike information criterion 
AIC(reg2)

# Schwarz information criterion (SIC) 
BIC(reg2)
# ------------


# Regression 3 ---
reg3 <- lm(slow_traffic~vehicle_excess + acc_vic + run_over + fire_vehicle + occinv_frieght)
summary(reg3)

# Adjusted R-Squared
summary(reg3)$adj.r.squared 

# Standard Error of the Regression
# Found in regression output

# Durbin-Watson Statistic
dwtest(slow_traffic~vehicle_excess + acc_vic + run_over + fire_vehicle + occinv_frieght)

# Akaike information criterion 
AIC(reg3)

# Schwarz information criterion (SIC) 
BIC(reg3)
# ------------
##--------------------------------------

##Improving Model--------------------------------------
# Another way to improve prediction model is by adding and/or subtracting variables from the regression
  # To improve prediction results, I am going to add the variables Fire and Point of Flooding to the regression model
  # I am also going to remove the variables Running Over and Immobilized Bus from the regression model 

# Add variables Fire and Point of Flooding 
fire <- traffic_data$Fire
point_flood <- traffic_data$Point.of.flooding

# Run new Regression (Reg 4) ---
reg4 <- lm(slow_traffic~broken_truck + vehicle_excess + acc_vic + fire_vehicle + occinv_frieght + fire + point_flood)
summary(reg4)

# Adjusted R-Squared
summary(reg4)$adj.r.squared 

# Standard Error of the Regression
# Found in regression output

# Durbin-Watson Statistic
dwtest(slow_traffic~broken_truck + vehicle_excess + acc_vic + fire_vehicle + occinv_frieght + fire + point_flood)

# Akaike information criterion 
AIC(reg4)

# Schwarz information criterion (SIC)  
BIC(reg4)





