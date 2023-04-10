clear
set more off

cd /Users/daniellesh/Desktop/ECON2311/Homework

********************************************************************************
* Open growth.dta
log using Lesh.D.Homework03.ECON2311.log, replace 
use /Users/daniellesh/Desktop/ECON2311/Data/birthweight_smoking.dta
*********************

*********************
* 1 
reg birthweight smoker, robust

* 2 
reg birthweight smoker alcohol nprevist, robust 

* 3
reg birthweight smoker alcohol nprevist unmarried, robust 
*********************

*********************
* a. What is the value of the estimated effect of smoking on birthweight in each of the regressions?

* Regression 1: Smoker est. affect = -253.2284

* Regression 2: Smoker est. affect = -217.5801

* Regression 3: Smoker est. affect = -175.3769
*********************

*********************
* b. Construct a 95% confidence interval for the effect of smoking on birthweight, using each of the regressions.

* Regression 1:  
* -253.23 +/- 1.96(26.81)
* = (-305.797, -200.6597)

* Regression 2: 
* -217.58 +/- 1.96(26.11)
* = (-268.7708, -166.3894)

* Regression 3: 
* -175.38 +/- 1.96(26.83)
* = (-227.9777, -122.7761)
*********************

*********************
* c. Does the coefficient on Smoker in regression (1) suffer from omitted variable bias? Explain.

* When comparing Smoker in regression (1) to Smoker in regression (3), it can be seen that the variable suffers from omitted variable bias. This is because when examining the p-value of these other variables, they are statistically significant as well. Thus shows that regression (1) suffers from omitted variable bias. 
*********************

*********************
* d. Does the coefficient on Smoker in regression (1) suffer from omitted variable bias? Explain.

* When comparing Smoker in regression (2) to Smoker in regression (3), it can also be seen that the variable suffers from omitted variable bias. This is because when examining the p-value of these other variables that are included in regression (3), they show an effect on birthweight as well. With this, it shows regression (2) suffers from omitted variable bias. 
*********************

*********************
* e. Consider the coefficient on Unmarried in regression (3). 

* i. Construct a 95% confidence interval for the coefficient.
* -187.1332 +/- 1.96(27.677)
* = (-241.401, -132.865)

* ii. Is the coefficient statistically significant? Explain.

* Yes, the coefficient is statistically significant. This is because the p-value is very low at 0.000. Along with this, the C.I. does not include zero, making it statistically significant as well. 

* iii. Is the magnitude of the coefficient large? Explain

* Yes, the value of the coefficient is large at -187.1332. This shows that if the individual is unmarried, then the birthweight decreases by this magnitude. When examining this coefficient for weight, this is a considerably large magnitude. This shows that this magnitude is large for the coefficient. 

* iv. A family advocacy group notes that the large coefficient suggests that public policies that encourage marriage will lead, on average, to healthier babies. Do you agree? (Hint: Review the discussion of control variables in Section 6.8 Discuss some of the various factors that Unmarried may be controlling for and how this affects the interpretation of its coefficient.)

* From the given value of the coefficients, it can be clearly stated on average that public policies that encourage marriages lead to overall healthier babies. I agree that this is the case. Although "Unmarried" indivudals can mean various things, Unmarried individuals who have babies out of wedlock may not have a "father-figure" for emotional, physical, and financial support of the child. Not having this additional support can lead to much stress, resulting in a decreased of the birthweight of the child. This can also turn the individual to additional alcohol and smoking use while preganant, also affecting the child's birthweight. The control variable places specific constraints on the variables, but it is needed to show as to why some of these unhealthy actions may be taken by an individual while pregrant. 
*********************

*********************
* f. Consider the various other control variables in the data set. Which do you think should be included in the regression? Using a table similar to Table 7.1, examine the robustness of the confidence interval you constructed in (b). What is a reasonable 95% confidence interval for the effect of smoking on birthweight?

***** Building Regression Table in Stata w/ outreg command *****
* Downloaded command using search outreg2 and tutorial on how to run command on UCLA Stata website.

reg birthweight smoker, robust
outreg2 using HW3.doc, replace

reg birthweight smoker alcohol nprevist, robust 
outreg2 using HW3.doc

reg birthweight smoker alcohol nprevist unmarried, robust 
outreg2 using HW3.doc 

* It can be seen with the table, that with more control variables added, R^2 increases. After plugging in for the adjusted R^2 as well, the same effect can be seen. As more variables are added to the regression, the higher and more effective R^2 becomes. This effect of effectiveness when adding control variables to the regression can be seen in the CIs generated in part b. of the problem. As more and more contorl variables are added, the CIs for smoking decrease, along with the magnitude of its coefficient. With this, the reasonable 95% CI for the effect of smoking on birthweight is regression (3). 
*********************

*********************
* g. Construct a joint hypothesis test of Smoker and Alcohol. Test your hypothesis using an F-test and a 95% significance level.

test smoker alcohol 

* H0: B(1) = 0; B(2) = 0 
* H1: B(1) ≠ 0; B(2) ≠ 0
* q = 2 

* 21.79 > 3.00. 

* When performing the F-Test, the critical value at 5% is greater than q at 2 which is 3.00. Since 21.79 > 3.00, it is NOT statistically significant from zero. We can not reject the H0. 
*********************

*********************
clear
log close




























