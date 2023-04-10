clear
set more off

cd /Users/daniellesh/Desktop/ECON2311/Homework

********************************************************************************
* Open growth.dta
log using Lesh.D.Homework02.ECON2311.log, replace 
use /Users/daniellesh/Desktop/ECON2311/Data/growth.dta
*********************

*********************
* 6. 
* a. Construct a scatterplot of average annual growth rate (Growth) on the average trade share (TradeShare). Does there appear to be a relationship between the variables?

scatter growth tradeshare 

* Yes, there appears to be a positive relationship between growth and average tradeshare. As growth tradeshare increases, so does growth. 
*********************

*********************
* b. One country, Malta, has a trade share much larger than the other countries. Find Malta on the scatterplot. Does Malta look like an outlier?

scatter growth tradeshare, mlabel(country)

* Yes, after running the scatterplot code with the mlabel(country), Malta looks like a significant outlier with a tradeshare of around 2 (x-intercept) and a growth of almost 7 (y-intercept). 
*********************

*********************
* c. Using all observations, run a regression of Growth on TradeShare. What is the estimated slope? What is the estimated intercept? Use the regression to predict the growth rate for a country with a trade share of 0.5 and with a trade share equal to 1.0.

reg growth tradeshare, robust

* est. slope = 2.3064
* est. intercept = .6403

* Predict growth rate for a country with trade share of .5: 
display .6403 + 2.3064 * .5
* = 1.7935

* Predict growth rate for a country with trade share of 1: 
display .6403 + 2.3064 * 1
* = 2.9467 
*********************

*********************
* d. Estimate the same regression, excluding the data from Malta. Answer the same questions in (c).

reg growth tradeshare if tradeshare<=1.5, robust

* est. slope = 1.6809
* est. intercept = .9574

* Predict growth rate for a country with trade share of .5: 
display .9574 + 1.6809 * .5 
* 1.7979

* Predict growth rate for a country with trade share of 1: 
display .9574 + 1.6809 * 1
* 2.6383
*********************

*********************
* e. Plot the estimated regression functions from (c) and (d). Using the scatterplot in (a), explain why the regression function that includes Malta is steeper than the regression function that excludes Malta.

graph twoway (lfit growth tradeshare) (lfit growth tradeshare if tradeshare<=1.5) (scatter growth tradeshare)

* The regression function that includes Malta is steeper than the regression function that doesn't because in the regression function that includes Malta, it is accounting for the data generated from the outlier. With having to account for the outlier, it completely changes the slope of the data for one point that is spread as an outlier across the data. The regression function that does not include Malta is less steep because it does not have to account for the outlier in its regression, making the line less steep and more approriate for the data points closer together and more uniform across the spread of the data.
*********************

*********************
* f. Where is Malta? Why is the Malta trade share so large? Should Malta be included or excluded from the analysis?

* Malta is an island nation located in the Meditterean Sea off the coast of Northern Africa and Southern Europe. Malta's large trade share is due to the fact that the nation is a "freight transport site". This means that goods that are imported into Malta's shores are almost immediately exported to other countries. The imports and exports of Malta differ from other countries, and due to this, Malta should not be included in the analysis.  
********************************************************************************


clear
set more off


********************************************************************************
* Open birthweight_smoking.dta
use /Users/daniellesh/Desktop/ECON2311/Data/birthweight_smoking.dta
*********************

*********************
* 7.
* a. 
	* i. What is the average value of Birthweight for all mothers?
		tabstat birthweight, stat(mean)
		* = 3382.934 grams 
	
	* ii. For mothers who smoke?
		tabstat birthweight if smoker==1, stat(mean)
		* = 3178.832 grams  
		
	* iii. For mothers who do not smoke?
		tabstat birthweight if smoker==0, stat(mean)
		* = 3432.06 grams 
*********************

*********************
* b. 
	* i. Use the data in the sample to estimate the difference in average birth weight for smoking and nonsmoking mothers.
		display 3432.06 - 3178.832
	* estimated difference = 253.228 grams 
	
	* ii. What is the standard error for the estimated difference in (i)? 
		tabstat birthweight if smoker==0, stat(semean)
		tabstat birthweight if smoker==1, stat(semean)
		* find the est. difference from this 
		display 24.04206-11.88903 
			* est. difference in SE = 12.15303
	
	* iii. Construct a 95% confidence interval for the difference in the average birth weight for smoking and nonsmoking mothers. 
		*  ON PAPER (7. - b. - iii.)
*********************

*********************
* c. Run a regression of Birthweight on the binary variable Smoker.
	reg birthweight smoker
	
	* i. Explain how the estimated slope and intercept are related to your answers in parts (a) and (b)
		* The estimated slope and intercept are related to my answers in part (a) and (b) because they represent the average birthweight per baby (constant) and the effect smoking has on that baby's birthweight (smoker). These estimated provide insights in showing that smoking and birthweight have a negative relationship, and this regression function further supports my finding made in parts (a) and (b). It also shows B1(hat) and Bo(hat), as it shows the mean average birthweight of a baby from a mother that does not smoke (3432.06) along with the estimated average difference between a smoking mom and a non-smoking mom (-253.2284). 
		
	* ii. Explain how the S.E.(B1hat) is related to your answer in b(ii).
		* The standard error is related to my answer in b(ii) because it is estimating the SE difference between smoking mothers and non-smoking mothers, just as I had done by hand in b(ii). 
	* iii. Construct a 95% confidence interval for the effect of smoking on birth weight.
		* ON PAPER (7. - c. - iii.)

* d. Do you think smoking is uncorrelated with other factors that cause low birth weight? That is, do you think that the regression error term, say ui, has a conditional mean of zero, given Smoking (xi)?
	* ON PAPER (7. - d.)
*********************

*********************
clear
log close 
