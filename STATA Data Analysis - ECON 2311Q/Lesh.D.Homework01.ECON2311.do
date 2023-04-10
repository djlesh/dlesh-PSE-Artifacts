clear
set more off

cd /Users/daniellesh/Desktop/ECON2311/Homework
log using Lesh.D.Homework01.ECON2311.log, replace 


*********************
* Open WAGE1.dta
use /Users/daniellesh/Desktop/ECON2311/Data/WAGE1.DTA
*********************

*********************
* a) Find the average education level in the sample. What are the lowest and highest years of education?
tabstat educ, stat (mean min max)
* Average educ level in sample = 12.56274 years 
* Lowest educ level in sample = 0 
* High educ level in sample = 18
**********************

**********************
* b) Find the average hourly wage in the sample. Does is seem high or low?
tabstat wage, stat (mean) 
* Average hourly wage in sample = 5.896103 
* This hourly wage seems relatively low compared to today's standards of a $7.25 minimum wage floor and a US median income of $61,937 in 2018. 
**********************

**********************
* c) The WAGE data are reported in 1976 dollars. Using the Internet or a printed source, find the Consumer Price Index (CPI) for the years 1976 and 2018.
* CPI in 1976 = 56.9 with a 5.7% rate of inflation
* CPI in 2018 = 251.1 with a 2.4% rate of inflation 
* source: https://www.minneapolisfed.org/about-us/monetary-policy/inflation-calculator/consumer-price-index-1913-
**********************

**********************
* d) Use the CPI values from part (c) to find the average hourly wage in 2018 dollars. Now does the average hourly wage seem reasonable?
* 5.87 (1976 hourly wage) * 251.1 (2018 CPI) / 56.9 (1976 CPI) = 25.90
* Yes, after calculating the average hourly wage in 2018 dollars, the hourly wage in 1976 does seem reasonable. It is actually more than the 2018 calculated median hourly wage which was 14.99.
* Source for 2018 median hourly wage: statista.com/statistics/185335/median-hourly-earnings-of-wage-and-salary-workers/
**********************

**********************
* e) How many women are in the sample? How many men?
table female 
* There are 252 female in the sample. (female = 1)
* There are 274 males in the sample. (male = 0) 
**********************

**********************
* f) What is the average # of years of tenure for men? For women? Show your answer to this in a two-by-one table, using the table command.
table female, c(mean tenure)
* Average # of years of tenure for men = 6.47445
* Average # of years of tenure for women = 3.61508
***********************

***********************
* g) Create a two-by-four table showing the average wage, tenure, experience, and education for women and men, using the tabstat command. What does the Total row of the table show us?
tabstat wage tenure exper educ, by(female) stat(mean)
* The Total row of the table shows the mean of wage, tenure, exper, and educ for both women and men combined. When seperated using the tabstat command, it shows the mean of wage, tenure, exper, and educ for women and men seperately. As can be seen in the data, men on average have higher wage, tenure, and exper, but almost the same mean educ when compared to women. 
************************

************************
* clear data 
clear
************************

************************
* close log 
log close 





