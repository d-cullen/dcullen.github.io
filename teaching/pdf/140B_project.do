set more off
use "E:\Google_Drive\ECON\ECON_140B\Fall 2017\cps_2016.dta" , clear

*************************************
************ Component 1 ************
*************************************

** 1
tab  incwage
sum  incwage
hist incwage
drop if incwage == 9999999

** ln of wage NOTE: will create missing values for 0
gen lnincwage = ln(incwage) 

** 2
gen     white = 0
replace white = 1 if race == 100

gen     black = 0
replace black = 1 if race == 200

gen     married = 0
replace married = 1 if marst == 1 | marst == 2

gen     male = 0 
replace male = 1 if sex == 1

gen     lshs = 0
replace lshs = 1 if educ <73

gen     hs = 0
replace hs =1 if educ >72 & educ < 111

gen     college = 0
replace college = 1 if educ >= 111 & educ < 123

gen     adv = 0 
replace adv = 1 if educ >122 & educ <= 125

gen     above_college = 0
replace above_college = 1 if college == 1 | adv == 1

*************************************
************ Component 2 ************
*************************************

** 1
** Omitted group less than high school
reg incwage hs college adv

**2
gen hs_male            = hs * male
gen white_hs           = white * hs
gen college_male       = college * male
gen white_college      = white * college
gen adv_male           = adv * male
gen white_adv          = white * adv
gen white_male         = white * male
gen white_male_hs      = white * male * hs
gen white_male_college = white * male * college
gen white_male_adv     = white * male * adv


reg incwage male white white_male hs hs_male white_hs 	///
	college college_male white_college 					///
	adv adv_male white_adv 								///
	white_male_hs white_male_college white_male_adv

**3
gen married_male = married * male

reg incwage male white white_male hs hs_male white_hs 	///
	college college_male white_college 					///
	adv adv_male white_adv 								///
	white_male_hs white_male_college white_male_adv		///
	married married_male

*************************************
************ Component 3 ************
*************************************

** test 1st stage region as instrument for going to college
** probably not a good instrument 
reg above_college i.region

ivregress 2sls incwage male white white_male (above_college = i.region)
