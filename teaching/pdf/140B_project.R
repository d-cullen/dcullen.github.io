#install.packages("readstata13")
#install.packages("AER")

library(readstata13) # To Read Stata data file
library(AER)         # TO run ivregress

setwd("E:/Google_Drive/ECON/ECON_140B/Fall 2017")

df <- read.dta13("cps_2016.dta")


###### Component 1 #########
## 1

# Drop observation where incwage = 9999999
hist(df$incwage)
df <- df[df$incwage != 9999999,]

## 2
df$white   <- ifelse(df$race == "white", 1, 0)
df$male    <- ifelse(df$sex  == "male", 1, 0)
df$married <- ifelse(df$marst == "married, spouse present" | df$marst == "married, spouse absent ", 1, 0)
df$hs      <- ifelse(df$educ == "high school diploma or equivalent" | df$educ == "some college but no degree" |
                     df$educ == "associate's degree, occupational/vocational program"  |
                     df$educ == "associate's degree, academic program", 1, 0)
df$college <- ifelse(df$educ == "bachelor's degree", 1, 0)
df$adv     <- ifelse(df$educ == "master's degree" | df$educ == "professional school degree" |
                     df$educ == "doctorate degree", 1, 0)

###### Component 2 #########
## 1

## Omitted group less than high school
r1 <- lm(incwage ~ hs + college + adv, data = df)
summary(r1)

## 2
r2 <- lm(incwage ~ male + white+ male*white + hs + hs*male + hs*white + college + college*male + college*white + 
          adv + adv*male + adv*white + hs*male*white + college*male*white + adv*male*white , data = df)
summary(r2)

## 3 
r3 <- lm(incwage ~ male + white+ male*white + hs + hs*male + hs*white + college + college*male + college*white + 
         adv + adv*male + adv*white + hs*male*white + college*male*white + adv*male*white  + married + married*male,
         data = df)
summary(r3)

###### Component 2 #########
## test 1st stage region as instrument for going to college
## probably not a good instrument 

df$above_college <- ifelse(df$college == 1 | df$adv == 1, 1, 0)
r4 <- lm(above_college ~ I(region), data = df)
summary(r4)

## exogenous regressors that are not instruments must appear before and after the vertical line
tslsmodel <- ivreg(incwage ~ above_college +  male +  white + white*male | 
                     I(region) + male + white + white*male, data = df)
summary(tslsmodel)
