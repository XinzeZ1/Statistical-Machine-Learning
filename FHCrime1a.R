## FHCRIME data exploration and modeling hints
##   M Boldin Dec 2021  M365/S465 final exam data

## Data Set 1:   Y & Crime data
## Each row is a US state, except 1, 48 rows
## The Y variable definition is intentionally not provided and  masked (the description is not given)
## One goal is to fit y with the available explanatory variables 
## using linear regression or another general additive model 

## Data Columns
##    ID --ignore--
##    y -- dependent/target variable that is  a share between 0 and 1
##    MedianIncome    Median household income
##    UnempRate       Share of the population that is unemployed (annual average)
##    MetroIndex      Based on share of the population that lives in a metropolitan areas
##    HSDegree1       Share of adults with a high-school degree, 2009
##    DemogShare1 	  Demographic share 1 (full definition masked)
##    DemogShare2 	  Demographic share 2 (full definition masked)
##    PovertyShare1   Share living in poverty
##    InequalityGini  A measure of income inequality in the state
##    CrimesIndex1    Crime index # 1, definition not provided
##    CrimesIndex2    Crime index # 2, definition not provided

## Use GAM for polynomials and splines
library(gam)
setwd("D:/workfiles/465/")
fn1 = 'HCrimes1b.csv'
dfA = read.csv(fn1)
names(dfA)
dim(dfA)  ##should be 48 x 12

## You may need to use a data set that removes any row with missing values
dfB = na.omit(dfA)
dim(dfB) ## should be 44 x 12

## Explore with code like below
summary(dfB)
hist(dfB$y,breaks=6)
hist(dfB$CrimeIndex1,breaks=4)
hist(dfB$CrimeIndex2,breaks=4)
vx = c('y','CrimeIndex1','CrimeIndex2')
dfx = dfB[,vx]
cor(dfx)

##LM using Crime Index and one other variable
fml1 = y ~ CrimeIndex1 + MedianIncome
m1 = lm(fml1,dfA)
summary(m1)

fml2 = y ~ CrimeIndex2 + MedianIncome 
m2 = lm(fml2,dfA)
summary(m2)

##Stepwise Selection

fit.s <- lm(y~., data = dfB)
step(fit.s)

## Note CrimeIndex1 seems to fit better than CrimeIndex2 
##   but has missing values, and this may not hold up in further analysis

## More Questions -- things to try before answering questions in the exam
##  1. What other variables seem to matter ?
##       try a step selection method
##  2. Does cross validation such as leave one out cross validation 
##       help decide on a model form?
##  3. Is there evidence of nonlinear effects with CrimeIndex1 or CrimeIndex2
##       see code below, and try POLY and NS() with different degrees
##  4. Do  residual diagnostics point out any problems
##       see code below for plots and check out leverage
##  5. How confident can you be in using either CrimeIndex1 or CrimeIndex2?
##       is one consistently better or is neither deserve to be included?

## Models to try/explore
## also replace CrimeIndex1 with CrimeIndex2 and try the larger dfA dataset

fml3a = y ~ CrimeIndex1  + MedianIncome + HSDegree1 + DemogShare1 + InequalityGini
fml3b = y ~ CrimeIndex1 + I(CrimeIndex1^2) + MedianIncome + HSDegree1 + DemogShare1 + InequalityGini
fml3c = y ~ ns(CrimeIndex1,3)  + MedianIncome  + HSDegree1 + DemogShare1  + InequalityGini

m3a = lm(fml3a,dfB)
summary(m3a)
plot(m3a)

m3b = lm(formula=fml3b,data=dfB)
summary(m3b)

m3c = gam(formula=fml3c,data=dfB)
summary(m3c)
plot(m3c)
## Note that ns() shows a nonlinear effect in the plot