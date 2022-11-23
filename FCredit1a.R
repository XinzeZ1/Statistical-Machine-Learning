## CREDIT data exploration and modeling code
##   M Boldin Dec 2021  M365/S465 final exam data

## Data Set 2:   Credit Approval as y variable (0,1 or YES/NO
## 500 rows of data for creidt applications
## Data columns:
##  ID	
##  Approved      	YES or NO for credit approval
##  ApprovedDummy	  1 when Approved is YES
##  Age	            Applicant age
##  Debt            Debt currently owed  by applicant	(thousands)
##  YearsEmployed   Years employed at current job
##  PriorDefault	  1 if default on prior loan
##  CreditScore	    Credit score, higher is better
##  Income          Monhtly income (thousands)

## This set:  NO YES 
##           275 225 

## Logit model results you should be able to match
##

#Call:
#  glm(formula = fml, family = "binomial", data = dfC)

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.6778  -0.3569  -0.2531   0.5446   2.6067  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)    0.49380    0.45737   1.080  0.28030    
#Age           -0.00577    0.01308  -0.441  0.65914    
#Debt          -0.01393    0.02755  -0.506  0.61305    
#YearsEmployed  0.09638    0.05451   1.768  0.07703 .  
#PriorDefault  -3.72544    0.34657 -10.749  < 2e-16 ***
#CreditScore    0.14165    0.04867   2.911  0.00361 ** 
#Income         0.36067    0.11103   3.248  0.00116 ** 

library(DescTools)
setwd("D:/workfiles/465/")
## Read data
fn1 = 'Credit2c.csv'
dfC = read.csv(fn1)
dftest = read.csv("Credit2test.csv")

## Check data
names(dfC)
dim(dfC)
table(dfC['Approved'])
summary(dfC)

## Logistic Model 1
fml = factor(Approved) ~ Age + Debt + YearsEmployed + + PriorDefault + CreditScore + Income       
lm(fml,dfC)
m1 = glm(fml,dfC,family='binomial')
summary(m1)

PseudoR2(m1)

## To do  before exam
##  * Find and interpret Psuedo-R2 for this model/case
##      You can use DescTools::PseudoR2(m1)
##  * Determine error rate, in sample
##  * Try transformation of Age or drop as an explanatory variable
##  *     and  Debt/(1+Income) or other transformations
##             (1+Income) is needed because Income=0 is possible 
##  * Categorize using another method: LDA, Tree, Neural Net
##      and compare to logistic model

##  In the exam you will be given a Holdout set to judge the accuracy
##   below the code creates a pseudo-hold-out set for practice

#xsample = sample(seq(1,500),30,replace=TRUE)
#dfC2 = dfC[xsample,]
yp = predict(m1,newdata=dftest,type='response')
length(yp)
yp

contrasts(as.factor(dftest$Approved))
glm.pred=rep("YES",152)
glm.pred[yp <0.50]=" NO"
table(glm.pred,as.factor(dftest$Approved))

## Try LDA, Tree, or NN model to see if a better error rate 
##  is found  using the same explanatory variables

