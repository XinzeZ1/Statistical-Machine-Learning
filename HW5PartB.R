library(ISLR)
library(MASS)
library(class)
library(DescTools)
library(ggplot2)

dfB <- read.csv("D:/workfiles/465/LFPart2b.csv")
dim(dfB)

n = nrow(dfB)

##Split into Train and Test sets 
set.seed(123)
x = sample(x=seq(1,n),size=600,replace=FALSE)
dfBtrain = dfB[x,]
dfBtest = dfB[-x,]

## Check dimemsions rows: 600 + 153 = 753 ?
n
dim(dfBtrain)
dim(dfBtest)

table(dfBtrain$LFPchoice)
table(dfBtest$LFPchoice)

dfA = dfBtrain
plot(dfA$LFPchoice)
hist(dfA$Educ)
boxplot(dfA$Educ)

A = table(dfA$LFPchoice,dfA$Dkidslt6)

A
(A[1,2] + A[2,1])
(A[1,2] + A[2,1]) / sum(A)
prop.table(A,margin=2)


fml = LFPchoice ~  Age + poly(Educ,4) + Dkidslt6 + huseduc + huswage
lfpfit = glm(formula=fml,data=dfA,family='binomial')
summary(lfpfit)

PseudoR2(lfpfit,which="McFadden")
PseudoR2(lfpfit,which='all')

## Prediction
p = predict(lfpfit,type = 'response')
e = dfA$LFPchoice - p
rss = sum(e^2)
n = length(e)
rmseb = sqrt(rss/n)
R2b = cor(dfA$LFPchoice,p)^2

## Try KNN, LDA QDA

# K-Nearest Neighbors
vx = c('Educ','Age','Dkidslt6','huswage')
Xtrain = as.matrix(dfBtrain[,vx])
Ytrain = as.matrix(dfBtrain[,'LFPchoice'])
Xtest = as.matrix(dfBtest[,vx])
Ytest = as.matrix(dfBtest[,'LFPchoice'])

set.seed(123)
knn.pred=knn(Xtrain,Xtest,Ytrain,k=5)
table(knn.pred,Ytest)
mean(knn.pred==Ytest)

# Linear Discriminant Analysis
ldafit=lda(fml,data=dfA)
ldafit
names(ldafit)
#plot(ldafit)
lda_class=predict(ldafit)$class
table(dfA$LFPchoice,lda_class)

# Quadratic Discriminant Analysis
qdafit=qda(fml,data=dfA)
qdafit
names(qdafit)
