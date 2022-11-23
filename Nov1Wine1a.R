## KNN LDA & CART 
##  M Boldin  Nov 2021
##  Using WINE1 data

library(ISLR)
library(MASS)
library(class)
library(DescTools)
library(gam)
library(ggplot2)
library(tree)

dfWine <- read.csv("D:/workfiles/465/wine1s.csv")
dim(dfWine)
names(dfWine)
#dfWine

#columns    # 'fixed_acidity''volatile_acidity''citric_acid''residual_sugar''chlorides'
# 'free_sulfur_dioxide''total_sulfur_dioxide''density''pH''sulphates'
# 'alcohol''quality''style'

dfA = dfWine
A = table(dfWine$style)
A
prop.table(A)
sum(A)
nrow(dfA)

## Items  y and X
va = c('fixed_acidity','volatile_acidity','citric_acid','residual_sugar',
       'chlorides','free_sulfur_dioxide','total_sulfur_dioxide',
       'density','pH','sulphates','alcohol','quality','style')
yn= 'style'
vx = va[1:11]

# K-Nearest Neighbors
#set.seed(123)
#n = nrow(dfA)
#n1 = as.integer((2/3)*n)
#train = sample(seq(1:n),n1, replace = FALSE)
#Xtrain = as.matrix(dfA[train,vx])
#Ytrain = as.matrix(dfA[train,yn])
#Xtest = as.matrix(dfA[-train,vx])
#Ytest = as.matrix(dfA[-train,yn])


#set.seed(123)
#knn1=knn(Xtrain,Xtest,Ytrain,k=5)
#table(knn1,Ytest)
#mean(knn1==Ytest)
#(35+117)/2166


# Linear Discriminant Analysis

fml1 = factor(style) ~ volatile_acidity + residual_sugar + chlorides + total_sulfur_dioxide + density

lda1 = lda(fml1,data=dfBtest[-200,])
lda1
#plot(lda1)

## Evaluate predictions 
pred1=predict(lda1)
class=pred1$class
table(class,dfBtest[-200,]$style)

## Run Train --> Test predictions 
set.seed(123)
N = nrow(dfA)
N1 = 5198
  train = sample(1:N, N1)
length(train)

## LDA Cross validation??


## Classification Tree
set.seed(123)
tree1=tree(fml2,dfA)
summary(tree1)

pred2 = predict(tree1,dfA,type="class")
A = table(pred2,dfA[,yn])
A
(A[1,2]+A[2,1])/sum(A)

plot(tree1)
text(tree1,pretty=0)