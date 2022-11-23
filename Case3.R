library(ISLR)
library(MASS)
library(class)
library(DescTools)



dfB <- read.csv("D:/workfiles/465/wine1r.csv", header = T,stringsAsFactors=T)
dim(dfB)

n = nrow(dfB)
#dfB$style[which(dfB$style=='red')] = 1
#dfB$style[which(dfB$style=='white')] = 0

##Split into Train and Test sets 
set.seed(123)
x = sample(x=seq(1,n),size=5198,replace=FALSE)
dfBtrain = dfB[x,]
dfBtrain.label = dfBtrain[,13]
dfBtest = dfB[-x,]
dfBtest.label = dfBtest[,13]


n
dim(dfBtrain)
dim(dfBtest)

table(dfBtrain$style)
table(dfBtest$style)

dfA = dfBtrain
hist(dfA$fixed_acidity)
boxplot(dfA$fixed_acidity)

fml = style ~  fixed_acidity + volatile_acidity + citric_acid + residual_sugar + chlorides + free_sulfur_dioxide + total_sulfur_dioxide + density + pH + sulphates + alcohol + quality
winefit = glm(fml,data=dfB,family=binomial)
summary(winefit)
coef(winefit)
summary(winefit)$coef
cor(dfA[,1:12])

PseudoR2(winefit,which="McFadden")
PseudoR2(winefit,which='all')

wine.probs=predict(winefit,type="response")
wine.probs[1:10]
contrasts(dfB$style)
glm.pred=rep("white",5198)
glm.pred[wine.probs >0.99]=" red"
table(glm.pred,dfB$style)


##LDA

fml1 = factor(style) ~ volatile_acidity + residual_sugar + chlorides + total_sulfur_dioxide + density + free_sulfur_dioxide

lda1 = lda(fml1,data=dfBtrain)
lda1
plot(lda1)

## Evaluate predictions 
pred1=predict(lda1,dfBtest)
class=pred1$class
table(class,dfBtest$style)
