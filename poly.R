library (MASS)
library(ISLR)
attach(Boston)
lm.fit= lm(medv~lstat, data=Boston)
names(Boston)
names(lm.fit)
confint(lm.fit)
predict(lm.fit, data.frame(lstat=c(5,10)), interval = 'confidence')
plot(lstat,medv, col= 'red')
par(mfrow=c(2,2))
plot(lm.fit)
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))
lm.fit=lm(medv~lstat+age, data= Boston)
summary(lm.fit)
install.packages('car')
library(car)

lm.fit=lm(medv~., data= Boston)
vif(lm.fit)

lm.fit=lm(medv~.-age, data=Boston)
summary(lm.fit)

lm.fit=lm(medv~lstat*age, data=Boston)
summary(lm.fit)

lm.fit2=lm(medv~lstat+I(lstat^2), data=Boston)
lm.fit=lm(medv~lstat, data=Boston)
anova(lm.fit2,lm.fit)
par(mfrow=c(2,2))
plot(lm.fit2)

lm.fit5=lm(medv~poly(lstat,5), data=Boston)
summary(lm.fit5)

LoadLibrariesforMe=function(){
  library(ISLR)
  library(MASS)
  print('libraries are loaded')
}
LoadLibrariesforMe()
