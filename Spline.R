library(ISLR)
attach(Wage)
fit=lm(wage~poly(age,4), data=Wage)  #Standardize
fit1=lm(wage~poly(age,4,raw=T), data=Wage)
fit2=lm(wage~cbind(age,age^2,age^3,age^4), data=Wage)
fit3=lm(wage~age+I(age^2)+I(age^3)+I(age^4), data=Wage)
summary(fit3)

agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit, newdata = list(age=age.grid), se=TRUE)
se.band=cbind(preds$fit+ 2*preds$se.fit, preds$fit-2*preds$se.fit)

preds2=predict(fit3, newdata = list(age=age.grid), se=TRUE)
max(abs(preds$fit-preds2$fit))
summary(preds)

####plot######
par(mfrow=c(1,2), mar=c(4.5,4.5,1,1), oma=c(0,0,4,0))
plot(age, wage, xlim=agelims, cex=.5, col="darkgray")
title("Degree 4-polynomial", outer=T)
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid,se.band, lwd=1, col="blue", lty=3)
#############
# ANOVA
fit.1=lm(wage~age, data=Wage)
fit.2=lm(wage~poly(age,2), data=Wage)
fit.3=lm(wage~poly(age,3), data=Wage)
fit.4=lm(wage~poly(age,4), data=Wage)
fit.5=lm(wage~poly(age,5), data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)
coef(summary(fit.5))

fit=glm(I(wage>250)~poly(age,4), data=Wage, family=binomial)
preds=predict(fit, newdata=list(age=age.grid), se=T)
preds
pfit=exp(preds$fit)/(1+exp(preds$fit))
se.band.logit=cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
se.bands=exp(se.band.logit)/(1+exp(se.band.logit))
se.bands
preds=predict(fit, newdata=list(age=age.grid), typre="response", se=T)
preds #same method but will give negative probs

#stepwise
table(cut(age,4))
fit=lm(wage~cut(age,4), data=Wage)
coef=(summary(fit))
coef

###Spline BS function 
library(splines)
fit=lm(wage~bs(age,knots=c(25,40,60)), data=Wage) #manually specify knots
pred=predict(fit, newdata=list(age=age.grid), se=T)
plot(age,wage,col="gray")
lines(age.grid,pred$fit,lwd=2)
summary(fit)

## ns function to specify knots automatically
attr(bs(age,df=10), "knots") 
fit2=lm(wage~ns(age,df=4), data=Wage)
pred2=predict(fit2, newdata=list(age=age.grid), se=T)
summary(fit2)
lines(age.grid, pred2$fit, col="red", lwd=2)

fit=smooth.spline(age,wage,df=16)
fit2=smooth.spline(age,wage,cv=TRUE)
fit2$df
lines(fit,col="red", lwd=2)
lines(fit2, col="blue", lwd=2)


## local regression
install.packages("gam")
library(gam)
gam.m3=gam(wage~s(year,4)+s(age,5)+education, data=Wage)
summary(gam.m3)
par(mfrow=c(1,3))
plot(gam.m3, se=TRUE, col="blue")
gam1=lm(wage~ns(year,4)+ns(age,5)+education, data=Wage)
plot.Gam(gam1, se=TRUE, col="red")

## local regression on GAM 2 dimensional
## use akima library

