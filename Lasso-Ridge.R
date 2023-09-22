library(ISLR)
attach(Hitters)
names(Hitters)
Hitters=na.omit(Hitters)
sum(is.na(Hitters))
install.packages("leaps")
library("leaps")
regfit.fulll=regsubsets(Salary~.,Hitters)
summary(regfit.fulll)
regfit.fulll=regsubsets(Salary~.,Hitters, nvmax=19)
regfit.summary=summary(regfit.fulll)
regfit.summary$rsq
par(mfrow=c(2,2))
plot(regfit.summary$adjr2,xlab="number of variables", ylab="RSS", type="l")
points(11, regfit.summary$adjr2[11], col="blue",cex=2, pch=20)

which.min(regfit.summary$bic)
plot(regfit.summary$bic,xlab="number of variables", ylab="BIC", type="l")
points(6, regfit.summary$bic[6], col="red",cex=2, pch=20)

plot(regfit.fulll, scale = "adjr2")
plot(regfit.fulll, scale = "Cp")
plot(regfit.fulll, scale = "r2")

coef(regfit.fulll,6)

#Forward. and Backward
regfit.fwd=regsubsets(Salary~., data= Hitters, nvmax=19, method= "forward")
summary(regfit.fwd)
coef(regfit.fwd,6)

## cross validation for subset selectio
set.seed(1)
train= sample(c(TRUE,FALSE), nrow(Hitters), rep=TRUE)
test= !train
regfit.best= regsubsets(Salary~., data=Hitters[train,], nvmax=19)
coef(regfit.best,7)
test.mat= model.matrix(Salary~., data=Hitters[test,])
val.errors=rep(NA,19)
for (i in 1:19){
  coefi=coef(regfit.best, id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}


## CV subset selection
predict.regsubsets=function(object,newdata, id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object, id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

regfit.best=regsubsets(Salary~., data=Hitters, nvmax=19)
coef(regfit.best,7)

set.seed (1)
folds=sample(1:k, nrow (Hitters),replace=TRUE)
cv.errors=matrix (NA,k, 19, dimnames= list (NULL, paste(1:19)))


for(j in 1:k){
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
  for(i in 1:19){
    pred=predict(best.fit,Hitters[folds==j,],id=i)
    cv.errors[j,i]=mean((Hitters$Salary[folds==j]-pred)^2)
    }
}

## averaging columns of CV
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors, type='b')


# Ridge regression and Lasso
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary
install.packages("glmnet")
library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mode=glmnet(x,y,alpha=0, lambda=grid)
dim(coef(ridge.mode))



Hitters <- na.omit(Hitters)

# Split the dataset into predictors and response variable
x = model.matrix(Salary~., Hitters)[,-1]
y = Hitters$Salary
set.seed(1)
train= sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
plot(ridge.mod)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)

#LASSO
lasso.mod=glmnet(x[train,],y[train],alpha=1)
plot (lasso.mod)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
best.lam=cv.out$lambda.min
lasso.pred=predict(lasso.mod, s=best.lam, newx=x[test,])
mean((lasso.pred-y.test)^2)

#resulting coef LASSO
out=glmnet(x,y,alpha=1, lambda=grid)
lasso.coef=predict(out, type="coefficients", s=best.lam)[1:20,]
lasso.coef




# Set up 10-fold cross-validation with Ridge Regression
set.seed(8300)
cv <- cv.glmnet(x[train], y[train], alpha = 0)
plot (cv)
# Fit ridge regression model using the minimum lambda
lambda_min <- cv$lambda.min
ridge_model <- glmnet(x, y, alpha = 0, lambda = lambda_min)