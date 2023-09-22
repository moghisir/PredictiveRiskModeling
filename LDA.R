library(ISLR)
names(Smarket)
attach(Smarket)
train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
ld.fit=lda(Direction~Lag1+Lag2, data=Smarket, subset=train)
ld.fit
plot(ld.fit)
lda.pred=predict(ld.fit, Smarket.2005)
names(lda.pred)
Direction.2005= Direction[!train]
lda.class=lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)
lda.pred$posterior[1:10,1]
lda.pred$x[1:20,1]

###KNN
library(class)
dim(Caravan)
attach(Caravan)
standardized.X=scale(Caravan[,-86])
var(Caravan[,1])
var(standardized.X[,1])
test=1:1000
train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1) ## change K
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred,test.Y)


### GLM 
glm.fits=glm(Purchase~., data=Caravan, family=binomial, subset=-test)
glm.probs=predict(glm.fits, Caravan[test,],type="response")
glm.pred= rep("No",1000)
glm.pred[glm.probs>.25]="Yes"
table(glm.pred, test.Y)
