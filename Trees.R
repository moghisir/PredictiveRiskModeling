install.packages("tree")
library(tree)
library(ISLR)
attach(Carseats)
se.seed(8300)
High=ifelse(Sales>8, "Yes", "No")
Carseats=data.frame(Carseats, High)
Carseats$High=as.factor(Carseats$High)

tree.carseats=tree(High~.-Sales, Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.carseats

set.seed(2)
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales, Carseats, subset=train)
tree.pred=predict(tree.carseats, Carseats.test, type="class")
table(tree.pred,High.test)
(86+57)/200


###. PRUNING tree
set.seed(30)
cv.carseats=cv.tree(tree.carseats, FUN=prune.misclass)
warnings()
names(cv.carseats)
cv.carseats
#plot
par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev, type="b")
#Prune
prune.carseats=prune.misclass(tree.carseats,best=8)
plot(prune.carseats)
text(prune.carseats,pretty=0)

###. PREEDICT
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(94+60)/200




###### REGRESSION TREE
library(MASS)
set.seed(20)
train=sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~., Boston, subset=train)
summary(tree.boston)
plot(tree.boston)

text(tree.boston, pretty=0)

##Prune
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type='b')
prune.boston=prune.tree(tree.boston,best=7)
plot(prune.boston)
text(prune.boston, pretty=0)

## CV 
yhat=predict(tree.boston, newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
mean((yhat-boston.test)^2)



#####Random forest
#install.packages("randomForest")
library(randomForest)
#RNGkind(sample.kind = "Rounding")
#set.seed(1, sample.kind = "Rounding")
set.seed(1)
bag.boston=randomForest(medv~.,data=Boston, 
                        subset=train, mtry=13, 
                        importance=TRUE)
bag.boston
yhat.bag=predict(bag.boston,newdata=Boston[-train])
plot(yhat.bag, boston.test)


