install.packages("pls")
library(pls)
library(ISLR)
set.seed(8300)
sum(is.na(Hitters$Salary))

#PCR
pcr.fit=pcr(Salary~., data=Hitters, scale=TRUE, validation= "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type="MSEP")

#PCR on training and test
x=model.matrix(Salary~., Hitters)[,-1]
y=(Hitters$Salary)
set.seed(8200)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
pcr.fit=pcr(Salary~., data=Hitters,subset=train, scale=TRUE, validation= "CV")
validationplot(pcr.fit, val.type="MSEP")
pcrpred=predict(pcr.fit, x[test,], ncomp=3)
summary (pcrpred)
mean((pcrpred-y.test)^2)

#PCR on full data
pcr.fit=pcr(y~x,scale=TRUE, ncomp=3 )
summary(pcr.fit)

#####################
#PLS
plsr.fit=plsr(Salary~., data=Hitters,subset=train, scale=TRUE, validation= "CV")
summary(plsr.fit)
validationplot(plsr.fit)

pls.pred=predict(plsr.fit, x[test,], ncomp=1)
mean((pls.pred-y.test)^2)

#on full dataset using ncom=1
pls.fit=plsr(y~x,scale=TRUE, ncomp=1 )
summary(pls.fit)
