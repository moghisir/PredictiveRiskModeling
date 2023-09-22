library(ISLR)
library(boot)

# Load Auto dataset
data(Auto)
attach(Auto)
set.seed(1)
train=sample(392,196)
  # Fit a linear regression model to predict mpg
lm.fit=lm(mpg ~ horsepower, data=Auto, subset=train)
mean((mpg - predict(lm.fit,Auto))[-train]^2)
lm.fit2=lm(mpg~poly(horsepower,2), data=Auto, subset=train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)


# Compute cross-validation error rate
cv.error=cv.glm(Auto, glmfit)$delta[2]

# Print the cross-validation error rate
cat("Cross-validation error rate:", cv.error, "\n")
