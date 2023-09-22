# Import Data
Auto <- read.csv("Auto.csv")
Auto$horsepower <- as.numeric(Auto$horsepower)

# Create linear model
auto_ln <- lm(mpg ~ horsepower, data = Auto)

# Summarize the results.
summary(auto_ln)

plot(auto_ln)
pairs(Auto[1:8])

Auto$horsepower[is.na(Auto$horsepower)] <- median(Auto$horsepower, na.rm = TRUE)

cor(Auto[1:8])

auto_ln2 <- lm(mpg ~ ., Auto[1:8])
summary(auto_ln2)

# Produce diagnostic plots of the linear regression fit.
par(mfrow=c(1,1))
plot(auto_ln2)

x = rnorm(100, mean= 0, sd =1)
eps = rnorm(100, mean =0, sd = 0.10)
y = -1+0.5*x+eps
lm.fit2 = lm(y~x)
summary(lm.fit2)
plot(y~x); abline(lm.fit2, col ="red") 
legend("bottomright", c("Regression line"), lwd=1, col="red",bty ="n")


set.seed(1)
x = rnorm(100, mean= 0, sd =1)
eps = rnorm(100, mean =0, sd = 0.50)
y = -1+0.5*x+eps
lm.fit3 = lm(y~x)
summary(lm.fit3)
plot(y~x); abline(lm.fit3, col ="red") 
legend("bottomright", c("Regression line"), lwd=1, col="red",bty ="n")

lm.fit1 = lm(y~x); lm.fit2 = lm(y~x); lm.fit3 = lm(y~x)
confint(lm.fit1);confint(lm.fit2); confint(lm.fit3)



