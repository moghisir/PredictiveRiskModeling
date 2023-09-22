

Auto_df = Auto
fit_data = lm(Auto_df$mpg~Auto_df$horsepower, data=Auto_df)
summary(fit_data)
# *** in data mark the significance level. More * means more significance.
#Since the difference of residuals (min-max) is smaller and mean to is smaller than 0,
#so the regression is providing good results. Since smaller the residuals better is the regression
#In Intercept section we see for every increase in horsepower -0.157845 is increased in mpg.
# The F-statistic value is high which means at least one variable is there which is siginificant than 0.

#PArt b
plot(Auto_df$mpg~Auto_df$horsepower,main='Scatter Plot and Regression Line',xlab='Predictor',ylab='Response')
abline(coefficients(fit_data),col='red')

#Part c
par(mfrow = c(2,2))
plot(fit_data)
#Here in Q-Q plot we see that the points are on a straight dashed line and are not deviating below the line
#SO this means residuals in the regression fit are almost normally distributed.
# In Residual Fitted plot the linearity of the residuals is voilated due to which the which plot investigation is required further.
#The plot of Residuals and Leverage shows few outliers (117,34) and high leverage points which are below and above 2 and -2

lm.fit <- lm(mpg ~ horsepower, data = Auto)
summary(lm.fit)
data()
Auto
library("ISLR")
lm.fit <- lm(mpg ~ horsepower, data = Auto)
summary(lm.fit)

attach(Auto)
na.omit(Auto)
