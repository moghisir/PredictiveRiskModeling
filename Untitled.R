library(MASS)
attach(Boston)
library(caret)
library(ggplot2)
library(lattice)
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10 )
ctrl
min(CV_MSE)
model <- regsubsets(crim ~ ., 
                    data = Boston, 
                    nvmax = ncol(Boston) - 1, 
                    method = "exhaustive")




# cross-validating to compare MSE:

CV_MSE <- c()

set.seed(10101)

for (i in 1:(ncol(Boston)-1)) {
  Boston_temp <- Boston[ ,c("crim", names(coef(model, id = i)[-1]))]
  model_temp <- train(crim ~ ., 
                      data = Boston_temp, 
                      method = "lm", 
                      trControl = ctrl)
  CV_MSE[i] <- model_temp$results$MSE
}