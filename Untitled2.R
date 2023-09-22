library(ISLR)
attach(College)
set.seed(3)
train_index <- sample(1:nrow(College), round(nrow(College) * 0.7))

train <- College[train_index, ]
nrow(train) / nrow(College)

test <- College[-train_index, ]
nrow(test) / nrow(College)

model_linear <- lm(Apps ~ ., data = train)
summary(model_linear)

library(caret)
train_mat <- dummyVars(Apps ~ ., data = train, fullRank = F) %>%
  predict(newdata = train) %>%
  as.matrix()

test_mat <- dummyVars(Apps ~ ., data = test, fullRank = F) %>%
  predict(newdata = test) %>%
  as.matrix()

set.seed(3)

model_ridge <- cv.glmnet(y = train$Apps, 
                         x = train_mat, 
                         alpha = 0, 
                         lambda = 10^seq(2,-2, length = 100), 
                         standardize = TRUE, 
                         nfolds = 5)

data.frame(lambda = model_ridge$lambda, 
           cv_mse = model_ridge$cvm) %>%
  ggplot(aes(x = lambda, y = cv_mse)) + 
  geom_point() + 
  geom_line() + 
  geom_vline(xintercept = model_ridge$lambda.min, col = "deepskyblue3") +
  geom_hline(yintercept = min(model_ridge$cvm), col = "deepskyblue3") +
  scale_x_continuous(trans = 'log10', breaks = c(0.01, 0.1, 1, 10, 100), labels = c(0.01, 0.1, 1, 10, 100)) + 
  scale_y_continuous(labels = scales::comma_format()) + 
  theme(legend.position = "bottom") + 
  labs(x = "Lambda", 
       y = "Cross-Validation MSE", 
       col = "Non-Zero Coefficients:", 
       title = "Ridge Regression - Lambda Selection (Using 5-Fold Cross-Validation)")

model_ridge_best <- glmnet(y = train$Apps,
                           x = train_mat,
                           alpha = 0, 
                           lambda = 10^seq(2,-2, length = 100))

ridge_pred <- predict(model_ridge_best, s = model_ridge$lambda.min, newx = test_mat)
(ridge_mse <- mean((ridge_pred - test$Apps)^2))