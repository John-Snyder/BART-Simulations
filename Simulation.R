library(bartMachine)
library(xgboost)
library(dplyr)

p <- 5
p0 <- 95
prior <- c(rep(10, times = p), rep(1, times = p0))
gen_friedman_data = function(n, p, sigma){
   if (p < 5){stop("p must be greater than or equal to 5")}
   X <- matrix(runif(n * p), nrow = n, ncol = p)
   y <- 10 * sin(pi * X[, 1] * X[, 2]) + 20 * (X[, 3] - .5)^2 +
   10 * X[, 4] + 5 * X[, 5] + rnorm(n, 0, sigma)
   data.frame(y, X)
   }
ntrain <- 500
sigma <- 2
fr_data <- gen_friedman_data(ntrain, p + p0, sigma)
y <- fr_data$y
X <- fr_data[, 2:101] %>% data.frame
ntest <- 500
fr_data <- gen_friedman_data(ntest, p + p0, sigma)
Xtest <- fr_data[, 2:101] %>% data.frame
ytest <- fr_data$y

#We construct a default bartMachine model as well as a bartMachine model with the informed
#prior and compare their performance using the RMSE metric on a test set of another 500
#observations.

bart_machine <- bartMachine(X, y)
bart_machine_informed <- bartMachine(X, y, cov_prior_vec = prior)
bart_predict_for_test_data(bart_machine, Xtest, ytest)$rmse


########
dtrain <- xgb.DMatrix(data = as.matrix(X), label= y)
dtest <- xgb.DMatrix(data = as.matrix(Xtest), label= ytest)

model <- xgboost(data = dtrain, # the data   
                 booster = "gbtree", objective = "reg:linear",
                 nround = 20) # max number of boosting iterations)  # the objective function

pred.xg <- predict(model, dtest)

# get & print the classification error
sqrt(mean((ytest - pred.xg)^2))

xgb.fit <- xgboost(data = data.matrix(Xtrain), label = XSalePrice,
                   booster = "gbtree", objective = "reg:linear",
                   colsample_bytree = 0.2, gamma = 0.0,
                   learning_rate = 0.05, max_depth = 6,
                   min_child_weight = 1.5, n_estimators = 7300,
                   reg_alpha = 0.9, reg_lambda = 0.5,
                   subsample = 0.2, seed = 42,
                   silent = 1, nrounds = 25)

pred <- predict(model, dtest)

# get & print the classification error
err <- mean(as.numeric(pred > 0.5) != test_labels)
print(paste("test-error=", err))
xgb.pred <- predict(xgb.fit, data.matrix(Xtrain))
postResample(xgb.pred, XSalePrice)