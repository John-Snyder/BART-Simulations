options(java.parameters = "-Xmx50g")
library(bartMachine)
numcores <- parallel::detectCores()
set_bart_machine_num_cores(numcores - 1)

library(xgboost)
library(randomForestSRC)
library(dplyr)

#Friedman Data Function
gen_friedman_data = function(n, p, sigma){
   if (p < 5){stop("p must be greater than or equal to 5")}
   X <- matrix(runif(n * p), nrow = n, ncol = p)
   y <- 10 * sin(pi * X[, 1] * X[, 2]) + 20 * (X[, 3] - .5)^2 + 10 * X[, 4] + 5 * X[, 5] + rnorm(n, 0, sigma)
   data.frame(y, X)
}

#Mirsha's Bird Function
gen_MirshasBird_data = function(n, p, sigma){
  if (p < 2){stop("p must be greater than or equal to 2")}
  X <- matrix(runif(n * p), nrow = n, ncol = p)
  X[,1] <- runif(n,-10,0)
  X[,2] <- runif(n,-6.5,0)
  y <- sin(X[,2])*exp((1-cos(X[,1]))^2) + cos(X[,1])*exp((1-sin(X[,2]))^2) + (X[,1] - X[,2])^2 + rnorm(n, 0, sigma)
  data.frame(y, X)
}

nsim <- 5
ntrain <- 1000
ntest <- 500
p_related <- 5
p_null <- 95
sigma <- 1

BART_RMSE <- XGBT_RMSE <- RANF_RMSE <- {}
for(i in 1:nsim)
{
  #Generate Data
  n <- ntrain + ntest
  p <- p_related + p_null
  
  AllData <- gen_MirshasBird_data(n,p,sigma)
  
  y_train <- AllData$y[1:ntrain]
  y_test <- AllData$y[(ntrain+1):n]
  
  X.train <- AllData[1:ntrain,-1] 
  X.test <- AllData[(ntrain+1):n,-1] 
  
  bart.model <- bartMachine(X.train,y_train,
                            num_trees = 200,
                            num_burn_in = 500,
                            num_iterations_after_burn_in = 2000,
                            verbose = FALSE)
  
  RF.model <- randomForest(y~.,data = AllData[1:ntrain,],ntree=500)
  
  ########
  dtrain <- xgb.DMatrix(data = as.matrix(X.train), label= y_train)
  dtest <- xgb.DMatrix(data = as.matrix(X.test), label= y_test)
  
  XG.model <- xgboost(data = dtrain, # the data   
                   booster = "gbtree", objective = "reg:linear",
                   nround = 20) # max number of boosting iterations)  # the objective function
  
  BART.preds <- predict(bart.model, X.test)
  RF.preds <- predict(RF.model, X.test)
  XG.preds <- predict(XG.model, dtest)
  
  sqrt(mean((BART.preds - y_test)^2))
  sqrt(mean((RF.preds - y_test)^2))
  sqrt(mean((XG.preds - y_test)^2))
  
}













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