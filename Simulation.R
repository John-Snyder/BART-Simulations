options(java.parameters = "-Xmx50g")
library(bartMachine)
numcores <- parallel::detectCores()
set_bart_machine_num_cores(numcores - 1)

library(xgboost)
library(randomForestSRC)
library(caret)
library(tidyverse)
library(rpart)
source("CV_Params.R")
source("Data_Generation.R")

nsim <- 3
ntrain <- 2000
ntest <- 500
p_null <- 0
sigma <- 1
AllDataTypes <- c("Friedman","Mirsha","Exp","Linear")

Sim_Result_Empty <- data.frame(RMSE = rep(NA,nsim),
                         TIME = rep(NA,nsim))
All_Res_Empty <- list(BART = Sim_Result_Empty,
                      XGBoost = Sim_Result_Empty,
                      RandomF = Sim_Result_Empty,
                      LinReg = Sim_Result_Empty)

All_Results <- list(All_Res_Empty,All_Res_Empty,All_Res_Empty,All_Res_Empty)
names(All_Results) <- AllDataTypes

Mean_Results <- SD_Results <- Time_Results <- data.frame(BART = rep(NA,4),
                                                         XGboost = rep(NA,4),
                                                         RandomF = rep(NA,4),
                                                         LinReg = rep(NA,4))
rownames(Mean_Results) <- rownames(SD_Results) <- c("Friedman","Mirsha","Exp","Linear")


n <- ntrain + ntest
for(Cur_Fun in AllDataTypes)
{
  for(i in 1:nsim)
  {
    #Generate Data
    AllData <- Generate_Data(nonlin_f = Cur_Fun, n, p_null, sigma)
      
    y_train <- AllData$y[1:ntrain]
    y_test <- AllData$y[(ntrain+1):n]
    
    X.train <- AllData[1:ntrain,-1] 
    X.test <- AllData[(ntrain+1):n,-1] 
      
    #Fit BART model, automatically cross-validating
    All_Results[[Cur_Fun]]$BART$TIME[i] <- system.time(
    bart.model <- bartMachineCV(X.train,y_train,
                              #num_trees = 200,
                              num_burn_in = 1000,
                              num_iterations_after_burn_in = 10000,
                              verbose = FALSE)
    )["elapsed"]
      
    #Fit RandomForest
    All_Results[[Cur_Fun]]$RandomF$TIME[i] <- system.time(
    RF.model <- randomForest(y~., data = AllData[1:ntrain,], ntree=500)
    )["elapsed"]
      
    ########
    dtrain <- xgb.DMatrix(data = as.matrix(X.train), label= y_train)
    dtest <- xgb.DMatrix(data = as.matrix(X.test), label= y_test)
    
    All_Results[[Cur_Fun]]$XGBoost$TIME[i] <- system.time(
    XG.model <- train(y~., data = AllData[1:ntrain,], method = "xgbTree",
                    trControl=trctrl,
                    tuneGrid = tune_grid_XGboost,
                    tuneLength = 20)$finalModel
    )["elapsed"] 
    
      
    # fit linear regression for funsies
    All_Results[[Cur_Fun]]$LinReg$TIME[i] <- system.time(
    LR.model <- lm(y~., data = AllData[1:ntrain,])
    )["elapsed"]
      
    BART.preds <- predict(bart.model, X.test)
    XG.preds <- predict(XG.model, dtest)
    RF.preds <- predict(RF.model, X.test)
    LR.preds <- predict(LR.model, X.test)
      
    All_Results[[Cur_Fun]]$BART$RMSE[i] <- sqrt(mean((BART.preds - y_test)^2))
    All_Results[[Cur_Fun]]$XGBoost$RMSE[i] <- sqrt(mean((XG.preds - y_test)^2))
    All_Results[[Cur_Fun]]$RandomF$RMSE[i] <- sqrt(mean((RF.preds - y_test)^2))
    All_Results[[Cur_Fun]]$LinReg$RMSE[i] <- sqrt(mean((LR.preds - y_test)^2))
    
    sprintf("Finished iteration %s out of %s for %s",i,nsim,Cur_Fun) %>% print()
  }
}

TabNames <- lapply(All_Results,function(l) names(l))[[1]]
Results <- lapply(All_Results,function(l) lapply(l, function(tab) colMeans(tab))) %>%
  lapply(function(x) x %>% unlist %>% matrix(.,ncol=2,byrow=TRUE))
for(i in 1:length(Results))
{
  rownames(Results[[i]]) <- TabNames
  colnames(Results[[i]]) <- c("mean_RMSE","mean_TIME")
}










# # xgb.fit <- xgboost(data = data.matrix(Xtrain), label = XSalePrice,
# #                    booster = "gbtree", objective = "reg:linear",
# #                    colsample_bytree = 0.2, gamma = 0.0,
# #                    learning_rate = 0.05, max_depth = 6,
# #                    min_child_weight = 1.5, n_estimators = 7300,
# #                    reg_alpha = 0.9, reg_lambda = 0.5,
# #                    subsample = 0.2, seed = 42,
# #                    silent = 1, nrounds = 25)
# 
# pred <- predict(model, dtest)
# 
# # get & print the classification error
# err <- mean(as.numeric(pred > 0.5) != test_labels)
# print(paste("test-error=", err))
# xgb.pred <- predict(xgb.fit, data.matrix(Xtrain))
# postResample(xgb.pred, XSalePrice)