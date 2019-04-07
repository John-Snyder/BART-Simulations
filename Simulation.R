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
p.null <- 0
sigma <- 1
AllDataTypes <- c("Friedman","Mirsha","Exp","Linear")

Sim.Result.Empty <- data.frame(RMSE = rep(NA,nsim),
                         TIME = rep(NA,nsim))
All.Res.Empty <- list(BART = Sim.Result.Empty,
                      XGBoost = Sim.Result.Empty,
                      RandomF = Sim.Result.Empty,
                      LinReg = Sim.Result.Empty)

All.Results <- list(All.Res.Empty,All.Res.Empty,All.Res.Empty,All.Res.Empty)
names(All.Results) <- AllDataTypes

Mean.Results <- SD.Results <- Time.Results <- data.frame(BART = rep(NA,4),
                                                         XGboost = rep(NA,4),
                                                         RandomF = rep(NA,4),
                                                         LinReg = rep(NA,4))
rownames(Mean.Results) <- rownames(SD.Results) <- c("Friedman","Mirsha","Exp","Linear")


n <- ntrain + ntest
Start <- proc.time()["elapsed"]
for(Cur.Fun in AllDataTypes)
{
  for(i in 1:nsim)
  {
    #Generate Data
    AllData <- Generate.Data(nonlin.f = Cur.Fun, n, p.null, sigma)
      
    y.train <- AllData$y[1:ntrain]
    y.test <- AllData$y[(ntrain+1):n]
    
    X.train <- AllData[1:ntrain,-1] 
    X.test <- AllData[(ntrain+1):n,-1] 
      
    #Fit BART model, automatically cross-validating
    All.Results[[Cur.Fun]]$BART$TIME[i] <- system.time(
    bart.model <- bartMachine(X.train,y.train,
                              num_trees = 200,
                              num_burn_in = 1000,
                              num_iterations_after_burn_in = 10000,
                              verbose = FALSE)
    )["elapsed"]
      
    #Fit RandomForest
    All.Results[[Cur.Fun]]$RandomF$TIME[i] <- system.time(
    RF.model <- train(y~., data = AllData[1:ntrain,], method = "ranger",
                        trControl=trctrl)$finalModel
    )["elapsed"]

    ########
    dtrain <- xgb.DMatrix(data = as.matrix(X.train), label= y.train)
    dtest <- xgb.DMatrix(data = as.matrix(X.test), label= y.test)
    
    All.Results[[Cur.Fun]]$XGBoost$TIME[i] <- system.time(
    XG.model <- train(y~., data = AllData[1:ntrain,], method = "xgbTree",
                    trControl=trctrl,
                    tuneGrid = tune.grid.XGboost,
                    tuneLength = 20)$finalModel
    )["elapsed"] 
    
      
    # fit linear regression for funsies
    All.Results[[Cur.Fun]]$LinReg$TIME[i] <- system.time(
    LR.model <- lm(y~., data = AllData[1:ntrain,])
    )["elapsed"]
      
    BART.preds <- predict(bart.model, X.test)
    XG.preds <- predict(XG.model, dtest)
    RF.preds <- predict(RF.model, X.test)$predictions
    LR.preds <- predict(LR.model, X.test)
      
    All.Results[[Cur.Fun]]$BART$RMSE[i] <- sqrt(mean((BART.preds - y.test)^2))
    All.Results[[Cur.Fun]]$XGBoost$RMSE[i] <- sqrt(mean((XG.preds - y.test)^2))
    All.Results[[Cur.Fun]]$RandomF$RMSE[i] <- sqrt(mean((RF.preds - y.test)^2))
    All.Results[[Cur.Fun]]$LinReg$RMSE[i] <- sqrt(mean((LR.preds - y.test)^2))
    
    sprintf("Finished iteration %s out of %s for %s",i,nsim,Cur.Fun) %>% print()
  }
}

print(sprintf("%s simulations finished in %s seconds.  Averaged %s seconds per sim",
        nsim,
        proc.time()["elapsed"] - Start,
        (proc.time()["elapsed"] - Start)/nsim))

TabNames <- lapply(All.Results,function(l) names(l))[[1]]
Results <- lapply(All.Results,function(l) lapply(l, function(tab) colMeans(tab))) %>%
  lapply(function(x) x %>% unlist %>% matrix(.,ncol=2,byrow=TRUE))
for(i in 1:length(Results))
{
  rownames(Results[[i]]) <- TabNames
  colnames(Results[[i]]) <- c("mean.RMSE","mean.TIME")
}
