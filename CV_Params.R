trctrl <- trainControl(method = "cv", number = 5)

tune_grid_XGboost <- expand.grid(nrounds=c(100,200,300,400),
                                 max_depth = c(3:7),
                                 eta = c(0.05, 1),
                                 gamma = c(0.01),
                                 colsample_bytree = c(0.75),
                                 subsample = c(0.50),
                                 min_child_weight = c(0))
