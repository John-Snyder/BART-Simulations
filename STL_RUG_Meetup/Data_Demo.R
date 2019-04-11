library(MASS)
library(tidyverse)

options(java.parameters = "-Xmx10g")
library(bartMachine)
numcores <- parallel::detectCores()
set_bart_machine_num_cores(numcores - 1)

data(Boston)
Boston %>% head %>% View

RegressMod <- lm(formula = medv ~ ., data = Boston)
summary(RegressMod)

set.seed(1)
train <- sample(1:nrow(Boston), .8*nrow(Boston))

y <- Boston$medv
X <- Boston %>% dplyr::select(-c("medv"))

#Fit BART model
bart.model <- bartMachine(X,y,
                          num_trees = 200,
                          num_burn_in = 1000,
                          num_iterations_after_burn_in = 5000)
bart.model
  
k_fold_cv(X.train, y.train, k_folds = 5)

rmse_by_num_trees(bart.model, num_replicates = 5)  

bart.model.cv <- bartMachineCV(X.train, y.train,
                               num_burn_in = 100,
                               num_iterations_after_burn_in = 100)
  
check_bart_error_assumptions(bart.model.cv)  
plot_convergence_diagnostics(bart.model.cv)  

plot_y_vs_yhat(bart.model.cv, credible_intervals = TRUE)

investigate_var_importance(bart.model.cv, num_replicates_for_avg = 20) 
interaction_investigator(bart.model.cv, num_replicates_for_avg = 25,num_var_plot = 10, bottom_margin = 5)

pd_plot(bart.model.cv, j = "lstat")
pd_plot(bart.model.cv, j = "rm")

cov_importance_test(bart.model.cv)
cov_importance_test(bart.model.cv, covariates = "lstat")

###
set.seed(1)
nsim <- 1000
x1 <- runif(nsim,-3.14,3.14)
x2 <- runif(nsim,-3.14,3.14)

y <- x1+sin(x2) + rnorm(nsim)
plot(x2,y)

bart.model <- bartMachine(data.frame(x1,x2),y,
                          num_trees = 200,
                          num_burn_in = 1000,
                          num_iterations_after_burn_in = 4000)

pd_plot(bart.model, j = "x2")
###


library(xgboost)
library(caret)

  
  
  
  
  
  
  
  
  
    