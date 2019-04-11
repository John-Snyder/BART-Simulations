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
  
k_fold_cv(X, y, k_folds = 10,
          num_trees = 200,
          num_burn_in = 1000,
          num_iterations_after_burn_in = 5000)

rmse_by_num_trees(bart.model, num_replicates = 20)  

bart.model.cv <- bartMachineCV(X, y,
                               num_burn_in = 1000,
                               num_iterations_after_burn_in = 5000)

investigate_var_importance(bart.model.cv)
interaction_investigator(bart.model.cv)

VarSel <- var_selection_by_permute(bart.model.cv)
VarSel$important_vars_local_names

RegressMod <- lm(formula = medv ~ ., data = Boston)
summary(RegressMod)

pd_plot(bart.model.cv, j = "lstat")
cov_importance_test(bart.model.cv, covariates = "lstat")

pd_plot(bart.model.cv, j = "indus")
cov_importance_test(bart.model.cv, covariates = "indus")

cov_importance_test(bart.model.cv)

## check your asusmptions
check_bart_error_assumptions(bart.model.cv)  
plot_convergence_diagnostics(bart.model.cv)  

plot_y_vs_yhat(bart.model.cv, credible_intervals = TRUE)



# predictions
MeanDF <- colMeans(X)
predict(bart.model.cv, newdata = MeanDF)
calc_credible_intervals(bart.model.cv, MeanDF, ci_conf = 0.95) %>% round(digits=2)
calc_prediction_intervals(bart.model.cv, MeanDF, pi_conf = 0.95) %>% round(digits=2)








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

  
  
  
  
  
  
  
  
  
    