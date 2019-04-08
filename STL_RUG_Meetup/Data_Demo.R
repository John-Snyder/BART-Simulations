library(tidyverse)

options(java.parameters = "-Xmx10g")
library(bartMachine)
numcores <- parallel::detectCores()
set_bart_machine_num_cores(numcores - 1)

library(xgboost)
library(randomForest)
library(caret)

data(benchmark_datasets)
