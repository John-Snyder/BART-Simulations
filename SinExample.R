options(java.parameters = "-Xmx12g")
library(bartMachine)
numcores <- parallel::detectCores()
set_bart_machine_num_cores(numcores - 1)


set.seed(1)
nsim <- 1000
x1 <- runif(nsim,-3.14,3.14)
x2 <- runif(nsim,-3.14,3.14)

y <- x1*sin(x2) + rnorm(nsim)
plot(x2,y)

bart.model <- bartMachine(data.frame(x1,x2),y,
                          num_trees = 200,
                          num_burn_in = 1000,
                          num_iterations_after_burn_in = 4000)
pd_plot(bart.model, j = "x2")
lines(x=seq(-3.14,3.14,length.out = 100),
      y=sin(seq(-3.14,3.14,length.out = 100)),col="red")
