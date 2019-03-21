library(knitr)

# load data
logit_x = data.matrix(read.table("data/logit-x.dat"))
logit_y = data.matrix(read.table("data/logit-y.dat"))
threshold = 10^(-10)
max_iter = 10000
step_size=0.1
theta = matrix(c(0,0,0), nrow=3)
x = cbind(logit_x, rep(1,99))
diff = 1
iter = 1
# store the values of the log likelihood function each iteration
log = rep(NA,2)
# continue while the difference between the log likelihood value greater than threshold
while(diff > threshold){
  h = as.vector( 1/(1 + exp(- x %*% theta)) )
  # gradient
  f = t(x) %*% (logit_y-h)
  # hessian
  H = - t(x) %*% diag(h*(1-h)) %*% x
  theta = theta - step_size*solve(H) %*% f
  # store each iteration log value
  log[iter] = - sum( logit_y*log(h) + (1-logit_y)*log(1-h) )
  # calculate the difference between two iterations
  if(iter > 1){
    diff = log[iter-1]-log[iter]
  }
  # end iteration if run too many times
  if(iter > max_iter) {
    break
  }
  iter = iter + 1
}
coef = data.frame(theta1 = theta[1], theta2 = theta[2],"Intercept" = theta[3])
kable(round(coef,4), align = "c")

log[iter-1] # 32.5856
iter # 130

# plot
plot(1:(iter-1),log, cex =.5, ylim = c(30,70), xlab='iter', ylab='negative log likelihood')
lines(1:(iter-1), log)

