# Gradient Descent for Unweighted Linear Regression
# load data
rx = data.matrix(read.table("data/rx.dat"))
ry = data.matrix(read.table("data/ry.dat"))
threshold = 10^(-10)
max_iter = 10000
step_size = 0.005
theta1 = matrix(c(0,0), nrow=2)
x = cbind(rx, rep(1,100))
m = length(ry)
diff = 1
iter1 = 1
# store the values of the log likelihood function each iteration
log1 = rep(NA,2)
# continue while the difference between the log likelihood value greater than threshold
while(diff > threshold){
  # gradient
  grad = t(x) %*% (x %*% theta1 -ry)/m
  theta1 = theta1 - step_size*grad
  # store each iteration log value
  log1[iter1] = t((ry - x %*% theta1)) %*% (ry- x %*% theta1)
  # calculate the difference between two iterations
  if(iter1 > 1){
    diff = log1[iter1-1]-log1[iter1]
  }
  # end iteration if run too many times
  if(iter1 > max_iter) {
    break
  }
  iter1 = iter1 + 1
}
coef = data.frame("Intercept" = theta1[2], theta = theta1[1])
kable(round(coef,4), align = "c")
log1[iter1-1]
iter1

# plot
plot(1:(iter1-1),log1, cex =.5, ylim = c(60,140), xlab='iter1', ylab='negative log likelihood')
lines(1:(iter1-1), log1)




# Gradient Descent for Locally Weighted Linear Regression
# load data
rx = data.matrix(read.table("data/rx.dat"))
ry = data.matrix(read.table("data/ry.dat"))
threshold = 10^(-10)
max_iter = 10000
step_size=0.005
theta2 = matrix(c(0,0), nrow=2)
x = cbind(rx, rep(1,100))
m = length(ry)
w = matrix(rep(0, m^2), nrow=m)
for (i in 1:m) {
  w[i,i] = exp(-rx[i]^2/20)
}
diff = 1
iter2 = 1
# store the values of the log likelihood function each iteration
log2 = rep(NA,2)
# continue while the difference between the log likelihood value greater than threshold
while(diff > threshold){
  # gradient
  grad = t(x) %*% w %*%  (x %*% theta2 -ry)
  theta2 = theta2 - step_size*grad
  # store each iteration log value
  log2[iter2] = t((ry - x %*% theta2)) %*%  w %*% (ry- x %*% theta2)
  # calculate the difference between two iterations
  if(iter2 > 1){
    diff = log2[iter2-1]-log2[iter2]
  }
  # end iteration if run too many times
  if(iter2 > max_iter) {
    break
  }
  iter2 = iter2 + 1
}
coef = data.frame("Intercept" = theta2[2], theta = theta2[1])
kable(round(coef,4), align = "c")
log2[iter2-1]
iter2

# plot
plot(1:(iter2-1),log2, cex =.5,ylim = c(10,50), xlab='iter2', ylab='negative log likelihood')
lines(1:(iter2-1), log2)


# comparison
y1 = x %*% theta1
y2 = x %*% theta2
plot(rx,ry, xlab='x', ylab='y')
lines(rx, y1,col="red")
lines(rx, y2,col="blue")

legend(5.5, -0.5, legend=c("(c)unweighted", "(d)weighted"),
       col=c("red", "blue"), lty=c(1,1), cex=1.2)
