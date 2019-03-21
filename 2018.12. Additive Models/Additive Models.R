# data
library(ggplot2)
data(economics)
data = data.frame(economics$psavert, economics$pce, economics$pop, economics$uempmed)
n=174
data = data[401:574,]
data = data.frame(1:n, data)
colnames(data)=c("order","y","x1", "x2", "x3")
head(data)

########################################## 1
# Kernel regression
data1 = data[order(data$x1),]
par(mfrow=c(1,3))
# 1
X = data1$x1
Y = data1$y
# Bandwidth selection
h_seq = seq(100, 300, 10)
CV_err = rep(NA,length(h_seq))
for(j in 1:length(h_seq)){
  h = h_seq[j]
  error = rep(NA, n)
  for(i in 1:n){
    xi = X[i]
    yi = Y[i]
    # validation set
    x = X[-i]
    y = Y[-i]
    # training set
    y_hat = ksmooth(x, y, kernel="normal", bandwidth=h, x.points = xi)
    error[i] = (yi - y_hat$y)^2
  }
  CV_err[j] = mean(error)
}
CV_err
plot(h_seq, CV_err, cex = .2, type = 'b', xlab = 'Bandwidth', ylab = 'LOOCV Prediction Error')
min = min(CV_err,na.rm=T)
b1 = h_seq[which(CV_err==min)]
k1 = ksmooth(X, Y, kernel="normal", bandwidth=b1,x.points = X)
y1 = k1$y
e1 = Y - y1
data1 <- data.frame(data1, y1, e1)

# 2
data1 = data1[order(data1$x2),]
X = data1$x2
Y = data1$e1
# Bandwidth selection
h_seq = seq(500, 700, 10)
CV_err = rep(NA,length(h_seq))
for(j in 1:length(h_seq)){
  h = h_seq[j]
  error = rep(NA, n)
  for(i in 1:n){
    xi = X[i]
    yi = Y[i]
    # validation set
    x = X[-i]
    y = Y[-i]
    # training set
    y_hat = ksmooth(x, y, kernel="normal", bandwidth=h, x.points = xi)
    error[i] = (yi - y_hat$y)^2
  }
  CV_err[j] = mean(error)
}
CV_err
plot(h_seq, CV_err, cex = .2, type = 'b', xlab = 'Bandwidth', ylab = 'LOOCV Prediction Error')
min = min(CV_err,na.rm=T)
b2 = h_seq[which(CV_err==min)]
k2 = ksmooth(X, Y, kernel="normal", bandwidth=b2, x.points = X)
y2 = k2$y
e2 = Y - y2
data1 <- data.frame(data1, y2, e2)

# 3
data1 = data1[order(data1$x3),]
X = data1$x3
Y = data1$e2
# Bandwidth selection
h_seq = seq(0, 25, 1)
CV_err = rep(NA,length(h_seq))
for(j in 1:length(h_seq)){
  h = h_seq[j]
  error = rep(NA, n)
  for(i in 1:n){
    xi = X[i]
    yi = Y[i]
    # validation set
    x = X[-i]
    y = Y[-i]
    # training set
    y_hat = ksmooth(x, y, kernel="normal", bandwidth=h, x.points = xi)
    error[i] = (yi - y_hat$y)^2
  }
  CV_err[j] = mean(error)
}
CV_err
plot(h_seq, CV_err, cex = .2, type = 'b', xlab = 'Bandwidth', ylab = 'LOOCV Prediction Error')
min = min(CV_err,na.rm=T)
b3 = h_seq[which(CV_err==min)]
k3 = ksmooth(X, Y, kernel="normal", bandwidth=b3, x.points = X)
y3 = k3$y
e3 = Y - y3
data1 <- data.frame(data1, y3, e3)

# plot 
par(mfrow=c(2,3))
plot(data1$x1, data1$y, cex = .4, xlab = "x1", ylab="y")
lines(k1,col="blue")
plot(data1$x2, data1$e1, cex = .4, xlab = "x2", ylab="y2")
lines(k2,col="blue")
plot(data1$x3, data1$e2, cex = .4, xlab = "x3",ylab="y3")
lines(k3,col="blue")
plot(data1$y1, data1$e1, cex = .4, main="Residual vs Prediction",xlab="Predictions",ylab="Residuals")
plot(data1$y2, data1$e2, cex = .4, main="Residual vs Prediction",xlab="Predictions",ylab="Residuals")
plot(data1$y3, data1$e3, cex = .4, main="Residual vs Prediction",xlab="Predictions",ylab="Residuals")


# (Cubic) Smoothing Spline
data2 = data[1:5]
# 1
data2 = data2[order(data2$x1),]
X = data2$x1
Y = data2$y
s1 = smooth.spline(X, Y, cv=T)
l1 = s1$spar
y1 = predict(s1,X)$y
e1 = Y - y1
data2 <- data.frame(data2, y1, e1)

# 2
data2 = data2[order(data2$x2),]
X = data2$x2
Y = data2$e1
s2 = smooth.spline(X, Y, cv=T)
l2 = s2$spar
y2 = predict(s2,X)$y
e2 = Y - y2
data2 <- data.frame(data2, y2, e2)

# 3
data2 = data2[order(data2$x3),]
X = data2$x3
Y = data2$e2
s3 = smooth.spline(X, Y, cv=T)
l3 = s3$spar
y3 = predict(s3,X)$y
e3 = Y - y3
data2 <- data.frame(data2, y3, e3)

# plot
par(mfrow=c(2,3))
plot(data2$x1, data2$y, cex = .4, xlab = "x1",ylab="y")
lines(s1,col="red")
plot(data2$x2, data2$e1, cex = .4, xlab = "x2",ylab="y2")
lines(s2,col="red")
plot(data2$x3, data2$e2, cex = .4, xlab = "x3",ylab="y3")
lines(s3,col="red")
plot(data2$y1, data2$e1, cex = .4, main="Residual vs Prediction",xlab="Predictions",ylab="Residuals")
plot(data2$y2, data2$e2, cex = .4, main="Residual vs Prediction",xlab="Predictions",ylab="Residuals")
plot(data2$y3, data2$e3, cex = .4, main="Residual vs Prediction",xlab="Predictions",ylab="Residuals")

# compare
par(mfrow=c(2,1))
plot(data1$x1, data1$y, cex = .4, xlab = "x1", ylab="y")
lines(k1,col="blue")
lines(s1,col="red")
plot(data1$x2, data1$e1, cex = .4, xlab = "x2", ylab="y2")
lines(k2,col="blue")
lines(s2,col="red")


########################################## 2
# x1 = 6934.5, 12161.5
# 19096/2    =    9548
# x2 = 283453, 320887
# 604340/2   =    302170
x1_c = 9962
x2_c = 307680
x1_s = 7055
x2_s = 285840

# kernel
y1_c_k = ksmooth(data1$x1, data1$y, kernel="normal", bandwidth=b1, x.points = x1_c)
y2_c_k = ksmooth(data1$x2, data1$e1, kernel="normal", bandwidth=b2, x.points = x2_c)
center_k = y1_c_k$y + y2_c_k$y
y1_s_k = ksmooth(data1$x1, data1$y, kernel="normal", bandwidth=b1, x.points = x1_s)
y2_s_k = ksmooth(data1$x2, data1$e1, kernel="normal", bandwidth=b2, x.points = x2_s)
side_k = y1_s_k$y + y2_s_k$y

# spline
y1_c_s = predict(s1,x1_c)
y2_c_s = predict(s2,x2_c)
center_s = y1_c_s$y + y2_c_s$y
y1_s_s = predict(s1,x1_s)
y2_s_s = predict(s2,x2_s)
side_s = y1_s_s$y + y2_s_s$y


########################################## 3
# bootstrap
B=10000
point1_k = rep(0,B)
point1_s = rep(0,B)
point2_k = rep(0,B)
point2_s = rep(0,B)

for (i in 1:B){
  #sampling data from the original data set with replacement 
  sample = data[sample(nrow(data), 50), ]
  
  #Kernel Regression Method
  kernel = sample[order(sample$x1),]
  X = kernel$x1
  Y = kernel$y
  k1 = ksmooth(X, Y, kernel="normal", bandwidth=b1, x.points = X)
  y1 = k1$y
  point1_k_hat1 = ksmooth(X, Y, kernel="normal", bandwidth=b1, x.points = x1_c)$y
  point2_k_hat1 = ksmooth(X, Y, kernel="normal", bandwidth=b1, x.points = x1_s)$y
  e1 = Y - y1
  kernel <- data.frame(kernel, y1, e1)
  kernel = kernel[order(kernel$x2),]
  X = kernel$x2
  Y = kernel$e1
  k2 = ksmooth(X, Y, kernel="normal", bandwidth=b2, x.points = X)
  y2 = k2$y
  point1_k_hat2 = ksmooth(X, Y, kernel="normal", bandwidth=b2, x.points = x2_c)$y
  point2_k_hat2 = ksmooth(X, Y, kernel="normal", bandwidth=b2, x.points = x2_s)$y
  e2 = Y - y2
  kernel <- data.frame(kernel, y2, e2)
  point1_k[i] = point1_k_hat1 + point1_k_hat2
  point2_k[i] = point2_k_hat1 + point2_k_hat2
  
  #Spline Regression Method
  spline = sample[order(sample$x1),]
  X = spline$x1
  Y = spline$y
  s1 = smooth.spline(X, Y, spar=l1)
  y1 = predict(s1,X)$y
  predict(s1,x1_c)$y
  point1_s_hat1 = predict(s1,x1_c)$y
  point2_s_hat1 = predict(s1,x1_s)$y
  e1 = Y - y1
  spline <- data.frame(spline, y1, e1)
  spline = spline[order(spline$x2),]
  X = spline$x2
  Y = spline$e1
  s2 = smooth.spline(X, Y, spar=l2)
  y2 = predict(s2,X)$y
  point1_s_hat2 = predict(s2,x2_c)$y
  point2_s_hat2 = predict(s2,x2_s)$y
  e2 = Y - y2
  spline <- data.frame(spline, y2, e2)
  point1_s[i] = point1_s_hat1 + point1_s_hat2
  point2_s[i] = point2_s_hat1 + point2_s_hat2
}

# point1_k
hist(na.omit(point1_k))
center_k_mean = mean(na.omit(point1_k))
center_k_sd = sd(na.omit(point1_k))
# point1_s
hist(na.omit(point1_s))
center_s_mean = mean(na.omit(point1_s))
center_s_sd = sd(na.omit(point1_s))
# point2_k
hist(na.omit(point2_k))
side_k_mean = mean(na.omit(point2_k))
side_k_sd = sd(na.omit(point2_k))
# point2_s
hist(na.omit(point2_s))
side_s_mean = mean(na.omit(point2_s))
side_s_sd = sd(na.omit(point2_s))

center_k1 =c(round(center_k_mean-1.645*center_k_sd,4), round(center_k_mean+1.645*center_k_sd,4))
center_s1 =c(round(center_s_mean-1.645*center_s_sd,4), round(center_s_mean+1.645*center_k_sd,4))
side_k1 =c(round(side_k_mean-1.645*side_k_sd,4), round(side_k_mean+1.645*side_k_sd,4))
side_s1 =c(round(side_s_mean-1.645*side_s_sd,4), round(side_s_mean+1.645*side_s_sd,4))

# bias correction
# point1_k
# y_c_k
q = na.omit(point1_k)
p = sum(center_k>q)/10000
z = qnorm(p)
b = mean(q) - q
a = sum(b^3) / (6*(sum(b^2))^(3/2))
l = z + (z+qnorm(0.05))/(1-a*(z+pnorm(0.05)))
q1 = pnorm(l)
u = z + (z+qnorm(0.95))/(1-a*(z+qnorm(0.95)))
q2 = pnorm(u)
center_k2 = quantile(q, c(q1,q2))

# bias correction
# point1_s
# y_c_s
q = na.omit(point1_s)
p = sum(center_s>q)/10000
z = qnorm(p)
b = mean(q) - q
a = sum(b^3) / (6*(sum(b^2))^(3/2))
l = z + (z+qnorm(0.05))/(1-a*(z+pnorm(0.05)))
q1 = pnorm(l)
u = z + (z+qnorm(0.95))/(1-a*(z+qnorm(0.95)))
q2 = pnorm(u)
center_s2 = quantile(q, c(q1,q2))

# bias correction
# point2_k
# y_s_k
q = na.omit(point2_k)
p = sum(side_k>q)/10000
z = qnorm(p)
b = mean(q) - q
a = sum(b^3) / (6*(sum(b^2))^(3/2))
l = z + (z+qnorm(0.05))/(1-a*(z+pnorm(0.05)))
q1 = pnorm(l)
u = z + (z+qnorm(0.95))/(1-a*(z+qnorm(0.95)))
q2 = pnorm(u)
side_k2 = quantile(q, c(q1,q2))

# bias correction
# point2_s
# y_s_s
q = na.omit(point2_s)
p = sum(side_s>q)/10000
z = qnorm(p)
b = mean(q) - q
a = sum(b^3) / (6*(sum(b^2))^(3/2))
l = z + (z+qnorm(0.05))/(1-a*(z+pnorm(0.05)))
q1 = pnorm(l)
u = z + (z+qnorm(0.95))/(1-a*(z+qnorm(0.95)))
q2 = pnorm(u)
side_s2 = quantile(q, c(q1,q2))
