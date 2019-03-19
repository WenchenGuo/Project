library(OpenImageR)
library(mvtnorm)
library(mixtools)
library(useful)

# ------------------------------------------------------------------------------
image = read.table("image/data.dat")
# visualize images
par(mfrow=c(1,2))
image2 = matrix(image[,1],ncol=28)
image2 = rotateImage(image2, 270)
image(image2)
image6 = matrix(image[,1900],ncol=28)
image6 = rotateImage(image6, 270)
image(image6)

# ------------------------------------------------------------------------------
data = t(image)
# use PCA to duduce the dimensions
pca = prcomp(data, center=TRUE, scale=FALSE)
# compute variance
pca_var = pca$sdev^2
#proportion of variance explained
prop_var = pca_var/sum(pca_var)
# plot of the first 20th PCs
par(mfrow=c(1,2))
# Proportion plot
plot(prop_var[1:20],xlab="Principal Component",ylab="Proportion of Variance Explained",type="b")
# Cumulative plot
plot(cumsum(prop_var[1:20]),xlab="Principal Component",ylab="Cumulative Proportion",type="b")
# only use the first two PCs
pc.use = 2
# new data set  dim = 1990*2
data = as.matrix(pca$x[,1:pc.use])

# ------------------------------------------------------------------------------
# Estep  get p_i,c
Estep = function(data,pi,mu,sigma,C){
  for(i in 1:N){
    for(c in 1:C){
      p[i,c] = pi[c]*dmvnorm(data[i,], mu[,c], sigma[,,c])
    }
  }
  p / rowSums(p)
}


# Mstep update theta
Mstep = function(p,data,C,N,degree) {
  # matrix: 1 indicates rows, 2 indicates columns
  p.sum = apply(p,2,sum)
  new.mix = p.sum / N
  new.mu = t(t(t(data) %*% p) / p.sum)
  new.sigma = array(0, dim=c(degree, degree, C))
  for(c in 1:C) {
    sig = matrix(0,degree,degree)
    for(n in 1:N) {
      sig = sig + p[n,c]*(data[n,]-new.mu[,c]) %*% t(data[n,]-new.mu[,c])
    }
    new.sigma[,,c] = sig / p.sum[c]
  }  
  list(new.mix, new.mu, new.sigma)
}

# calculate log-likelihood
Q = function(p,data,C,N,degree) {
  probs = 0
  for(i in 1:N){
    for(c in 1:C){
      probs = probs + p[i,c]*(log(pi[c]) + log(dmvnorm(data[i,], mu[,c], sigma[,,c])))
    }
  }
  probs
}

# ------------------------------------------------------------------------------
C = 2 # number of the clusters
degree = 784  # degree of the data: p
N = 1990 # number of samples
degree = pc.use # reduce the degree
# initial values for theta
pi = rep(1/C,C)
# here we use the mu matrix which is close to the mean of the data
# the log-likelihood will converge faster
mu = matrix(apply(data,2,mean), nrow=degree, ncol=C)
mu = mu + 0.1*matrix(runif(C*degree), nrow=degree, ncol=C)
sigma = array(0, dim=c(degree, degree, C))
for(c in 1:C){
  sigma[,,c] = diag(1,nrow=degree, ncol=degree)
}
p = matrix(0,ncol=C,nrow=N)
n.trial = 100
Qlist = rep(0,n.trial)

# EM algorithm
for(i in 1:n.trial) {
  p = Estep(data,pi,mu,sigma,C)
  result = Mstep(p,data,C,N,degree)
  pi = result[[1]]
  mu = result[[2]]
  sigma = result[[3]]
  Qlist[i] = Q(p,data,C,N,degree)
}

# plot log-likelihood
plot(Qlist[1:n.trial],xlab ="Number of Iterations",ylab ="log-likelihood",type ="l")


# ------------------------------------------------------------------------------
# Use the pi,c to infer the labels of the images
est.label = rep(0,N)
for(i in 1:N){
  if (p[i,1]>0.5){
    est.label[i]=1
  }
  else{
    est.label[i]=2
  }
}

# inversely transform the mean vectors into original vcetor space p=784
mean = colMeans(t(image))
a = mu[,1] %*% t(pca$rotation[,1:pc.use])
a = scale(a, center = -mean, scale = FALSE)
b = mu[,2] %*% t(pca$rotation[,1:pc.use])
b = scale(b, center = -mean, scale = FALSE)
# visualize images
par(mfrow=c(1,2))
im2 = matrix(b,ncol=28)
image(rotateImage(im2, 270))
im6 = matrix(a,ncol=28)
image(rotateImage(im6, 270))


# ------------------------------------------------------------------------------
# true label
label = read.table("image/label.dat")
true.label = t(label)
true.label[which(true.label==2)] = 2
true.label[which(true.label==6)] = 1
length(which(true.label==1))
length(which(true.label==2))
# 1032 958
# estimate label
length(which(est.label==1))
length(which(est.label==2))
# 1084 906
# compare
summary(compare.list(true.label,est.label))

