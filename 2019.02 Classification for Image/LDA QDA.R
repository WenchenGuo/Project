library(MASS)
library(xtable)

# ------------------------------------------------------------------------------
X = read.table("image/data.dat")
y = read.table("image/label.dat")
data = data.frame(t(rbind(y,X)))
colnames(data)<-c("label",1:784)
label = data.frame(t(rbind(1:1990,y)))
colnames(label)<-c("index","label")
image2 = label[label$label == 2,]
image6 = label[label$label == 6,]

# split the data into two part
## set the seed to make the partition reproducible
set.seed(1)
a = image2[sample(nrow(image2),826,replace=FALSE),]$index
b = image6[sample(nrow(image6),766,replace=FALSE),]$index
index= c(a,b)
# 80% training data  n = 1592
training = data[index, ]
training_x = training[,2:785]
training_y = training[,1]
# 20% training data  n = 398
testing = data[-index, ]
testing_x = testing[,2:785]
testing_y = testing[,1]

#------------------------------------------------------------------------------
Sigma = t(training_x)%*%as.matrix(training_x)
# use PCA to the covariance matrix
pca = prcomp(Sigma, center=TRUE, scale=FALSE)
pc.use = 50
# plot
par(mfrow=c(2,3))
# plot of the first 50th PCs
plot(pca$sdev[1:pc.use],xlab="Principal Component",ylab="eigenvalues",
     type="b",main ="largest 50 principal components")
for (i in 1:5){
  pc_i = pca$x[,i]
  im_i = matrix(pc_i,ncol=28)
  im_i = rotateImage(im_i, 270)
  image(im_i, main=i) 
}

#------------------------------------------------------------------------------
# new training data set dim = 1592*50
training_data = data.frame(cbind(training_y,as.matrix(training_x) %*% pca$x[,1:pc.use]))
# new testing data set  dim = 398*50
testing_data = data.frame(cbind(testing_y,as.matrix(testing_x) %*% pca$x[,1:pc.use]))
training2 = training_data[training_data$training_y == 2, ]
training6 = training_data[training_data$training_y == 6, ]
testing2 = testing_data[testing_data$testing_y == 2, ]
testing6 = testing_data[testing_data$testing_y == 6, ]

#------------------------------------------------------------------------------
# LDA
pi_1 = length(which(training_y==2))/1592
pi_2 = length(which(training_y==6))/1592
mu_1 = colSums(training2[, 2:51])/826
mu_2 = colSums(training6[, 2:51])/766
sigma = as.matrix(cov(training_data[,2:51]))

# testing
label1 = rep(0,398)
label2 = rep(0,398)
for (i in 1:398){
  label1[i] = t(mu_1)%*%solve(sigma)%*%t(testing_data[i,2:51])-(t(mu_1)%*%solve(sigma)%*%mu_1)/2+log(pi_1)
  label2[i] = t(mu_2)%*%solve(sigma)%*%t(testing_data[i,2:51])-(t(mu_2)%*%solve(sigma)%*%mu_2)/2+log(pi_2)
}

est.label_l = rep(0,398)
for(i in 1:398){
  if (label1[i]>label2[i]){
    est.label_l[i]=2
  }
  else{
    est.label_l[i]=6
  }
}
mean(est.label_l == testing_y)
table(Predicted=est.label_l, true_label=testing_y)

#------------------------------------------------------------------------------
# LDA - use package
# Fit the model
fit1 <- lda(training_y~., data = training_data)
testing_pred <- predict(fit1,testing_data)$class
#accuracy
mean(testing_pred == testing_y)
table(Predicted=testing_pred, true_label=testing_y)


#------------------------------------------------------------------------------
# QDA
sigma1 = as.matrix(cov(training2[, 2:51]))
sigma2 = as.matrix(cov(training6[, 2:51]))

# testing
label1 = rep(0,398)
label2 = rep(0,398)
for (i in 1:398){
  x = as.matrix(testing_data[i,2:51])
  label1[i] =-x%*%solve(sigma1)%*%t(x)/2 + t(mu_1)%*%solve(sigma1)%*%t(x)-
    (t(mu_1)%*%solve(sigma1)%*%mu_1)/2-log(det(sigma1))/2+log(pi_1)
  label2[i] =-x%*%solve(sigma2)%*%t(x)/2 + t(mu_2)%*%solve(sigma2)%*%t(x)-
    (t(mu_2)%*%solve(sigma2)%*%mu_2)/2-log(det(sigma2))/2+log(pi_2)
}

est.label_q = rep(0,398)
for(i in 1:398){
  if (label1[i]>label2[i]){
    est.label_q[i]=2
  }
  else{
    est.label_q[i]=6
  }
}
mean(est.label_q == testing_y)
table(Predicted=est.label_q, true_label=testing_y)

#------------------------------------------------------------------------------
# QDA - use package
fit2 <- qda(training_y~., data = training_data)
testing_pred <- predict(fit2,testing_data)$class
#accuracy
mean(testing_pred == testing_y)
table(Predicted=testing_pred, true_label=testing_y)

