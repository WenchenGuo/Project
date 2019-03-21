library(survival)
library(survminer)
# access the data
data(lung)
head(lung)
# 228 observations of 10 variables
dim(lung)
lung[c(1:10),]
library(xtable)
xtable(lung[c(1:10),], label="data", caption="First 10 rows of the Lung Cancer Data")

# survival function
# create a survival object for lifetime data
surv = Surv(lung$time, lung$status)
#Fits a survival curve
fit1 = survfit(surv~1, data=lung, conf.int=0.9)
print(fit1)
summary(fit1)
summary(fit1)$table
summary(fit1, times=seq(0, 1000, 200))
plot(fit1, main="Survival Curve for Lung Data", xlab="Time in Days", ylab="Proportion Surviving")

# Survival Function with Group
# fit survival curves separately by sex
fit2 = survfit(surv~sex,data=lung,conf.int=0.9)
print(fit2)
summary(fit2)
summary(fit2, times=seq(0, 1000, 200))
plot(fit2, main="Survival Curve for Lung Data with Group", xlab="Time in Days", ylab="Proportion Surviving", col=c(1:2))
legend('topright', c("Male","Female"), lty=1, col=c(1:2))



# bootstap for survival function
B = 1000 # #bootstrap-samples = 1000
# get Bootstrap samples for estimates 
SKM = rep(NA, times=B) 
sd = rep(NA, times=B) 
UI = rep(NA, times=B) 
LI = rep(NA, times=B) 
t = 300
# 50 pairs for each loop
for (i in 1:B){
  sample.i = sample(surv, 50, replace=TRUE) 
  fit = survfit(sample.i~1,conf.int=0.9)
  summary(fit, times=t)
  SKM[i] = summary(fit, times=t)$surv
  sd[i] = summary(fit, times=t)$std.err
  UI[i] = summary(fit, times=t)$upper
  LI[i] = summary(fit, times=t)$lower
}
Mean_SKM = mean(SKM)
UI = mean(UI)
LI = mean(LI)
summary(fit1, times=300)



################### five different methods of constructing CIs
# 1
surv = Surv(cancer$time, cancer$status)
#Fits a survival curve
fit = survfit(surv~1, data=cancer, conf.int=0.9)

summary(fit)$table # median = 310
quantile(fit)
summary(fit, times=310) # 0.495
# 0.44   0.557
LCL1 = 0.44  
UCL1 = 0.557


# 2
# bootstrap
library("Hmisc")
q = bootkm(surv, time=310, B=10000)
# save the bootstrap data to txt file
write.table(q, "q.txt", sep="\t")
# access the data
q = data.matrix(read.csv("q.txt", sep=""))

describe(q)
mean_q = mean(q)
se_q = sd(q)
UCL2 = mean_q + 1.645*se_q
LCL2 = mean_q - 1.645*se_q


# 3
quantile(q, c(.05,.95))
LCL3 = 0.4349957 
UCL3 = 0.5521079


# 4
b = mean(q) - q
p = sum(0.495>=q)/10000
z = qnorm(p)
a = sum(b^3) / (6*(sum(b^2))^(3/2))

l = z + (z+qnorm(0.05))/(1-a*(z+pnorm(0.05)))
q1 = pnorm(l) # 0.05317461
u = z + (z+qnorm(0.95))/(1-a*(z+qnorm(0.95)))
q2 = pnorm(u) # 0.9530417

quantile(q, c(q1,q2))
LCL4 = 0.4365107 
UCL4 = 0.5528617 


# 5
library(coda)
HPDinterval(as.mcmc(q),prob=0.9)
LCL5 = HPDinterval(as.mcmc(q),prob=0.9)[1] # 0.4385311
UCL5 = HPDinterval(as.mcmc(q),prob=0.9)[2] # 0.55483097
lengthHPD=UCL5-LCL5
lengthHPD 


# Anderson-Darling Test
library(nortest)
ad.test(q)



############################ plot
curve(dnorm(x, mean(q), sd(q)), main="Normal distribution", add=TRUE, col=2, xlim=c(0.3,0.7))
abline(v=mean(q),col=2)
abline(v=LCL2,lty = 2,col=2)
abline(v=UCL2,lty = 2,col=2)

# 2
plot(density(q),main="Distribution of q",xlim=c(0.3,0.7))
abline(v=LCL1,lty = 2, col=1)
abline(v=UCL1,lty = 2, col=1)
# 3
abline(v=LCL3,lty = 2,col=3)
abline(v=UCL3,lty = 2,col=3)
# 4
abline(v=LCL4,lty = 2,col=4)
abline(v=UCL4,lty = 2,col=4)
# 5
abline(v=LCL5,lty = 2,col=5)
abline(v=UCL5,lty = 2,col=5)
legend('topright', c("1","3","4","5"), lty=2, col=c(1,3,4,5))



###############################################################

t = bootkm(surv, q=0.5, B=10000)
# save the bootstrap data to txt file
write.table(t, "~/Desktop/t.txt", sep="\t")
# access the data
t = data.matrix(read.csv("~/Desktop/t.txt", sep=""))

describe(t)

summary(fit1)$table # median = 310.00000
# CI 285.00000  353.00000 

# histogrm
h = hist(t)
h$density = h$counts/sum(h$counts)
plot(h,freq=FALSE, ylim=c(0,0.5))
plot(density(t),main="Distribution of median time")

mean_t = mean(t)
# 318.0018
sd_t = sd(t)
# 23.5513

bias = mean(t) - 310
