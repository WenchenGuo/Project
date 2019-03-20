# Data Input
library(readxl)
data = read_excel("data.xls")
# there are only 199 data-value-rows.

# check whether normality assumption is valid for the y-data = sbp
sbp = data.matrix(data[,2])

par(mfrow=c(1,2))
hist(sbp, main="Histogram of sbp (= y-data)")
qqnorm(sbp, main="Normal QQ-plot",xlab="Sample Quantiles",ylab="Theoretic Quantiles")
# It seems that the data-distribution is skewed-to-the-right with a thin-tail in the right-side

# Use the BoxCox transformation to transform it to a near-normal distribution
library(MASS) 
# need to load the library("mass") for using the BoxCox transformation

# Run the first-path main-effect model
mod1 = lm(sbp ~ dbp + scl + followup + age + bmi + month + sex + chdfate, data) 

par(mfrow=c(1,1)) 
boxcox(mod1, lambda = seq(-3.5, 3.5, 1/10), plotit = TRUE, xlab = "Lambda Values", ylab = "Log-Likelihood")
title("BoxCox Plot for Normality Transformation")
# BoxCox Plot shows that sbp should be transformed into 1/sbp for a better normality-y-data modeing

sbpinv = data.matrix(1/sbp)

# Check its histogram and Normal QQ-plot to see whether the inverse-transformation works well
par(mfrow=c(1,2))
hist(sbpinv, main="Histogram of 1/sbp (= new-y-data)")
qqnorm(sbpinv, main="Normal QQ-plot for 1/sbp",xlab="Sample Quantiles",ylab="Theoretic Quantiles")

# The normality-validation-plots look well; we will proceed with validating x-y linear relationships next

sex = data.matrix(data[,1])
dbp = data.matrix(data[,3])
scl = data.matrix(data[,4])
chdfate = data.matrix(data[,5])
followup = data.matrix(data[,6])
age = data.matrix(data[,7])
bmi = data.matrix(data[,8])
month = data.matrix(data[,9])


# Note that validation of x-y's linear relationship only makes sense for continuous x-variables
par(mfrow=c(3,2))
plot(dbp,sbpinv,xlab="dbp",ylab="1/sbp")
plot(scl,sbpinv,xlab="scl",ylab="1/sbp")
plot(followup,sbpinv,xlab="followup",ylab="1/sbp")
plot(age,sbpinv,xlab="age",ylab="1/sbp")
plot(bmi,sbpinv,xlab="bmi",ylab="1/sbp")
plot(month,sbpinv,xlab="month",ylab="1/sbp")
mtext("X-Y Plots for Validating Linear X-Y Relationships", side = 3, line = -2, outer = TRUE)


# Define the following two-variable combinational-effects
# only dbp has a visualized clear x-y linear relationship
# only consider dbp's combinational effects with other continuous variables
dscl = dbp*scl
dfol = dbp*followup
dage = dbp*age
dbmi = dbp*bmi
dmo = dbp*month

# create a new data-object
data[,2] = sbpinv
names(data)[2] = "sbpinv"
dataext = cbind(data,dscl,dfol,dage,dbmi,dmo)

# Run a regression model with all main-effects and 2-fi-interactions
mod2 = lm(sbpinv ~ dbp + scl + followup + age + bmi + month + sex + chdfate + dscl + dfol + dage + dbmi + dmo, dataext)
mod2_summary = summary(mod2)
mod2_summary 


# examined potential collinearity issues with VIF evaluations
library(car)  
vifmod2 = vif(mod2)
vifmod2


# Run another regression model just for all main-effects without any 2-fi-interactions
modn1 = lm(sbpinv ~ dbp + scl + followup + age + bmi + month + sex + chdfate, dataext)
modn1_summary = summary(modn1)
modn1_summary 

vifmodn1 = vif(modn1)
vifmodn1


#----------------------------------------------------------------------------
# Forward-Selection

# One-variable Model
# let us start with the x-variable, which is most correlated to y = sbpinv
cor_yx = cor(dataext) # correlation matrix
cor_yax = cor_yx[,2]  
cor_yax
# check the absolute value of the correlation

# dbp is the most correlated x-variable
mone = lm(sbpinv ~ dbp, dataext)
mone_s = summary(mone)
mone_s


# Two-Variable-Model
reduced = mone
# add a continous variable (since scl is the next in the data column; start with scl)
full = lm(formula = sbpinv ~ dbp + scl, dataext)
full_s = summary(full)
full_s

anova(reduced, full)

# Three-Variable-Model
reduced = full
# add followup 
full = lm(formula = sbpinv ~ dbp + scl + followup, dataext) 

anova(reduced, full)

# Four-Variable-Model
reduced = full
#add  age
full = lm(formula = sbpinv ~ dbp + scl + followup + age, dataext) 

anova(reduced, full)

# Five-Variable-Model
reduced = full
# bmi
full = lm(formula = sbpinv ~ dbp + scl + followup + age + bmi, dataext) 

anova(reduced, full)

# the forward selection is stop at the Four-Variable-Model.

# Summary for the Final Model from the Forward-Selections
mod_forward = lm(formula = sbpinv ~ dbp + scl + followup + age, dataext)
mod_forward_s = summary(mod_forward)
mod_forward_s



# examine the model-diagnostic plots
mfres = mod_forward$residuals
mfpred = predict(mod_forward)

par(mfrow=c(2,2))
hist(mfres, main="Histogram of Residuals")
qqnorm(mfres, main="Normal QQ-plot",xlab="Sample Quantiles",ylab="Theoretic Quantiles")
plot(mfpred, mfres, main="Plot of Residual versus Prediction",xlab="Predictions",ylab="Residuals")
abline(h = mean(mfres))
nmf = length(dataext[,2])
plot(c(1:nmf),mfres, main="Plot of Residual versus Data-Sequence",xlab="Data-Sequence",ylab="Residuals")
abline(h = mean(mfres))
mtext("Model Diagnostic Plots For the Model Selected by the Forward-Selections", side = 3, line = -1, outer = TRUE)



#----------------------------------------------------------------------------
# Best-Subset Regression
library(leaps)

moda = regsubsets(sbpinv ~ dbp + scl + followup + age + bmi + month + sex + chdfate + dscl + dfol + dage + dbmi + dmo, data=dataext, nbest=2, nvmax = 10)
# only keep the best two models in the models with the same #x-variables

modas = summary(moda) 
modas

# get the Adjusted-R^2, Cp and BIC for each model calculated
c1 = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10)
# bind various summary metrics into a data-matrix
modas_M = cbind(c1,modas$adjr2,modas$cp,modas$bic)
modas_Mf = as.data.frame(modas_M)
names(modas_Mf)[1] = "Nvar"
names(modas_Mf)[2] = "AdjR2"
names(modas_Mf)[3] = "Cp"
names(modas_Mf)[4] = "BIC"
modas_Mf

# Best-Subsets Variable-Selection Plots
par(mfrow=c(2,2))
plot(modas$adjr2,xlab="no. variables",ylab="Adjusted R^2",ylim=c(0.40,0.80))
plot(modas$cp,xlab="no. variables",ylab="Cp-value")
plot(modas$bic,xlab="no. variables",ylab="BIC") #no AIC, but BIC values is available
mtext("Best-Subsets Variable-Selection Plots", side = 3, line = -2, outer = TRUE)



# use Adjusted R^2 criterion to select the best model
# run regression for the selected model
modb = lm(formula = sbpinv ~ dbp + age + bmi + sex + chdfate + dage + dbmi + dmo, dataext)
modb_s = summary(modb)
modb_s



# examine the model-diagnostic plots 
modbr = modb$residuals
modbp = predict(modb)

par(mfrow=c(2,2))
hist(modbr, main="Histogram of Residuals")
qqnorm(modbr, main="Normal QQ-plot",xlab="Sample Quantiles",ylab="Theoretic Quantiles")
plot(modbp, modbr, main="Plot of Residual versus Prediction",xlab="Predictions",ylab="Residuals")
abline(h = mean(modbr))
nmf = length(dataext[,2])
plot(c(1:nmf),modbr, main="Plot of Residual versus Data-Sequence",xlab="Data-Sequence",ylab="Residuals")
abline(h = mean(modbr))
mtext("Model Diagnostic Plots for the Best-Model Selected via AdjR2-Criterion", side = 3, line = -1, outer = TRUE)




# mod_forward_s
# modb_s
model_r = lm(formula = sbpinv ~ dbp + age + chdfate + dage, p4dataext)
modb_r = summary(model_r)
vif(model_r)


# mod_forward_s
# modb_s
# vif(mod_forward)
# vif(modb)
modelf = lm(formula = sbpinv ~ dbp + age + chdfate, p4dataext)
modb_f = summary(modelf)
# modb_f
# vif(model_f)

modb_fr = modb_f$residuals
modb_fp = predict(modelf)

par(mfrow=c(2,2)) #it will be a 2 by 2 plot
hist(modb_fr, main="Histogram of Residuals")
qqnorm(modb_fr, main="Normal QQ-plot",ylab="Sample Quantiles",xlab="Theoretic Quantiles")
plot(modb_fp, modb_fr, main="Plot of Residual versus Prediction",xlab="Predictions",ylab="Residuals")
abline(h = mean(modb_fr)) #place an horiztonal line on expected-residual=0 
nmf = length(p4dataext[,2]) #get the size of data to prepare the next plot
plot(c(1:nmf),modb_fr, main="Plot of Residual versus Data-Sequence",xlab="Data-Sequence",ylab="Residuals")
abline(h = mean(modb_fr))
mtext("Model Diagnostic Plots", side = 3, line = -1, outer = TRUE)