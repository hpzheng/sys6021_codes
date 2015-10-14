#      		Session 7
#			
#	 Multiple Linear Regression 2
#
#******************************************************

#load data
source("AccidentInput.R")

setwd(traindir)

my.path <- getwd()
setwd(my.path)

acts <- file.inputl(my.path)

sapply(acts, dim)
dim(acts[[12]])

setdiff(colnames(acts[[1]]), colnames(acts[[8]]))
comvar <- intersect(colnames(acts[[1]]),colnames(acts[[8]]))

totacts <- combine.data(acts, comvar)


##Build a data frame xdmg with only extreme accidents for ACCDMG



# Remove duplicates from xdmg and call new data frame xdmgnd




##******************************************* 
## Build linear regression models in R: lm ##
##*******************************************

# Linear regression models with quantitative predictors
xdmgnd.lm1<-lm(ACCDMG~TEMP,data=xdmgnd)
xdmgnd.lm2<-lm(ACCDMG~TEMP+TRNSPD,data=xdmgnd)
xdmgnd.lm3<-lm(ACCDMG~TEMP+TRNSPD+TONS,data=xdmgnd)

# The next two lines of R code are equivalent
xdmgnd.lm4<-lm(ACCDMG~TEMP+TRNSPD+TONS+CARS,data=xdmgnd)

xdmgnd.lm4<-lm(ACCDMG~.,data=xdmgnd[,c('ACCDMG','TEMP','TRNSPD','TONS','CARS')])

# Display regression results:
summary(xdmgnd.lm1)


# You should be able to find: estimated coefficients, residuals, t-test results, F test results, R^2, adjusted R^2,
names(xdmgnd.lm1)


##What are the coefficients of each of the linear models?
coef(xdmgnd.lm1)


##what is the sum of the residuals squared?
sum(xdmgnd.lm1$res^2)

################################################ Metrics and Variable Selection      ################################################

##******************************* 
## Criterion based assessments ##
##*******************************

# Adjusted R^2:
summary(xdmgnd.lm1)$adj.r.squared


# AIC:
AIC(xdmgnd.lm1)


#BIC:
AIC(xdmgnd.lm1,k=log(nrow(totacts)))



# Assess the 4 models using criterion based assessment approaches.  Which performs better based on each criterion?


##************************
## Stepwise Regression  ##
##************************ 
xdmgnd.lm4.step<-step(xdmgnd.lm4)

# If you have many predictors, it will take some time to get results. To save time, you can set 'trace=F' to get reults without showing each step:
xdmgnd.lm4.step<-step(xdmgnd.lm4, trace=F)
summary(xdmgnd.lm4.step)


# What predictors are left in your stepwise model?

##****************** 
## Partial F Test ##
##******************

# Recall that we can only compare two nested models by partial F test:
anova(xdmgnd.lm1,xdmgnd.lm2)


# Compare your stepwise model to model 4 using the partial F test.




##****************** 
## Test Sets      ##
##******************

setwd(sourcedir)
source("TestSet.R")

#set test sets size: 
test.size<-1/3

# generate training sets and test sets from original data:
xdmgnd.data<-test.set(xdmgnd,test.size)

# Check distribution of ACCDMG of test set, training set:
par(mfrow=c(2,2))
hist(xdmgnd.data$train$ACCDMG)
hist(xdmgnd.data$test$ACCDMG)
hist(xdmgnd$ACCDMG)
par(mfrow=c(1,1))

# Are the training and test sets representative of total data?


# Build model with train set:
xdmgnd.lm4.train<-lm(ACCDMG~TEMP+TRNSPD+TONS+CARS,data=xdmgnd.data$train)


# Recall that we need to measure predicted MSE. 
# First, how to predict with lm models:
xdmgnd.lm4.pred<-predict(xdmgnd.lm4.train,newdata=xdmgnd.data$test) 


# Next, compute PMSE:
pmse.xdmgnd.lm4<-mse(xdmgnd.lm4.pred,xdmgnd.data$test$ACCDMG)
pmse.xdmgnd.lm4


# Compare xdmgnd.lm4 and xdmgnd.lm3 based on PMSE.  Which model performs better?


##******************** 
## Cross-Validation ##
##********************

# Need the boot library
library(boot)

# You need to use glm (a funciton to estimate generalized linear model) instead of lm. Don't be confused by generalized linear models. 
# Because lm is a special case of glm, glm function can be used to estimate lm models as long as you set parameters correctly. 
xdmgnd.lm4.cv<-glm(ACCDMG~TEMP+TRNSPD+TONS+CARS,data=xdmgnd)


# Cross-validation:
xdmgnd.lm4.err<-cv.glm(xdmgnd,xdmgnd.lm4.cv,K=10)
xdmgnd.lm4.err$delta

#There are two components for estimated errors: the first is the raw cross-validation estimate of prediction error; the second is the adjusted cross-validation estimate.



# Compare xdmgnd.lm4 and xdmgnd.lm3 based on adjusted cross-validation estimate.  Which model performs better?