#
#      	     Session 8
#			Multiple Linear Regression
#	 Diagnostics & Transformations
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


# Build a data frame xdmg with only extreme accidents for ACCDMG
dmgbox <-boxplot(totacts$ACCDMG)
xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]

# Remove duplicates from xdmg and call new data frame xdmgnd
xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]


##******************************************* 
## Build linear regression models in R: lm ##
##*******************************************

# Build 2 multiple linear regression models with the data fram xdmgnd: 
# xdmgnd.lm1  ACCDMG ~ TEMP + TRNSPD + TONS + CARS + HEADEND1
# xdmgnd.lm2  ACCDMG ~ TEMP + TONS + CARS 

xdmgnd.lm1<-lm(ACCDMG ~ TEMP + TRNSPD + TONS + CARS + HEADEND1,data=xdmgnd)
xdmgnd.lm2<-lm(ACCDMG ~ TEMP + TONS + CARS ,data=xdmgnd)

# Display regression results for each model
summary(xdmgnd.lm1)
summary(xdmgnd.lm2)

# Source TestSet.R
source("../../Rcode/TestSet.R")


# create vectors to store PMSE
pmse1.result<-NULL; #Two empty vectors to record PMSEs
pmse2.result<-NULL;

for (i in c(1:20)){
  #set test sets size: 
  test.size<-1/3
  # generate training sets and test sets from original data:
  xdmgnd.data<-test.set(xdmgnd,test.size)
  
  # Build model with train set:
  lm1.train<-lm(ACCDMG ~ TEMP + TRNSPD + TONS + CARS + HEADEND1,data=xdmgnd.data$train)
  lm2.train<-lm(ACCDMG ~ TEMP + TONS + CARS,data=xdmgnd.data$train)
  
  # First, how to predict with lm models:
  lm1.pred<-predict(lm1.train,newdata=xdmgnd.data$test) 
  lm2.pred<-predict(lm2.train,newdata=xdmgnd.data$test) 
  
  # Next, compute PMSE:
  pmse.lm1<-mse(lm1.pred,xdmgnd.data$test$ACCDMG)
  pmse.lm2<-mse(lm2.pred,xdmgnd.data$test$ACCDMG)
  
  # Add the PMSE for this run into your vector to stor PMSE
  pmse1.result<-c(pmse1.result,pmse.lm1)
  pmse2.result<-c(pmse2.result,pmse.lm2)
}

plot(1, 2, xlab = "Test")
# Compare models based over 20 runs of PMSE
plot(pmse1.result,type='b',col='blue',xlab="Index", ylab="PMSE")
lines(pmse2.result,type='b',col='red')
title(main="Model Comparison Based on PMSE")

# 1. Which model is better from visual inspection of graph?

# The blue one

# We can also use statistical tests to compare our models.  


# Paired t test:
t.test(pmse1.result,pmse2.result,paired=T)

# Wilcoxon Test:
wilcox.test(pmse1.result,pmse2.result,paired=T)


# 2. Which model performs better based on the paired t test and paired wilcox test?


##************************* 
## Diagnostics Plot      ##
##************************* 
# Generate diagnostics plot one by one
plot(xdmgnd.lm1,labels.id = NULL)

#Plot all four plots together
par(mfrow=c(2,2))
plot(xdmgnd.lm1, labels.id = NULL)
par(mfrow=c(1,1)) 

#Save the above plot as png:
png('diagnostics.png')
par(mfrow=c(2,2))
plot(xdmgnd.lm1, labels.id = NULL)
par(mfrow=c(1,1)) 
dev.off()

#Plot graphs individually
plot(xdmgnd.lm1,which=1) #Residual vs. Fitted
plot(xdmgnd.lm1,which=2) #QQ
plot(xdmgnd.lm1,which=3) #Scale-Location
plot(xdmgnd.lm1,labels.id = NULL, which=4) #Cook's distance
plot(xdmgnd.lm1,which=5) #Redisuals vs. Leverage
plot(xdmgnd.lm1,which=6) #Cook's dist vs. Leverage

# Take a closer look at observations with a high Cook's D




# 3. What happened in each of the accidents noted on the Cook's Distance plot?




# 4. What do you observe in each diagnostic plot?  Discuss your observations and any issues.

# a. residuals vs. fitted

# b. qq-plot

# c. Scale-Location?

# d. Cook's distance?

# e. Redisuals vs. Leverage?

# f. Cook's dist vs. Leverage?



# Let's take a look at the response variable ACCDMG.  
plot(density(xdmg$ACCDMG))

# 5. Do we violate the distributional assumption of our response variable?



##************************* 
## Box-Cox Plot          ##
##************************* 
# load the MASS library 
library(MASS)
boxcox(xdmgnd.lm1) #box-cox plot
boxcox(xdmgnd.lm1, plotit=T, lambda=seq(-2,2,by=0.5))
boxcox(xdmgnd.lm1,plotit=F) #values
max(boxcox(xdmgnd.lm1, plotit = F)$y)
boxcox(xdmgnd.lm1, plotit = F)$x[which.max(boxcox(xdmgnd.lm1, plotit = F)$y)] 

#The best lambda
L<-boxcox(xdmgnd.lm1, plotit = F)$x[which.max(boxcox(xdmgnd.lm1, plotit = F)$y)] 

#The best lambda
xdmgnd.lm1.boxcox<-lm(ACCDMG^-L ~TEMP+TRNSPD+TONS+CARS+HEADEND1,data=xdmgnd)

# Display regression results for boxcox model
summary(xdmgnd.lm1.boxcox)

# Let's replot our density function for our response variable ACCDMG
plot(density((xdmgnd$ACCDMG^L)))


# Plot diagnostic for your new model xdmgnd.lm1.boxcox
par(mfrow=c(2,2))
plot(xdmgnd.lm1.boxcox)
par(mfrow=c(1,1))


# 6. What do you observe in the diagnostic plots for your new model xdmgnd.lm1.boxcox?  Did the transformation help?




# Now, let's try a logarithm transform of the response variable ACCDMG.
par(mfrow=c(2,2))
plot((density(xdmgnd$ACCDMG)))
plot((density(log(xdmgnd$ACCDMG))))
plot((density(xdmgnd$ACCDMG^L)))
par(mfrow=c(1,1))


# Build a new model with log(ACCDMG) as the respnse
xdmgnd.lm1.log<-lm(log(ACCDMG) ~TEMP+TRNSPD+TONS+CARS+HEADEND1,data=xdmgnd)

# Plot diagnostics for your new model xdmgnd.lm1.log
par(mfrow=c(2,2))
plot(xdmgnd.lm1.log)
par(mfrow=c(1,1))
  
  
  
# 7. What do you observe in the diagnostic plots for your new model xdmgnd.lm1.log?  Which trasformation do you choose?


