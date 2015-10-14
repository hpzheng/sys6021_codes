
for (i in c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P'))
{
  assign(paste("Method",i,sep=""), rep(0, nrow(xdmgnd)))
}
MethodA[which(grepl('A', xdmgnd$METHOD) == TRUE)] <- 1
MethodB[which(grepl('B', xdmgnd$METHOD) == TRUE)] <- 1
MethodC[which(grepl('C', xdmgnd$METHOD) == TRUE)] <- 1
MethodD[which(grepl('D', xdmgnd$METHOD) == TRUE)] <- 1
MethodE[which(grepl('E', xdmgnd$METHOD) == TRUE)] <- 1
MethodF[which(grepl('F', xdmgnd$METHOD) == TRUE)] <- 1
MethodG[which(grepl('G', xdmgnd$METHOD) == TRUE)] <- 1
MethodH[which(grepl('H', xdmgnd$METHOD) == TRUE)] <- 1
MethodI[which(grepl('I', xdmgnd$METHOD) == TRUE)] <- 1
MethodJ[which(grepl('J', xdmgnd$METHOD) == TRUE)] <- 1
MethodK[which(grepl('K', xdmgnd$METHOD) == TRUE)] <- 1
MethodL[which(grepl('L', xdmgnd$METHOD) == TRUE)] <- 1
MethodM[which(grepl('M', xdmgnd$METHOD) == TRUE)] <- 1
MethodN[which(grepl('N', xdmgnd$METHOD) == TRUE)] <- 1
MethodO[which(grepl('O', xdmgnd$METHOD) == TRUE)] <- 1
MethodP[which(grepl('P', xdmgnd$METHOD) == TRUE)] <- 1






xdmgnd.lm10<-lm(ACCDMG~MethodA+MethodB+MethodC+MethodD+MethodE+MethodF+MethodG+MethodH+MethodI+MethodJ+MethodK+MethodL+MethodM+MethodN+MethodO+MethodP,data=xdmgnd)
summary(xdmgnd.lm10)
par(mfrow = c(2,2))
plot(xdmgnd.lm10)
par(mfrow = c(1,1))

xdmgnd.lm20<-lm(ACCDMG~(MethodA+MethodB+MethodC+MethodD+MethodE+MethodF+MethodG+MethodH+MethodI+MethodJ+MethodK+MethodL+MethodM+MethodN+MethodO+MethodP)^2,data=xdmgnd)
summary(xdmgnd.lm20)
par(mfrow = c(2,2))
plot(xdmgnd.lm20)
par(mfrow = c(1,1))



xdmgnd.lm11<-lm(log(ACCDMG)~MethodA+MethodB+MethodC+MethodD+MethodE+MethodF+MethodG+MethodH+MethodI+MethodJ+MethodK+MethodL+MethodM+MethodN+MethodO+MethodP,data=xdmgnd)
summary(xdmgnd.lm11)
plot(density(log(xdmgnd$ACCDMG)))
par(mfrow = c(2,2))
plot(xdmgnd.lm11)
par(mxdmgnd.c(1,1))


library(MASS)
boxcox(xdmgnd.lm10) #box-cox plot
boxcox(xdmgnd.lm10, plotit=T, lambda=seq(-2,2,by=0.5))
boxcox(xdmgnd.lm10, plotit=F)
max(boxcox(xdmgnd.lm10, plotit = F)$y)
boxcox(xdmgnd.lm10, plotit = F)$x[which.max(boxcox(xdmgnd.lm10, plotit = F)$y)] 

# ACCDMG, first order model
L<-boxcox(xdmgnd.lm10, plotit = F)$x[which.max(boxcox(xdmgnd.lm10, plotit = F)$y)] 
xdmgnd.lm12<-lm(ACCDMG^L~MethodA+MethodB+MethodC+MethodD+MethodE+MethodF+MethodG+MethodH+MethodI+MethodJ+MethodK+MethodL+MethodM+MethodN+MethodO+MethodP,data=xdmgnd)

# Display regression results for boxcox model
summary(xdmgnd.lm12)
par(mfrow = c(2,2))
plot(xdmgnd.lm12)
par(mfrow =c(1,1))

# ACCDMG, second order model
L<-boxcox(xdmgnd.lm20, plotit = F)$x[which.max(boxcox(xdmgnd.lm20, plotit = F)$y)] 
xdmgnd.lm22<-lm(ACCDMG^L~(MethodA+MethodB+MethodC+MethodD+MethodE+MethodF+MethodG+MethodH+MethodI+MethodJ+MethodK+MethodL+MethodM+MethodN+MethodO+MethodP)^2,data=xdmgnd)

# Display regression results for boxcox model
summary(xdmgnd.lm22)
par(mfrow = c(2,2))
plot(xdmgnd.lm22)
par(mfrow =c(1,1))




source("../../Rcode/TestSet.R")


# create vectors to store PMSE
pmse10.result<-NULL; #Two empty vectors to record PMSEs
pmse12.result<-NULL;

for (i in c(1:20)){
  #set test sets size: 
  test.size<-1/3
  # generate training sets and test sets from original data:
  xdmgnd.data<-test.set(xdmgnd,test.size)
  
  # Build model with train set:
  lm10.train<-lm(ACCDMG ~ MethodA+MethodB+MethodC+MethodD+MethodE+MethodF+MethodG+MethodH+MethodI+MethodJ+MethodK+MethodL+MethodM+MethodN+MethodO+MethodP, data=xdmgnd.data$train)
  lm12.train<-lm(ACCDMG^L ~ MethodA+MethodB+MethodC+MethodD+MethodE+MethodF+MethodG+MethodH+MethodI+MethodJ+MethodK+MethodL+MethodM+MethodN+MethodO+MethodP, data=xdmgnd.data$train)
  
  # First, how to predict with lm models:
  lm10.pred<-predict(lm10.train,newdata=xdmgnd.data$test) 
  lm12.pred<-predict(lm12.train,newdata=xdmgnd.data$test) 
  
  # Next, compute PMSE:
  pmse.lm10<-mse(lm10.pred,xdmgnd.data$test$ACCDMG)
  pmse.lm12<-mse(lm12.pred,xdmgnd.data$test$ACCDMG)
  
  # Add the PMSE for this run into your vector to stor PMSE
  pmse10.result<-c(pmse10.result,pmse.lm10)
  pmse12.result<-c(pmse12.result,pmse.lm12)
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

