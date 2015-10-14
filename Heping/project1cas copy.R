# Build a data frame xdmg with only extreme accidents for CASU
totacts$CASU <- totacts$TOTKLD + totacts$TOTINJ
cas <- totacts[totacts$CASU > 0 & totacts$CASU < 1000, ]

casnd <- cas[!(duplicated(cas[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]

casbox <- boxplot(casnd$CASU)
xcasnd <- casnd[casnd$CASU > casbox$stats[5],]


for (i in c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P'))
{
  assign(paste("Method",i,sep=""), rep(0, nrow(casnd)))
  eval(parse(text=paste("Method",i,"[which(grepl('",i,"', casnd$METHOD) == TRUE)] <- 1",sep="")))
}
#=== xcasnd ===
for (i in c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P'))
{
  assign(paste("Method",i,sep=""), rep(0, nrow(xcasnd)))
  eval(parse(text=paste("Method",i,"[which(grepl('",i,"', xcasnd$METHOD) == TRUE)] <- 1",sep="")))
}

xcasnd.lm60<-lm(CASU~MethodA+MethodB+MethodC+MethodD+MethodE+MethodF+MethodG+MethodH+MethodI+MethodJ+MethodK+MethodL+MethodM+MethodN+MethodO+MethodP,data=xcasnd)
summary(xcasnd.lm60)
L<-boxcox(xcasnd.lm60, plotit = F)$x[which.max(boxcox(xcasnd.lm60, plotit = F)$y)] 
xcasnd.lm62<-lm(CASU^L~MethodA+MethodB+MethodC+MethodD+MethodE+MethodF+MethodG+MethodH+MethodI+MethodJ+MethodK+MethodL+MethodM+MethodN+MethodO+MethodP,data=xcasnd)
summary(xcasnd.lm62)
par(mfrow = c(2,2))
plot(xcasnd.lm62)
par(mfrow = c(1,1))
AIC(xcasnd.lm62)
BIC(xcasnd.lm62)
AIC(casnd.lm62)
BIC(casnd.lm62)
# end === xcasnd ===

casnd.lm60<-lm(CASU~MethodA+MethodB+MethodC+MethodD+MethodE+MethodF+MethodG+MethodH+MethodI+MethodJ+MethodK+MethodL+MethodM+MethodN+MethodO+MethodP,data=casnd)
summary(casnd.lm60)
par(mfrow = c(2,2))
plot(casnd.lm60)
par(mfrow = c(1,1))

casnd.lm60.step <- step(casnd.lm60, trace=F)
summary(casnd.lm60.step)

casnd.lm70<-lm(CASU~(MethodA+MethodB+MethodC+MethodD+MethodE+MethodF+MethodG+MethodH+MethodI+MethodJ+MethodK+MethodL+MethodM+MethodN+MethodO+MethodP)^2,data=casnd)
summary(casnd.lm70)
par(mfrow = c(2,2))
plot(casnd.lm70)
par(mfrow = c(1,1))


# Casulties, first order model
library(MASS)
boxcox(casnd.lm60) #box-cox plot
boxcox(casnd.lm60, plotit=T, lambda=seq(-2,2,by=0.5))
boxcox(casnd.lm60, plotit=F)
max(boxcox(casnd.lm60, plotit = F)$y)
L<-boxcox(casnd.lm60, plotit = F)$x[which.max(boxcox(casnd.lm60, plotit = F)$y)] 
casnd.lm62<-lm(CASU^L~MethodA+MethodB+MethodC+MethodD+MethodE+MethodF+MethodG+MethodH+MethodI+MethodJ+MethodK+MethodL+MethodM+MethodN+MethodO+MethodP,data=casnd)
casnd.lm63<-lm(log(CASU)~MethodA+MethodB+MethodC+MethodD+MethodE+MethodF+MethodG+MethodH+MethodI+MethodJ+MethodK+MethodL+MethodM+MethodN+MethodO+MethodP,data=casnd)
xcasnd.lm63<-lm(log(CASU)~MethodA+MethodB+MethodC+MethodD+MethodE+MethodF+MethodG+MethodH+MethodI+MethodJ+MethodK+MethodL+MethodM+MethodN+MethodO+MethodP,data=xcasnd)
par(mfrow = c(2,2))
plot(xcasnd.lm63)
par(mfrow =c(1,1))


casnd.lm62.step <- step(casnd.lm62, trace=F)
summary(casnd.lm62.step)

# Display regression results for boxcox model
summary(casnd.lm62)
par(mfrow = c(2,2))
plot(casnd.lm62)
par(mfrow =c(1,1))

summary(casnd.lm63)
par(mfrow = c(2,2))
plot(casnd.lm63)
par(mfrow =c(1,1))

summary(casnd.lm62.step)
par(mfrow = c(2,2))
plot(casnd.lm62.step)
par(mfrow =c(1,1))

# Casulties, second order model
L<-boxcox(casnd.lm70, plotit = F)$x[which.max(boxcox(casnd.lm70, plotit = F)$y)] 
casnd.lm72<-lm(CASU^L~(MethodA+MethodB+MethodC+MethodD+MethodE+MethodF+MethodG+MethodH+MethodI+MethodJ+MethodK+MethodL+MethodM+MethodN+MethodO+MethodP)^2,data=casnd)

# Display regression results for boxcox model
summary(casnd.lm72)
par(mfrow = c(2,2))
plot(casnd.lm72)
par(mfrow =c(1,1))
