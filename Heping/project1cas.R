# Build a data frame xdmg with only extreme accidents for CASU
totacts$CASU <- totacts$TOTKLD + totacts$TOTINJ

# Remove duplicates from xdmg and call new data frame casnd
casnd <- totacts[!(duplicated(totacts[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]




for (i in c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P'))
{
  assign(paste("Method",i,sep=""), rep(0, nrow(casnd)))
}
MethodA[which(grepl('A', casnd$METHOD) == TRUE)] <- 1
MethodB[which(grepl('B', casnd$METHOD) == TRUE)] <- 1
MethodC[which(grepl('C', casnd$METHOD) == TRUE)] <- 1
MethodD[which(grepl('D', casnd$METHOD) == TRUE)] <- 1
MethodE[which(grepl('E', casnd$METHOD) == TRUE)] <- 1
MethodF[which(grepl('F', casnd$METHOD) == TRUE)] <- 1
MethodG[which(grepl('G', casnd$METHOD) == TRUE)] <- 1
MethodH[which(grepl('H', casnd$METHOD) == TRUE)] <- 1
MethodI[which(grepl('I', casnd$METHOD) == TRUE)] <- 1
MethodJ[which(grepl('J', casnd$METHOD) == TRUE)] <- 1
MethodK[which(grepl('K', casnd$METHOD) == TRUE)] <- 1
MethodL[which(grepl('L', casnd$METHOD) == TRUE)] <- 1
MethodM[which(grepl('M', casnd$METHOD) == TRUE)] <- 1
MethodN[which(grepl('N', casnd$METHOD) == TRUE)] <- 1
MethodO[which(grepl('O', casnd$METHOD) == TRUE)] <- 1
MethodP[which(grepl('P', casnd$METHOD) == TRUE)] <- 1






casnd.lm60<-lm(CASU~MethodA+MethodB+MethodC+MethodD+MethodE+MethodF+MethodG+MethodH+MethodI+MethodJ+MethodK+MethodL+MethodM+MethodN+MethodO+MethodP,data=casnd)
summary(casnd.lm60)
par(mfrow = c(2,2))
plot(casnd.lm60)
par(mfrow = c(1,1))

casnd.lm70<-lm(CASU~(MethodA+MethodB+MethodC+MethodD+MethodE+MethodF+MethodG+MethodH+MethodI+MethodJ+MethodK+MethodL+MethodM+MethodN+MethodO+MethodP)^2,data=casnd)
summary(casnd.lm70)
par(mfrow = c(2,2))
plot(casnd.lm70)
par(mfrow = c(1,1))


# Casulties, first order model
L<-boxcox(casnd.lm60, plotit = F)$x[which.max(boxcox(casnd.lm60, plotit = F)$y)] 
casnd.lm62<-lm(CASU^L~MethodA+MethodB+MethodC+MethodD+MethodE+MethodF+MethodG+MethodH+MethodI+MethodJ+MethodK+MethodL+MethodM+MethodN+MethodO+MethodP,data=casnd)

# Display regression results for boxcox model
summary(casnd.lm62)
par(mfrow = c(2,2))
plot(casnd.lm62)
par(mfrow =c(1,1))

# Casulties, second order model
L<-boxcox(casnd.lm70, plotit = F)$x[which.max(boxcox(casnd.lm70, plotit = F)$y)] 
casnd.lm72<-lm(CASU^L~(MethodA+MethodB+MethodC+MethodD+MethodE+MethodF+MethodG+MethodH+MethodI+MethodJ+MethodK+MethodL+MethodM+MethodN+MethodO+MethodP)^2,data=casnd)

# Display regression results for boxcox model
summary(casnd.lm72)
par(mfrow = c(2,2))
plot(casnd.lm72)
par(mfrow =c(1,1))
