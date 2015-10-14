#
#      	     Session 9
#			Multiple Linear Regression
#	 Transformations & Qualitative Variables
#
#******************************************************

source("/Users/dust/src/sys6021/AccidentInput.R")

setwd("/Users/dust/Courses/sys4021/data/TrainAccidents")
my.path <- getwd()
setwd(my.path)

acts <- file.inputl(my.path)

setdiff(colnames(acts[[1]]), colnames(acts[[8]]))
comvar <- intersect(colnames(acts[[1]]),colnames(acts[[8]]))

totacts <- combine.data(acts, comvar)

# Build a data frame xdmg with only extreme accidents for ACCDMG
dmgbox <-boxplot(totacts$ACCDMG)
xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]

# Remove duplicates from xdmg and call new data frame xdmgnd
xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]

totacts$CASU <- totacts$TOTKLD + totacts$TOTINJ
cas <- totacts[totacts$CASU > 0 & totacts$CASU < 1000, ]
casnd <- cas[!(duplicated(cas[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]
xcasnd <- casnd[which(casnd$CASU >= 3),]

for (i in c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P'))
{
  eval(parse(text=paste("xdmgnd$Method",i," <- rep(0, nrow(xdmgnd))",sep="")))
  eval(parse(text=paste("xdmgnd$Method",i,"[which(grepl('",i,"', xdmgnd$METHOD) == TRUE)] <- 1",sep="")))
  eval(parse(text=paste("casnd$Method",i," <- rep(0, nrow(casnd))",sep="")))
  eval(parse(text=paste("casnd$Method",i,"[which(grepl('",i,"', casnd$METHOD) == TRUE)] <- 1",sep="")))
  eval(parse(text=paste("xcasnd$Method",i," <- rep(0, nrow(xcasnd))",sep="")))
  eval(parse(text=paste("xcasnd$Method",i,"[which(grepl('",i,"', xcasnd$METHOD) == TRUE)] <- 1",sep="")))
}

par(mfrow=c(2,8))
for (i in c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P'))
{
  eval(parse(text=paste("plot(as.factor(xdmgnd$Method",i,"))",sep="")))
  title(main=paste("Method",i,sep=""))
}
par(mfrow=c(1,1))


TRKCLAS <- c('X','1','2','3','4','5','6','7','8','9')
SPDFREIGHT <- c(10, 10, 25, 40, 60, 80, 110, 125, 160, 200)
SPDPASSENGER <- c(0, 15, 30, 60, 80, 90, 110, 125, 160, 200)
eqpspd.data <- data.frame(TRKCLAS, SPDFREIGHT, SPDPASSENGER)

xdmgnd <- merge(xdmgnd, eqpspd.data, by="TRKCLAS", all.x=TRUE)
xdmgnd$SPDDIFF <- xdmgnd$TRNSPD-xdmgnd$SPDFREIGHT
xdmgnd$Speeding <- rep(0, nrow(xdmgnd))
xdmgnd$Speeding[which(xdmgnd$SPDDIFF > 0)] <- 1


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

#  create models with interactions between all of the quantitative predictors ----
xdmgnd.lm3<-lm(ACCDMG~(TEMP+TRNSPD+TONS+CARS+HEADEND1)^2,data=xdmgnd)
summary(xdmgnd.lm3) 

# Is this the complete second order model?

#  I() allows your model to contain normal mathematical sysmbols 
#  Create complete second order model                            
xdmgnd.lm4<-lm(ACCDMG~(TEMP+TRNSPD+TONS+CARS+HEADEND1)^2+I(TEMP^2)+I(TRNSPD^2)+I(TONS^2)+I(CARS^2)+I(HEADEND1^2),data=xdmgnd)
summary(xdmgnd.lm4)

# How many parameters and coefficients are in each of the models?


# Create a stepwise regression model on xdmgnd.lm4
xdmgnd.lm4.step <- step(xdmgnd.lm4)
summary(xdmgnd.lm4.step)


# partial F test ----
anova(xdmgnd.lm3,xdmgnd.lm4)

# Which model is better based on the partial F test, the larger or smaller?


# Interaction Plot Example
TONS.factor<-xdmgnd$TONS
TONS.factor[which(xdmgnd$TONS<142)]<-'low tons'
TONS.factor[which(xdmgnd$TONS>=142)]<-'high tons'

# create an interaction plot for TRNSPD in terms of high weight and low weight accidents
interaction.plot(xdmgnd$TRNSPD,TONS.factor,log(xdmgnd$ACCDMG))


##************************* 
## Qualitative Variables ##
##************************* 

# Create a qualitative variable for Cause 
Cause <- rep(NA, nrow(xdmgnd))
Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "M")] <- "M"  ##Miscellaneous
Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "T")] <- "T"  ##Track
Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "S")] <- "S"  ##Signal or communication
Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "H")] <- "H"  ##Human
Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "E")] <- "E"  ##Electrical or mechanical

# This new variable, Cause, has to be a factor
Cause <- as.factor(Cause)

# Lets look at the default treatment coding
contrasts(Cause)

# What is the base case for Cause?



# Write a model to predict ACCDMG in terms of Cause 
xdmgnd.lm5<-lm(ACCDMG~Cause,data=xdmgnd)
summary(xdmgnd.lm5) 

# How do we interperet the model xdmgnd. lm5?


#Chage based case to H ----
contrasts(Cause)<-matrix(c(1,0,0,0,0, 0,0,1,0,0, 0,0,0,1,0,  0,0,0,0,1),nrow=5)
colnames(contrasts(Cause)) <-matrix(c("E","M","S","T"),ncol=4)
contrasts(Cause)

xdmgnd.lm6<-lm(ACCDMG~Cause,data=xdmgnd)
summary(xdmgnd.lm6)

#How do we interperet the model xdmgnd.lm6?


#More qualitative variables:
xdmgnd.lm7<-lm(ACCDMG~as.factor(WEATHER),data=xdmgnd)
summary(xdmgnd.lm7) #What is the base case?


# Recode the qualitative variable Weather using the categories in accident descriptions pdf document
Weather <- rep(NA, nrow(xdmgnd))
Weather[which(xdmgnd$WEATHER == 1)] <- "clear"
Weather[which(xdmgnd$WEATHER == 2)] <- "cloudy"
Weather[which(xdmgnd$WEATHER == 3)] <- "rain"
Weather[which(xdmgnd$WEATHER == 4)] <- "fog"
Weather[which(xdmgnd$WEATHER == 5)] <- "sleet"
Weather[which(xdmgnd$WEATHER == 6)] <- "snow"

xdmgnd.lm8<-lm(ACCDMG~Weather,data=xdmgnd)
summary(xdmgnd.lm8)


# What is the base case?


# Build a model with the qualitative variable weather



# What is your conclusion?



# Build a new model to test the hypothesis: "Accidents of type derailment do not increase the severity of rail road accidents."
 
TypeD <- rep(NA, nrow(xdmgnd))
TypeD[which(xdmgnd$TYPE != 01)] <- "Others"
TypeD[which(xdmgnd$TYPE == 01)] <- "Derail"
table(TypeD)

MethodUsed <- rep(NA, nrow(xdmgnd))
MethodUsed[which(xdmgnd$METHOD == "")] <- "-"
for (i in c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P'))
{
  length(grep(i,xdmgnd$METHOD))
}


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



table(MethodUsed)



# What is the base case?

contrasts(factor(MethodUsed))
          


# What is your conclusion?

xdmgnd.lm10<-lm(ACCDMG~MethodA+MethodB+MethodC+MethodD+MethodE+MethodF+MethodG+MethodH+MethodI+MethodJ+MethodK+MethodL+MethodM+MethodN+MethodO+MethodP,data=xdmgnd)
summary(xdmgnd.lm10)
plot(xdmgnd.lm10)

anova(xdmgnd.lm9,xdmgnd.lm10)
The conclusion is to accept the NULL hypothesis: ""Accidents of type derailment do not increase the severity of rail road accidents.""