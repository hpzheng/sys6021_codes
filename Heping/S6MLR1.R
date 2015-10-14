#    			Session 6
#			
#	 Multiple Linear Regression 1
#
#******************************************************

##load data
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

##Build a data frame with only extreme accidents for ACCDMG
dmgbox <-boxplot(totacts$ACCDMG)
xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]

## Remove duplicates from xdmg and call new data frame xdmgnd
xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]

# Setup categorical variables
xdmgnd$Cause <- rep(NA, nrow(xdmgnd))

xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "M")] <- "M"
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "T")] <- "T"
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "S")] <- "S"
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "H")] <- "H"
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "E")] <- "E"

# This new variable, Cause, has to be a factor

xdmgnd$Cause <- factor(xdmgnd$Cause)

xdmgnd$Type <- factor(xdmgnd$TYPE, labels = c("Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative"))

#***********************************************************
#
#  	Possible predictors of damage	
#
#***********************************************************

# SPM
# Principal components with the correlation matrix for extreme data with 1 metric and quantitative predictors.

source("../../Rcode/SPM_Panel.R")
uva.pairs(xdmgnd[,c("ACCDMG", "TRNSPD", "TONS", "CARS", "TIMEHR", "TEMP")])

# PCA
#Scatter plot matricies for quantitative predictors and single metric.

source("../../Rcode/PCAplots.R")
pred.pca <- princomp(xdmgnd[,c("ACCDMG", "TRNSPD", "TONS", "CARS", "TIMEHR", "TEMP")], cor = T )
metric.pca <- princomp(xdmgnd[,c("ACCDMG", "EQPDMG", "TRKDMG", "TOTKLD", "TOTINJ")], cor = T )

biplot(pred.pca)
biplot(metric.pca)

##Which predictors are most correlated with accident damage?


###############################
# Categorical plots

# heatmap

source("http://www.phaget4.org/R/myImagePlot.R")
myImagePlot(table(xdmgnd$Cause, xdmgnd$Type), title = "No. of Accidents by Cause and Type of Accident")

##Which accident causes and types have the highest numbers of extreme accidents?

# TYPE & TRNSPD

library(lattice)
xyplot(ACCDMG~TRNSPD | TYPE, data = xdmgnd, type = c("p", "r"))


# Cause & TRNSPD

xyplot(ACCDMG~TRNSPD | Cause, data = xdmgnd, type = c("p", "r"))

##What can you determine about the relationship between train speed and accident
##damages for different types / causes of accidents?

# Cause X TYPE and TRNSPD

xyplot(ACCDMG~TRNSPD | Cause * TYPE, data = xdmgnd, type = c("p", "r"))

# Create the Derail variable & 
# then look at interactions with Cause

xdmgnd$Derail <- (xdmgnd$Type == "Derailment")

# plot xy with interactions of Derail and TYPE

xyplot(ACCDMG~TRNSPD | Cause * Derail, data = xdmgnd, type = c("p", "r"))

# Create a Freight variable
# then look at interactions with Cause &
# TYPE

xdmgnd$Freight <- (xdmgnd$TYPEQ == 1)

# Interaction plots

interaction.plot(xdmgnd$TYPE, xdmgnd$Cause, xdmgnd$ACCDMG)

# Get it for derailment

interaction.plot(xdmgnd$Derail, xdmgnd$Cause, xdmgnd$ACCDMG)

# Get it for freight trains

interaction.plot(xdmgnd$Freight, xdmgnd$Cause, xdmgnd$ACCDMG)

# Interaction plots with quantitative variables

Speed <- cut(xdmgnd$TRNSPD, c(min(xdmgnd$TRNSPD),50,max(xdmgnd$TRNSPD)), include.lowest = T, labels = c("low speed", "high speed"))

Weight <- cut(xdmgnd$TONS, c(min(xdmgnd$TONS),50,max(xdmgnd$TONS)), include.lowest = T, labels = c("light", "heavy"))


interaction.plot(Speed, Weight, xdmgnd$ACCDMG)

## How do these results inform your hypotheses?
## Use the multivariate visualiztions as evidence to form at least 1 hypothesis.

####################################
#
#	Now repeat for TOTKLD + TOTINJ
#
####################################

####################################
#
# Linear Models
#
####################################

# Build linear regression models with different combinations of quantitative predictors to provide evidence for your hypothesis

xdmgnd.lm1<-lm(ACCDMG~TEMP,data=xdmgnd)
xdmgnd.lm2<-lm(ACCDMG~TEMP+TRNSPD,data=xdmgnd)
xdmgnd.lm3<-lm(ACCDMG~TEMP+TRNSPD+TONS,data=xdmgnd)
xdmgnd.lm4<-lm(ACCDMG~TEMP+TRNSPD+TONS+CARS,data=xdmgnd)


summary(xdmgnd.lm1)
summary(xdmgnd.lm2)
summary(xdmgnd.lm3)
summary(xdmgnd.lm4)

names(xdmgnd.lm1)
names(xdmgnd.lm2)
names(xdmgnd.lm3)
names(xdmgnd.lm4)

coef(xdmgnd.lm1)

coef(xdmgnd.lm2)

coef(xdmgnd.lm3)

sum(xdmgnd.lm1$res^2)


# 4. Interperet your model coefficients.  Do they make sense?



# 5. Interperet your developed models using the model utility test and t-test.



# 5a. Write out the null and alternative hypothesis for each of the tests.  



# 5b. Do you accept or reject H0 for each test?
