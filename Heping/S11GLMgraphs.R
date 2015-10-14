


#************************************************************
#
#				SPAM Filter 1 
#			Graphical Analysis of SPAM 
#
#
#************************************************************


#
# Load the data, Spam.txt (no headers)
# Descriptions of the variables are in spam.names.txt
# Source SPM_Panel.R, PCAplots.R and 
# 
#

spam <- read.table("Spam.txt", sep = " ", header = F)
path<-setwd(sourcedir)
#**************************************************************
source("SPM_Panel.r")
source("PCAplots.R")
source("FactorPlots.r")

#**************************************************************
#
#				Graphical Analysis
#
#**************************************************************

# Which variable is the response variable?

# What proportion is spam?

sum(spam[,58])/length(spam[,58])

# What do we know about variables 1-47? Variables 48-57?

# What do the SPM tell us? Look at variables 1-10 & 58 
# use png format

uva.pairs(spam[,c(1:10,58)])

uva.pairs(log(spam[,c(1:10,58)]+.00001))

# Look at spm for variables 48-58.
uva.pairs(log(spam[,c(48:57,58)]+.00001))


# Obtain boxplots with variables 1-9 vs. the response.
# Which variable are more discriminatory?
par(mfrow = c(3,3))
for(i in 1:9)
{
  boxplot(spam[,i]~spam[,58], xlab = "Type of Email", ylab = "Count", main = paste("Variable", i))  
}
par(mfrow = c(1,1))


# Obtain boxplots with variables 49-57 vs. the response.
# Which variable are more discriminatory?


#****************************************************
#
#		Principal Components
#
#****************************************************

# Obtain the principal components for variables 1-57. 
# Look at the biplot and explain what you see.


# What is the outlier?



# Obtain the biplot.fact of your principal components.
# Explain what you see.


#***************************************************************
#
#		Log of Predictors
#
#***************************************************************

# Repeat the above graphical analysis with a log transform of the
# predictor variables



#1. Obtain box plots for log transforms of variables 1-9 and then 49-57.



#2. Obtain scatter plot matices for log transforms of variables 1-9 with variable 58 and then log transforms of variables 49-57 with variable 58.



#3. Obtain the principal components for the log transform of variables 1-57. 
#   Look at the biplot and explain what you see.



#4. Obtain the biplot.fact of your principal components for the log transformed variables.
#   Explain what you see.

