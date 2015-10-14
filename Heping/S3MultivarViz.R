#******************************************************
#
#			 		Multivariate Displays
#					Session 3
#
#******************************************************

##############
# Get the data
##############

# Source AccidentInput

source("../../Rcode/AccidentInput.R")

# you should have two data structures in working memory
# First - a list of data frames for each year of accident data

acts <- file.inputl(".")

# Next a data frame with all accidents from all years from 2001 - 2013
# with columns that are consistent for all of these years

# Get a common set the variables
	
	comvar <- intersect(colnames(acts[[1]]), colnames(acts[[8]]))

# the combined data frame

	totacts <- combine.data(acts, comvar)

	dim(totacts)


#*************************************************
#
#		More Data Cleaning
#
#*************************************************

# Variable names

names(totacts)


# View the data types

str(totacts)

# Look at the type for TYPE using summary

totacts$TYPE <- factor(totacts$TYPE, labels = c("Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative" ))

# Use table() to see the frequencies

table(totacts$TYPE)

# Use barplot() to graph this

barplot(table(totacts$TYPE))

# Add color, a title, and change the text size


# Looks at TYPEQ

summary(totacts$TYPEQ)

# First convert to numeric, using as.numeric()


# Now convert to factor

totacts$TYPEQ <- factor(totacts$TYPEQ, labels = c("Freight", "Passenger", "Commuter", "Work",  "Single", "CutofCars", "Yard", "Light", "Maint"))


# Look at CAUSE with summary

# Create a new variable called Cause
# that uses labels for cause.
# Add it to totacts.

totacts$Cause <- rep(NA, nrow(totacts))

totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "M")] <- "M"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "T")] <- "T"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "S")] <- "S"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "H")] <- "H"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "E")] <- "E"

# This new variable, Cause, has to be a factor

totacts$Cause <- factor(totacts$Cause)

# use table() and barplot() to see it.

# Look at histograms of TEMP with different breaks


# Change the color and title

# Now do the summary for totacts, but remove all narratives
# and any other variables you won't use for this project

cbind(1:ncol(totacts), names(totacts))




#**************************************************
#
#			Scatter Plot Matrices
#
#**************************************************

# Scatter plots

plot(2001:2013, tapply(totacts$ACCDMG, as.factor(totacts$YEAR), sum), type = "l", ylab = "Damage ($)", xlab = "Year", main = "Total Damage per Year")



source("../../Rcode/SPM_Panel.R")


# without panel functions for 2010

pairs(~  TRKDMG + EQPDMG + ACCDMG + TOTINJ + TOTKLD, data = acts[[10]])

 # with panel function
 
 uva.pairs(acts[[10]][,c("TRKDMG", "EQPDMG", "ACCDMG", "TOTINJ", "TOTKLD")]) 

# Do this for all accidents


# Print as png to avoid problems in the document

png("metrics.png")
 uva.pairs(acts[[10]][,c("TRKDMG", "EQPDMG", "ACCDMG", "TOTINJ", "TOTKLD")]) 
dev.off()


#**************************************************
#
#		Trellis Categorical Plots
#
#**************************************************


# load the lattice library
install.packages("lattice")
library(lattice)

# Plotting damage per year

bwplot(as.factor(YEAR)~ACCDMG, data = totacts, main = "Box Plots of Accident Damage", xlab = "Damage ($)", ylab = "Year")

# put this in png format

# Find out the worst accident for damage

which(totacts$ACCDMG == max(totacts$ACCDMG))


# Find out the worst accident for total killed and injured


# Plotting accident cause vs. damage


bwplot(Cause~ ACCDMG, main = "Box Plots of Log(Accident Damage)", xlab = "Damage ($)", ylab = "Accident Cause", data = totacts)

bwplot(Cause~ log(ACCDMG+1), main = "Box Plots of Log(Accident Damage)", xlab = "log(Damage ($))", ylab = "Accident Cause", data = totacts)


# Plot cause vs. no. killed or injured

bwplot(Cause ~ TOTKLD, main = "Box Plots of Total Killed", xlab = "Total Killed", ylab = "Accident Cause", data = totacts)

bwplot(Cause~ TOTINJ, main = "Box Plots of Total Injured", xlab = "Total Injured", ylab = "Accident Cause", data = totacts)

# X-Y plots conditioned on cause

xyplot(ACCDMG~TOTKLD | Cause, main = "Damage vs. Killed Conditioned on Cause", xlab = "Total Killed", ylab = "Total Accident Damage", data = totacts)

