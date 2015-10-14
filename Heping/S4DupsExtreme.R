

#******************************************************
#
#					Session 4
#				Duplicates, 
#				Categorial Variable Relationships &
#				Extreme values
#
#******************************************************

##############
# Get the data
##############

# Source AccidentInput

source("../../Rcode/AccidentInput.R")

# you should have two data structures in working memory
# First - a list of data frames for each year of accident data

acts <- file.inputl(getwd())

# Next a data frame with all accidents from all years from 2001 - 2014
# with columns that are consistent for all of these years

# Get a common set the variables
	
comvar <- intersect(colnames(acts[[1]]), colnames(acts[[8]]))

# the combined data frame

totacts <- combine.data(acts, comvar)

dim(totacts)


#***********************************************************
#
#			Extreme Points
#
#***********************************************************

hist(totacts$ACCDMG)

#Look also at TOTKLD and TOTINJ

# Get the values in the box plot

dmgbox <- boxplot(totacts$ACCDMG)

# extreme points

length(dmgbox$out)

# What proportion of accidents are extreme?
	
length(dmgbox$out)/nrow(totacts)

# Proportion of costs
sum(totacts$ACCDMG[which(totacts$ACCDMG > dmgbox$stat[5])]) / sum(totacts$ACCDMG)

# Create a data frame with just the extreme ACCDMG accidents

xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]

dim(xdmg)

# Look at the graphs of these extreme accidents

hist(xdmg$ACCDMG)
boxplot(xdmg$ACCDMG, col = "steelblue", main = "Accidents with Extreme Damage", ylab = "Cost ($)")

plot(1:14, tapply(xdmg$ACCDMG, xdmg$YEAR, sum), type = "l", xlab = "Year", ylab = "Total Damage ($)", main = "Total Accident Damage per Year")

# also plot number of accidents per year.
# use year as a factor


# Frequency of accident types

barplot(table(xdmg$TYPE)) #compare with the totacts plot

# SPM of metrics

# SPM of metrics & train variables

uva.pairs(xdmg[,c("ACCDMG", "TOTKLD", "TOTINJ", "TRNSPD", "TONS" )])

# Categorical variables

# Cause

bwplot(Cause~ ACCDMG, main = "Box Plots of Accident Damage by Cause", xlab = "Damage ($)", ylab = "Accident Cause", data = xdmg)

# Type of accident

which(totacts$ACCDMG > dmgbox$stats[5])
totacts[1429, 123:137]
totacts$TOTKLD[1429]
totacts$TOTINJ[1429]
totacts$ACCDMG[1429]
max(totacts$TOTKLD)

# Year


#Use of jitter

bwplot(as.factor(YEAR)~jitter(ACCDMG, amount = 2.5e5), data = xdmg, main = "Box Plots of Extreme Accident Damage by Year (with jitter)", xlab = "Damage ($)", ylab = "Year")


par(mfrow = c(1,2))
boxplot(jitter(xdmg$ACCDMG, amount = 2.5e5), col = "steelblue", main = "Extreme Accident Damage with Jitter")
boxplot(xdmg$ACCDMG, col = "steelblue", main = "Extreme Accident Damage without Jitter")
par(mfrow = c(1,1))


# Conditioning on categorical variables

# on Cause
xyplot(ACCDMG~TRNSPD | Cause, main = "Extreme Damage vs. Train Speed Conditioned on Cause", xlab = "Train Speed", ylab = "Total Accident Damage", data = xdmg)

# on type of accident



# Repeat the above extreme point analysis but use TOTINJ + TOTKLD
# But wait until we do more cleaning

#***********************************************************
#
#			Heatmaps for categorical variabels
#
#***********************************************************

table(xdmg$Cause, xdmg$TYPE)

heatmap(table(xdmg$Cause, xdmg$TYPE), Rowv = NA, Colv = NA)

# With legend (optional)


install.packages("gplots", dependencies = T)

library(gplots)

heatmap.2(table(xdmg$Cause, xdmg$TYPE), Rowv = F, Colv = F)

source("http://www.phaget4.org/R/myImagePlot.R")

myImagePlot(table(xdmg$Cause, xdmg$TYPE), title = "No. of Accidents by Cause and Type of Accident")

	
#***********************************************************
#
#			Data Cleaning
#
#***********************************************************

# Let's look at again at extreme cost accidents. Is there another one interest?

# Look at the second most costly accident

which(xdmg$ACCDMG > 15e6)



# Duplicates?

# The max

which(xdmg$ACCDMG == max(xdmg$ACCDMG))

# Look at the narrative

as.matrix(names(xdmg))

xdmg[which(xdmg$ACCDMG == max(xdmg$ACCDMG)), 123:137]

# Are there other duplicates?

duplicated(xdmg[, c("YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])


# What about longitude and latitude?

# what about incident number?

which(xdmg$INCDTNO == "1")

xdmg[which(xdmg$INCDTNO == "1"),  c("YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")]



duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])

# Not duplicated

!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")]))


#remove the duplicates


dim(xdmgnd)

# number of duplcates

nrow(xdmg) - nrow(xdmgnd)

