

#********************************************************************************
#
#					Univariate Graphics
#
#********************************************************************************


#************************************
# Reading in data
#************************************

# Set working directory

# For example, an Mac user

setwd("/Users/dust/Courses/sys4021/data/TrainAccidents")

# or a Windows user

setwd("C:/me/data/TrainAccidents")


#***********************************************

# Read in the accident files one at at time
# for the first questions in the in-class assignment we will 
# only use data from 2014

acts14 <- read.table("RailAccidents14.txt", sep = ",", header = T)

# Since the files are really csv you can use

acts14 <- read.csv("RailAccidents14.txt")


#**************************************************

# To get a summary of all of the variables use

summary(acts14)

# To get a summary of a subset of the variables (e.g., "ACCDMG", "TOTKLD", "CARS" ) 
# you can use

summary(acts14[,c("ACCDMG", "TOTKLD", "CARS")])


# To get individual statistics (e.g. mean, var) you can use

mean(acts14$TOTKLD)

var(acts14$TOTKLD)

# You can round your answer using the round() function

round(mean(acts14$TOTKLD))

#**************************************************

# You will need to read in all 13 years of the data 
# You will put the data into a data structure called a list

# To do this you will use code I have written, AccidentInput.R 
# Put that file in your working directory and then source it:

source("AccidentInput.R")

# Now use it to read in all the data. You must have ALL and ONLY the rail accident data
# files in one directory. Call that directory and its path, path.
# You can give your data a name
# In my examples below I use acts as the name for data sets
# Then use the command

acts <- file.inputl(path) 

# path is the specification of the path to your file.

# Now acts[[1]] is the data set from year 2001, 
# acts[[2]] is the data set from year 2002, etc.

# Before we put all the data into one data frame
# we must clean the data

##################################################
#
#	Data Cleaning
#
##################################################

# Notice that the number of columns changes from year to year

ncol(acts[[1]])
ncol(acts[[8]])


# Get a common set the variables
comvar <- intersect(colnames(acts[[1]]), colnames(acts[[8]]))

	
# Now combine the data frames for all 14 years
# Use combine.data()

totacts <- combine.data(acts, comvar)

dim(totacts)



#***********************************
#
# 	histograms of ACCDMG
#
#***********************************

# These examples are for 2011 

hist(acts[[11]]$ACCDMG) # for 2011

hist(acts[[11]]$ACCDMG, main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")

# Different bin widths

hist(acts[[11]]$ACCDMG, breaks = "scott", main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")

hist(acts[[11]]$ACCDMG, breaks = "fd", main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")

hist(acts[[11]]$ACCDMG, breaks = 20, main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")

hist(acts[[11]]$ACCDMG, breaks = 100, main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")


# other years


par(mfrow = c(2,2))
hist(acts[[1]]$ACCDMG, main = "Total Accident Damage in 2001", xlab = "Dollars ($)", col = "steelblue")
hist(acts[[4]]$ACCDMG, main = "Total Accident Damage in 2004", xlab = "Dollars ($)", col = "steelblue")
hist(acts[[8]]$ACCDMG, main = "Total Accident Damage in 2008", xlab = "Dollars ($)", col = "steelblue")
hist(acts[[11]]$ACCDMG, main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")
par(mfrow = c(1,1))


par(mfrow = c(4,3))
for(i in 1:11)
{
	j <- which(colnames(acts[[11]]) == "ACCDMG")
	if(i < 10)
	{
	hist(acts[[i]][,j], main = paste("Total Accident Damage in 200", i, sep = "") , xlab = "Dollars ($)", col = "steelblue")	
	}
	else
	{
	hist(acts[[i]][,j], main = paste("Total Accident Damage in 20", i, sep = "") , xlab = "Dollars ($)", col = "steelblue")	
	}
	
}
par(mfrow = c(1,1))


# Damage in constant scales

par(mfrow = c(4,3))
for(i in 1:11)
{
	j <- which(colnames(acts[[11]]) == "ACCDMG")
	if(i < 10)
	{
	hist(acts[[i]][,j], main = paste("Total Accident Damage in 200", i, sep = "") , xlab = "Dollars ($)", col = "steelblue", xlim = c(0,1.7e7), ylim = c(0,4000))	
	}
	else
	hist(acts[[i]][,j], main = paste("Total Accident Damage in 20", i, sep = "") , xlab = "Dollars ($)", col = "steelblue", xlim = c(0,1.7e7), ylim = c(0, 4000))	
	}
	
}
par(mfrow = c(1,1))

#*********************************************************************
#
# 				Boxplots of ACCDMG
#
#*********************************************************************

boxplot(acts[[11]]$ACCDMG)

boxplot(acts[[11]]$ACCDMG, main = "Total Accident Damage in 2011", ylab = "Dollars ($)", col = "steelblue", pch = "*")


boxplot(acts[[1]]$ACCDMG, main = "Total Accident Damage in 2001", ylab = "Dollars ($)", col = "steelblue", pch = 4)


boxplot(acts[[6]]$ACCDMG, main = "Total Accident Damage in 2006", ylab = "Dollars ($)", col = "steelblue")


par(mfrow = c(2,2))
boxplot(acts[[1]]$ACCDMG, main = "Total Accident Damage in 2001", ylab = "Dollars ($)", col = "steelblue")
boxplot(acts[[4]]$ACCDMG, main = "Total Accident Damage in 2004", ylab = "Dollars ($)", col = "steelblue")
boxplot(acts[[8]]$ACCDMG, main = "Total Accident Damage in 2008", ylab = "Dollars ($)", col = "steelblue")
boxplot(acts[[11]]$ACCDMG, main = "Total Accident Damage in 2011", ylab = "Dollars ($)", col = "steelblue")
par(mfrow = c(1,1))


par(mfrow = c(2,2))
boxplot(acts[[1]]$ACCDMG, main = "Total Accident Damage in 2001", ylab = "Dollars ($)", col = "steelblue", ylim = c(0,1.7e7))
boxplot(acts[[4]]$ACCDMG, main = "Total Accident Damage in 2004", ylab = "Dollars ($)", col = "steelblue", ylim = c(0,1.7e7))
boxplot(acts[[8]]$ACCDMG, main = "Total Accident Damage in 2008", ylab = "Dollars ($)", col = "steelblue", ylim = c(0,1.7e7))
boxplot(acts[[11]]$ACCDMG, main = "Total Accident Damage in 2011", ylab = "Dollars ($)", col = "steelblue", ylim = c(0,1.7e7))
par(mfrow = c(1,1))


par(mfrow = c(4,3))
for(i in 1:11)
{
	j <- which(colnames(acts[[11]]) == "ACCDMG")
	if(i < 10)
	{
	boxplot(acts[[i]][,j], main = paste("Total Accident Damage in 200", i, sep = "") , ylab = "Dollars ($)", col = "steelblue")	
	}
	else
	{
	boxplot(acts[[i]][,j], main = paste("Total Accident Damage in 20", i, sep = "") , ylab = "Dollars ($)", col = "steelblue")	
	}
	
}
par(mfrow = c(1,1))



par(mfrow = c(4,3))
for(i in 1:11)
{
	j <- which(colnames(acts[[11]]) == "ACCDMG")
	if(i < 10)
	{
	boxplot(acts[[i]][,j], main = paste("Total Accident Damage in 200", i, sep = "") , ylab = "Dollars ($)", col = "steelblue", ylim = c(0,1.7e7), ylim = c(0,4000))	
	}
	else
	boxplot(acts[[i]][,j], main = paste("Total Accident Damage in 20", i, sep = "") , ylab = "Dollars ($)", col = "steelblue", ylim = c(0,1.7e7), ylim = c(0, 4000))	
	}
	
par(mfrow = c(1,1))



#*****************************
#
#		QQ Plots
#
#*****************************


	qqnorm(acts[[11]]$ACCDMG, main = "Total Accident Damage")
	qqline(acts[[11]]$ACCDMG)

	qqnorm(acts[[11]]$TEMP, main = "Accident Temperature")
	qqline(acts[[11]]$TEMP)


