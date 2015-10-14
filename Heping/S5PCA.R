

#******************************************************
#
#					Session 5
#				
#				Principal Components
#
#******************************************************


#***********************************************************
#
#			Get the data
#
#***********************************************************

# Source AccidentInput

source("AccidentInput.R")

# you should have two data structures in working memory
# First - a list of data frames for each year of accident data

acts <- file.inputl(path)

# Next a data frame with all accidents from all years from 2001 - 2014
# with columns that are consistent for all of these years

# Get a common set the variables
	
	comvar <- intersect(colnames(acts[[1]]), colnames(acts[[8]]))

# the combined data frame

	totacts <- combine.data(acts, comvar)

	dim(totacts)


# Get the extreme accident data

# For ACCDMG
dmgbox <-boxplot(totacts$ACCDMG)

xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]
sdmg <- totacts[totacts$ACCDMG < dmgbox$stats[2],]

# For Casualties (TOTINJ + TOTKLD)

xdmg$Casualties <- xdmg$TOTINJ + xdmg$TOTKLD

# Remove 9/11

xdmg$YEAR[which(xdmg$TOTKLD == max(xdmg$TOTKLD))]

# Remove duplicates
xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]
sdmgnd <- sdmg[!(duplicated(sdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]



#***********************************************************
#
#		Principal Components with the Correlation Matrix	
#
#***********************************************************

	
# Principal Components with the Correlation Matrix for extreme data 2 (metrics)

xdmgnd.pca <- princomp(xdmgnd[,c("CARSDMG","EQPDMG", "TRKDMG","ACCDMG", "TOTKLD", "TOTINJ")], cor = T)
	
# View data in the first 2 PC

biplot(xdmgnd.pca)


# Remove outliers in component 2
xdmgnd.pca <- princomp(xdmgnd[-c(4231, 30016, 18969),c("CARSDMG","EQPDMG", "TRKDMG","ACCDMG", "TOTKLD", "TOTINJ")], cor = T)

# View the first 2 PC without ouliers

biplot(xdmgnd.pca)
	
# Variance plot
	
screeplot(xdmgnd.pca, main = "Variance for PC of Metrics")
	
# Loadings
	
barplot(xdmgnd.pca$loadings[,1])
barplot(xdmgnd.pca$loadings[,2])
	
# Cumulative variance

source("PCAplots.R")
	
cumplot(xdmgnd.pca, col = "blue")

#***********************************************************
#
#		Possible predictors of damage	
#
#***********************************************************

# SPM
source("SPM_Panel.R")
uva.pairs(xdmgnd[,c("ACCDMG", "TRNSPD", "TONS", "CARS", "TIMEHR", "TEMP")])

# PCA

pred.pca <- princomp(xdmgnd[,c("ACCDMG", "TRNSPD", "TONS", "CARS", "TIMEHR", "TEMP")], cor = T )

biplot(pred.pca)




	