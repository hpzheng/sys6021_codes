



#******************************************************
#
#				Additional Plots for
#				Principal Components
#
#******************************************************


	loadingsplot <- function(pca.obj, j=1,  k = 2, ...)  
	{
		par(mfrow = c(k-j+1,1))
		if(nrow(pca.obj$loadings) > 8){cn <- 8/nrow(pca.obj$loadings)} else {cn <- 1}
		for(i in j:k)
		{barplot(pca.obj$loadings[,i], main = paste("Component ", i), cex.names = cn , ...)}
		par(mfrow = c(1,1))
		
		}
	
	cumplot <- function(pca.obj, ...)
	{
		xc <- cumsum(pca.obj$sdev^2)/sum(pca.obj$sdev^2)
		barplot(xc, ylim = c(0,1), main = "Proportion of Variance", ylab = "Proportion", names.arg = 1:length(pca.obj$sdev), xlab = "Components", ...)
		
		}

DF0 <- 'A'	
DF1 <- rbind(length(grep('A', xdmgnd$METHOD)))
DF2 <- rbind(length(grep('A', sdmgnd$METHOD)))
DF3 <- rbind(100*length(grep('A', xdmgnd$METHOD))/dim(xdmgnd)[1])
DF4 <- rbind(100*length(grep('A', sdmgnd$METHOD))/dim(sdmgnd)[1])
DF5 <- NULL

for(i in c('B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P'))
{
  DF0 <- rbind(DF0, i)
  DF1 <- rbind(DF1, length(grep(i, xdmgnd$METHOD)))
  DF2 <- rbind(DF2, length(grep(i, sdmgnd$METHOD)))
  DF3 <- rbind(DF3, 100*length(grep(i, xdmgnd$METHOD))/dim(xdmgnd)[1])
  DF4 <- rbind(DF4, 100*length(grep(i, sdmgnd$METHOD))/dim(sdmgnd)[1])
  
}

DF5$method <- DF0
DF5$nxdmg <- DF1
DF5$nsdmg <- DF2
DF5$pxdmg <- DF3
DF5$psdmg <- DF4
DF5$ratio <- DF5$psdmg / DF5$pxdmg
ratbox <- boxplot(DF5$ratio)

TRKCLS <- c('X','1','2','3','4','5','6','7','8','9')
SPDFREIGHT <- c(10, 10, 25, 40, 60, 80, 110, 125, 160, 200)
SPDPASSENGER <- c(0, 15, 30, 60, 80, 90, 110, 125, 160, 200)
eqpspd.data <- data.frame(TRKCLS, SPDFREIGHT, SPDPASSENGER)


TRKCLAS <- c('X','1','2','3','4','5','6','7','8','9')
SPDFREIGHT <- c(10, 10, 25, 40, 60, 80, 110, 125, 160, 200)
SPDPASSENGER <- c(0, 15, 30, 60, 80, 90, 110, 125, 160, 200)
eqpspd.data <- data.frame(TRKCLAS, SPDFREIGHT, SPDPASSENGER)

xdmgnd <- merge(xdmgnd, eqpspd.data, by="TRKCLAS", all.x=TRUE)

xdmgnd$Cause <- rep(NA, nrow(xdmgnd))
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "M")] <- "M"
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "T")] <- "T"
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "S")] <- "S"
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "H")] <- "H"
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "E")] <- "E"
xdmgnd$Cause <- factor(xdmgnd$Cause)
xdmgnd$Type <- factor(xdmgnd$TYPE, labels = c("Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative"))

xdmgnd$SPDDIFF <- xdmgnd$TRNSPD-xdmgnd$SPDFREIGHT

# xyplot --> numeric vs. numeric
xyplot(ACCDMG~SPDDIFF, data = xdmgnd, type = c("p", "r"))
xyplot(ACCDMG~SPDDIFF | TRKCLAS, data = xdmgnd, type = c("p", "r"))
xyplot(ACCDMG~SPDDIFF | Cause, data = xdmgnd, type = c("p", "r"))
xyplot(ACCDMG~SPDDIFF | Type, data = xdmgnd, type = c("p", "r"))

# bwplot --> categorical vs. numeric
library(lattice)
bwplot(TYPEQ~TRNSPD, data=xdmgnd)

# heatmap --> categorical vs. categorical
source("http://www.phaget4.org/R/myImagePlot.R")
myImagePlot(table(xdmgnd$TRKCLAS, xdmgnd$TYPEQ), title = "whatever")