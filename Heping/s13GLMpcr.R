

#************************************************************
#
#				SPAM Filter  
#			GLM Principal Components Regression 
#
#
#************************************************************




#*****************************
#
# Load the data & source files
#
#*****************************

spam <- read.table("Spam.txt", header = F)


# Source PCAplot.R

source("PCAplots.R")
source("pc.glm.R")



#*****************************
#
# GLM with Interactions
#
#*****************************

# Compare a main effects model with all variables to this same model that also includes
# the interaction terms between V5, V6, V7. 
# Do the comparison with a partial likelihood test.
# (note: do not do a complete interacton model! Unless you have time.)
# Which model do we choose?


spam.glm2 <- glm(V58~. + (V5+V6+V7)^2, data = spam, family = binomial)


# Now compare a main effects model with log transformed predictors 
# to this same model that also includes
# the interaction terms between the log transformed variables V5, V6, V7. 
# Use an offset of 0.1
# Do this comparison with a partial likelihood test.
# (note: Again do not do a complete interacton model! Unless you have time.) 
# Which model do you choose?




#**************************************************
#
# 		Evaluate performance with AIC
#
#**************************************************

# Compare the AIC for the 4 models you developed.
# Which model would you choose?

AIC(spam.glm)



# Compare the BIC for the 4 models you developed.
# Which model would you choose?

BIC(spam.glm)




#***********************************************************
#
#  Principal Components Regression
#
#***********************************************************

# obtain the principal components for the predictors with a correlation matrix

spam.pca <- princomp(spam[,-58], cor = T)

# Scree plot

screeplot(spam.pca)

# to see how many components are needed for 90% of the variance we use

var.comp(spam.pca, 90)

#How many components do we need for 98%?


# Use pc.glm() to get the principal component regression results.

spampca.glm98 <- pc.glm(spam.pca, 98, spam[,58])

# Do a model utility test starting with pc.null()

spampc.null <- pc.null(spam.pca, 98, spam[,58])

anova(spampc.null, spampca.glm98, test = "Chi")


# Do a model utility test for model that uses 
# PC that account for 90% of the variance



# Do a model utility test for model that uses 
# PC that account for 50% of the variance




# Do a partial likelihood test between the 
# 98 % and 50% models



# Compare the AIC for your PC model for 98% 
# of the variance to one that with 
# components that account for 90%


#to one that with 
# components that account for 50%



# Do the comparisons with BIC


# Use AIC to choose among all models.
# Use BIC to choose among all models.


