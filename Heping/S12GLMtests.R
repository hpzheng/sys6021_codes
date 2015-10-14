

#************************************************************
#
#				SPAM Filter 2 
#			Generalized Lnear Models of SPAM 
#
#
#************************************************************


#*****************************
#
# Load the data & source files
#
#*****************************


spam <- read.table("/Users/dust/Courses/sys4021/data/spam/Spam.txt", header = F)

summary(spam)

#*****************************
#
# GLM
#
#*****************************

# Use the glm() function to obtain the glm for the spam filter.

spam.glm <- glm(V58~., data = spam, family = binomial)

# notice the warning message!

# Use summary to evaluate the model

summary(spam.glm)

# Can we use an F test for model utility?

#Perform a model utility test for the glm

# Model Utility Test using Chi2 statistic

# What do you conclude?

spam.null <- glm(V58~1, data = spam, family = binomial)
spam.null <- glm(V58~0, data = spam, family = binomial)

anova(spam.null, spam.glm, test = "Chi")


# Create a model with just the capital letters, 
# V55 - V58  as predictors

spam.glm <- glm(V58~., data = spam[,], family = binomial)


# How does this model do on the model utility test?




# Use the likelihood ratio or partial chi square test to compare the full main effects model with the capital letters model


# Can we use the t-test for individual coefficients?
# Use the full main effect model and test the coefficient on 
# V57

spam.no57 <- update(spam.glm, .~.-V57, data = spam)

anova(spam.no57, spam.glm, test = "Chi")

# What is the contribution of variable 51 to the model?
# notice that we add one to the variable number (51)
# to account for the intercept


(exp(spam.glm$coefficients[52]) -1)*100

# Explain this result.

spam[1,]
predict(spam.glm, newdata = spam[1,])
exp(predict(spam.glm, newdata = spam[1,]))
exp(predict(spam.glm, newdata = data.frame(spam[1,c(1:50, 52:57)], V51 = 1)))




# Repeat this for 57

# Variable 57 (total number of capital letters)
# notice that we add one to the variable number (57)
# to account for the intercept

(exp(spam.glm$coefficients[58]) -1)*100


# compare the drop1 chi square test to the approximate Gaussian test in summary.

library(MASS)

drop1(spam.glm, response~., test = "Chi", data = dspam)

#  Compare the step model with capital letter predictors to the capital letter model

step.cap <- step(spam.cap, data = spam, family = binomial)


# Run stepwise for the complete model
# Do this at home when  you have time.
# Compare a stepwise model that starts with the full main effects model to the main effects model.

# **********************************************************
# Repeat the above analysis with log transformed predictors
#***********************************************************
