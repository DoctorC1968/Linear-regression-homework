#  Introduction

#   Learning objectives:
##     Learn the R formula interface
##     Specify factor contrasts to test specific hypotheses
##     Perform model comparisons
##     Run and interpret variety of regression models in R

## Set working directory

setwd("C:/Users/Owner/Documents")

getwd() # where am I?
list.files("dataSets") # files in the dataSets folder

## Exercise 1: least squares regression

##   Use the states.rds data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
##   2. Print and interpret the model `summary'
##   3. `plot' the model to look for deviations from modeling assumptions

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with metro as the only predictor?
states.data <- readRDS("dataSets/states.rds") 
str(states.data)
head(states.data)

states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
states.info

# summary of energy and metro columns, all rows
sts.energy.metro <- subset(states.data, select = c("energy", "metro"))
str(sts.energy.metro)
summary(sts.energy.metro)
# correlation between energy and metro
cor(sts.energy.metro,use="complete.obs")

## Plot the data before fitting models

##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

# scatter plot of energy vs metro
plot(sts.energy.metro)
#No clear relationship between "energy" and "metro"
# Fit our regression model
energy.mod <- lm(energy ~ metro, data=states.data)
# Summarize and print the results
summary(energy.mod) # show regression coefficients table
#Adjusted R^2 is only 0.097. Not a good model at all. 

hist(residuals(energy.mod))
#Residuals are skewed to the right. It's not likely that they're
#normally distributed. 
plot(energy.mod)
#The residuals have a curved pattern. So they don't have an equal "scatter"
#about the horizontal axis. So they don't satisfy the assumption of 
#homoscedasticity. 
#The points on the quantile plot do not lie along a straight line, so we
#do not have evidence from the qq plot either that the residuals are 
#normally distributed. 
sts.more.vars <- subset(states.data, select = c("energy", "metro","waste",
                              "toxic","green","income"))
str(sts.more.vars)
summary(sts.energy.metro)
# correlation between energy and independent variables
cor(sts.more.vars,use="complete.obs")
#Only "toxic" and "green" seem to have a strong correlation with "energy"

#Make scatterplots of energy vs. each independent variable:
plot(sts.more.vars$waste,sts.more.vars$energy)
plot(sts.more.vars$toxic,sts.more.vars$energy)
plot(sts.more.vars$green,sts.more.vars$energy)
plot(sts.more.vars$income,sts.more.vars$energy)
#The scatterplots confirm the conclusion that we obtained from the
#correlation matrix.

# Fit our new regression model
energy.mod2 <- lm(energy ~ metro+waste+toxic+green+income, data=states.data)
summary(energy.mod2)
#Only the predictors "toxic" and "green" are significant. So we will keep
#"toxic" and "green" in the model. In addition, we will keep "metro" so that 
#we can use the F-test to compare the old model with the new model (The 
#F-test can only be used if the predictors of one model are a subset of the
#predictors of the other model,i.e. the models are nested)
energy.mod2 <- lm(energy ~ metro+toxic+green, data=states.data)
summary(energy.mod2)
#This time the adjusted R^2 is 0.7483. By that measure, the new model
#is much better than the old one. 

hist(residuals(energy.mod2))
#This histogram is more bell-shaped, suggesting that this time the 
#residuals are normally distributed.
plot(energy.mod2)
#The residuals seem to be more equally scattered in this model, though there
#are still a few extreme residuals 
#The points in the QQ plot are closer to a straight line than they were
#in the last model, though a few outliers are still evident. 
#Now compare the two models using ANOVA: 

energy.mod <- update(energy.mod, data=na.omit(states.data))
anova(energy.mod,energy.mod2)
#p-value is 1.46e-13, or practically 0. By this measure, the new model is
#much better than the old one; the reduction in the residual sum of squares
#that comes about from adding the additional predictors is statistically
#significant at any reasonable value of alpha. 

#Next we use the Akaike information critera and Bayes information 
#criteria to compare the models
AIC(energy.mod)#593
AIC(energy.mod2)#533
#AIC(energy.mod2)<AIC(energy.mod), so energy.mod2 is better by this metric.

BIC(energy.mod)#599
BIC(energy.mod2)#542
#BIC(energy.mod2)<BIC(energy.mod), so energy.mod2 is better by this metric.
##########################################################################

## Interactions and factors

## Modeling interactions

##   Interactions allow us assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.

## Exercise 2: interactions and factors

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

energy.mod3<-lm(energy~metro+toxic*green, data=states.data)
summary(energy.mod3)
#Adjusted R^2=0.7712, which beats the adjusted R^2 of 0.7483 from the model
#without the interaction. 
coef(summary(energy.mod3))
#Only "green" and the interaction "toxic:green" are significant

#Now put region into the model
states.data$region <- factor(states.data$region)
energy.mod4<-lm(energy~metro+toxic*green+region, data=states.data)
summary(energy.mod4)

#Adjusted R-squared=0.7932, which beats the adjusted R-squared of 
#0.7712 from energy.mod3
contrasts(states.data$region)
#Base level is "West". 
#Here is part of the summary of energy.mod4, which we will use to determine
#whether there are significant diffences across region:
#Coefficients:
#               Estimate    Std. Error t value Pr(>|t|) 
#------------------------------------------------------
#regionN. East -29.58832    26.19792  -1.129   0.26545    
#regionSouth    36.46042    21.25086   1.716   0.09395 .  
#regionMidwest  18.38341    22.90718   0.803   0.42700

#Each of the p-values of 0.26545, 0.09395, and 0.42700 are well above
#0.05, which is the largest reasonable threshold for statistical significance.
#In other words, there is no visible evidence that the regions Northeast,
#South or Midwest differ signifiantly from the base region "West". 
#So there is not adequate evidence of significant differences across the 
#four regions. 