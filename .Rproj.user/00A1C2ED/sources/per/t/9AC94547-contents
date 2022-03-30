##################################################################
# The purpose of this R script is to demonstrate a logistic regression
# statistical analysis. This was made mostly for my own reference and a way to 
# practice these functions so it is far from perfect or comprehensive,  
# but I will share it for others who may find it helpful.
#
# Script by: Kim Fake
#
#
###################################################################

# Load libraries
library(dplyr) # pretty much always load for filtering and manipulating data
library(lubridate) #aids in manipulating data containing dates


#load data
data <- read.csv (
  './Biology 101 Data CLEAN.csv', 
  stringsAsFactors = FALSE, 
  fileEncoding = 'UTF-8-BOM'
)

# as you can see, this data is on 3 biology classes
# we want to use a linear regression to determine if Age
# predicts whether students study the recommended 10+ hrs per week

#first we must calculate the age of students based on their birthdates provided
# change a data type to date
data$B_Day <- as.Date(data$B_Day, 
                      format= "%m/%d/%Y"
)

# create a year of birth column
data <- data %>%
  dplyr::mutate(Year = lubridate::year(B_Day))

# make a new column with age made based on birth year
data <- data %>%
  mutate(Age = 2022-Year)

# We must set the binary variable (Studied_10 
# = did student study => 10hrs per wk)
# to a numeric vector of 1's and 0's
data$Studied_10[data$Studied_10 == "Y"] <- 1
data$Studied_10[data$Studied_10 == "N"] <- 0
data$Studied_10<- as.numeric(data$Studied_10)

# In regression, it is often recommended to center and scale continuous variables 
# so that the predictors have a mean of 0 and a standard deviation of 1
# If you are testing an interaction between a continuous variable and another 
# variable (continuous or categorical) the continuous variable(s) should be 
# centered to avoid multicollinearity issues, which could affect model 
# convergence and/or inflate the standard errors.
data$Age <- scale(data$Age)


# logistic regression model
# examining the relationship between Age 
# and studying the recommended 10+ hrs per week
model <- glm(
  Studied_10 ~ Age,
  data=data
)
summary(model)

# the coefficient in the model for Age attended is positive 
# and significant. This indicates that older students are
# students are more likely to study the recommended 10+ hours per wk

# However, we hypothesize Sex might have an effect 
# on the study habitats of students
# and want to include it in our model as a control

# first we need to set sex to a factor data type
data$Sex <- as.factor(data$Sex)

# logistic regression model
# examining the relationship between Age 
# and studying the recommended 10+ hrs per week
# with Sex as a control
model <- glm(
  Studied_10 ~ Age + Sex,
  data=data
)
summary(model)

# the coefficient in the model for Age attended is still positive 
# and significant. This indicates that older students are
# students are more likely to study the recommended 10+ hours per wk
# the coefficient in the model for sex is not significant
# so male and female students did not differ
# in their adherence to studying the recommended 10+ hours per wk

# the previous model, however does not consider the non-independence of students
# sampled from different biology classes, and as study habits may vary between classes due
# to other factors such as instructors, we will class as a random effect in our model
# for this we will need a mixed-effects logistic regression model and so
# we must load a package with a function that allows for random effects in the model

#load package 
library(lme4)

# first we need to set class to a factor data type
data$Class <- as.factor(data$Class)

# Fit a mixed-effects logistic regression model 
model <- glmer(
  Studied_10 ~ Age + Sex + (1|Class),
  data=data,
  family = binomial
)
summary(model)

# although this package does not return with an indication of significance
# using p-values, we can see if the 95% CI of the coefficient over laps with 
# zero to determine if it is significant
# 0.3513-0.02723 to 0.3513+0.02723 does not overlap with zero
# Therefore there is a significantly positive relationship between
# the Age and studying the recommended 10+ hrs per week