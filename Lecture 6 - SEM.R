# Lecture 7 - SEM
install.packages("lavaan")
library(tidyverse)
library(lavaan)
library(semPlot)

# We use weight_behavior data from the mma package
weight_behavior <- 
    read_csv("datasets/weight_behavior.csv") %>% 
    mutate(screenhours = tvhours + cmpthours + cellhours,
           sports = if_else(sports == 2L, 0L, sports))

# Variable Name	Description
# bmi	Body mass index
# age	Age at survey
# sex	Male or female
# race	African American, Caucasian, Indian, Mixed or Other
# numpeople	Number of people in family
# car	Number of cars in family
# gotosch	Four levels of methods to go to school
# snack	Eat a snack in a day or not
# tvhours	Number of hours watching TV per week
# cmpthours	Number of hours using computer per week
# cellhours	Number of hours playing with cell phones per week
# sports	In a sport team or not
# exercise	Number of hours of exercises per week
# sweat	Number of hours of sweat-producing activities per week
# overweigh	The child is overweight or not

glm(overweigh ~ sports + screenhours, data = weight_behavior, family = "binomial") %>% summary()
lm(bmi ~ sports + screenhours + gotosch + age , data = weight_behavior) %>% summary()

sports_model <- " 
        # direct effect
             bmi ~ c*screenhours
        # mediator
            sports ~ a*screenhours
            bmi ~ b*sports
        # indirect effect (a*b)
            ab := a*b
        # total effect
            total := c + (a*b)
        "

# Check if sports group membership mediates the relationship between screen time and bmi.
fit_sports <- sem(bmi_model, data = weight_behavior)
summary(fit_sports, fit.measures = TRUE)
semPaths(fit_sports, "std", nCharNodes = 6, edge.label.cex = 1.5, sizeMan = 10, edge.color = "black", shapeMan = "rectangle", rotation = 2)

exercise_model <- " 
        # direct effect
             bmi ~ c*screenhours
        # mediator
            exercises ~ a*screenhours
            bmi ~ b*exercises
        # indirect effect (a*b)
            ab := a*b
        # total effect
            total := c + (a*b)
        "

# Sports group membership partially mediates the effect of screen time and bmi
# Is this just because of sports group memberhip, or exercise in general
fit_exercise <- sem(exercise_model, data = weight_behavior)
summary(fit_exercise, fit.measures = TRUE)
semPaths(fit_exercise, "std", nCharNodes = 6, edge.label.cex = 1.5, sizeMan = 10, edge.color = "black", shapeMan = "rectangle", rotation = 2)


# SEM
PoliticalDemocracy %>% as_tibble()

pd_model <- '
  # measurement model
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
'


# Fit model
pd_fit <- sem(pd_model, data = PoliticalDemocracy)

# Check the model graphically
semPaths(pd_fit, rotation = 4)

summary(pd_fit, standardized = TRUE, fit.measures = TRUE)
semPaths(pd_fit, "std", nCharNodes = 6, edge.label.cex = 1.5, sizeMan = 10, edge.color = "black", shapeMan = "rectangle", rotation = 2)

# Modification indices
summary(pd_fit, modindices = TRUE)

modificationindices(pd_fit, sort = TRUE)
