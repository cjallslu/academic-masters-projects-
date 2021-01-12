#Submitted by:

#Chaitanya Muppala 662043907
#CJ All            650065604
#Manisha Singh     674146073


install.packages('ISLR')
library(ISLR)
View(Auto)

# Problem 4: Linear Regression
getwd()
setwd('/Users/cjall/Desktop/Grad School/IDS 575 Machine Learning and Statistical Methods for Business Analytics/Assignments/Assignment 1')

# (a)
summary(Auto)
colnames(Auto) 

# 1) There are 5 training examples in the Auto dataset, and 8 features excluding the 'Name' attribute
# 2) The data is a skinny, tall matrix

# (b)
library(ggplot2)
library(dplyr)
install.packages('corrplot')
library(corrplot)
install.packages('coefplot')
library(coefplot)
library(car)

Auto = select(Auto, mpg, cylinders, displacement, horsepower, weight, acceleration, year, origin)
head(Auto, n = 10)
plot(Auto)
correlations = cor(Auto)
corrplot(correlations)

# (c)
lmMpg <- lm(mpg ~ cylinders + displacement + horsepower + weight + acceleration + year + origin, data = Auto)
summary(lmMpg)

par(mfrow=c(2,2))
plot(lmMpg)



# (d)
coefplot(lmMpg)
crPlots(lmMpg)

# (e)
# origin should be categorical and thus wasn't included
# creating new columns for each log-based feature 
Auto$Logcyl = log(Auto$cylinders)
Auto$Logdis = log(Auto$displacement)
Auto$Loghorse = log(Auto$horsepower)
Auto$Logweight = log(Auto$weight)
Auto$Logacc = log(Auto$acceleration)
Auto$Logyear = log(Auto$year)
View(Auto)

# re-running linear regression for our log based columns
LoglmMpg <- lm(mpg ~ Logcyl + Logdis + Loghorse + Logweight + Logacc + Logyear, data = Auto)
summary(LoglmMpg)
plot(LoglmMpg)

# creating new columns for each feature squared
Auto$Sqrcyl = sqrt(Auto$cylinders)
Auto$Sqrdis = sqrt(Auto$displacement)
Auto$Sqrhorse = sqrt(Auto$horsepower)
Auto$Sqrweight = sqrt(Auto$weight)
Auto$Sqracc = sqrt(Auto$acceleration)
Auto$Sqryear = sqrt(Auto$year)
View(Auto)

SquarelmMpg <- lm(mpg ~ Sqrcyl + Sqrdis + Sqrhorse + Sqrweight + Sqracc + Sqryear, data = Auto)
summary(SquarelmMpg)





