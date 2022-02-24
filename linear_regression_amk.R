# 02-24-2022
# linear regression 

# this is not a statistics class, more models will be 

### NOTES
#today focusing on beak depth~length
# y=mx+b
# x is length
# computer is caluculating the slope which is our parameters 
# E = y^hat -y 
# predicted y minus actual y is the error

# a linear model has to be linear in the paremters (slopes) not in the the x and y 

# basic assumptions for a linear model 

# linear relationship- 
# just because the linear relationship is not there with the data
# you can transform the vairable
# like take the sqrt(X)- then make a better fit 
# 

# normality of the model residuals 
# residuals are err 
# a good model- error would have a normal distribution 

# no or little multicolinearity 
# bill depth and mass are correlated then model will be confused 
# towards how much weight to put towards two different variables 
# one variable can take the significance away from body mass. 
# mechanistic claims - can leave in colinear variables - lots of philosophy 

# no auto-correlation 
# samples are independent 
# so paired data - is not independent 
# oops about my dead birds
# variation - in space and time are closely realted 
# treat those very carefully 


# homoscedasticity- assuming homoscedasity 
# assuming the variance is the same across independent  variables
# don't want heteroscedasticity
# across range of x the y variance gets wider like a cone shape 

#regression diagnostics - reclcone the repo to get the less typo filled 
# lots of tests that you can do to double check the stats behind your linear model. 

library(tidyverse)
library(palmerpenguins)
library(GGally)

#simple linear  regression
penguins %>%
  select(bill_depth_mm, bill_length_mm)%>%
  ggpairs()

#if you see structure in your data that your model doesn't acocunt for 
# thats no good, need to adresss

lm1 <- lm(bill_depth_mm ~ bill_length_mm, data=penguins)
# theres alot more info in the lm model that the print commmand doesn't give us

class(lm1) # lm is its own class 
summary(lm1)

# f statistic is the model precitive power
# Rsqaured - is the correlation squarred and then penalized for how complex the model is
# an R squared of 0.05 is trash 


### cheater plot of this model 
ggplot(data=penguins, aes(x=bill_length_mm, y=bill_depth_mm))+
  geom_point()+
  geom_smooth(method="lm") # this is so cheating 
# this stinks this model is not capturing a lot of data in bill length 

class(lm1)
plot(lm1) #plot.lm- plot() is a wrapper fxn, and it uses plot.lm when passed a linear model 

#gives a few different plots for diagnostics 
#

#

#

#residuals vs leverage 
# a good fit would be close to 0 
# the leat favorite points are flagged by are and they are way up 



#now we are going to try this model with one species 
lm2<- lm(bill_depth_mm ~ bill_length_mm, data = gentoo)
summary(lm2)
# slope and intercept are still significant - so maybe not the best tinterpretaion of model fittness
# the R^2 is 0.4 which is much better 

gentoo %>% 
  select(bill_depth_mm, bill_length_mm) %>%
  GGally::ggpairs() 


ggplot(data=gentoo, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point() +
  geom_smooth(method = "lm")

# check all bill models
ggplot(data=penguins)+
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm, color = species))+
  geom_smooth(method="lm", aes(x=bill_length_mm, y=bill_depth_mm, color = species))+
  geom_smooth(method = "lm", aes(x=bill_length_mm, y=bill_depth_mm), color = "black")

# sweet figure is shows off the simpsons paradox 
# a assumption we did not talk about 
# ommitted variable bias 

# Exercise 5.1 
# march forward and take science in teh wrong direction 
# gentoo bill depth as a function of flipper lentgth 
# depth ~bill length vs depth ~ flipper length

ggplot(data=penguins)+
  geom_point(aes(x=flipper_length_mm, y=bill_depth_mm, color = species))+
  geom_smooth(method="lm", aes(x=flipper_length_mm, y=bill_depth_mm, color = species))+
  geom_smooth(method = "lm", aes(x=flipper_length_mm, y=bill_depth_mm), color = "black")

head(penguins)
summary(penguins)


lm3<- lm(bill_depth_mm ~ flipper_length_mm, data = gentoo)
summary(lm3)
ggplot(data=gentoo, aes(x = flipper_length_mm, y = bill_depth_mm)) +
  geom_point() +
  geom_smooth(method = "lm")

# I think that flipper length does a better job at predicting bill depth because the R^2
# is 0.49 so almost 50% 


