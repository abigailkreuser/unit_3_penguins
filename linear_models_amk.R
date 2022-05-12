#2022-03-01
# linear models

library(car)
library(palmerpenguins)
library(tidyverse)
library(ggiraph)
library(ggiraphExtra) 
library(broom)
library(tidyr)

#reviewing the simpsons paradox of the bill depth and bill length
# multiple regression is often what we see people doing instead 
# of running multiple simple linear models
# encouraged to ruminate on philosophy at length about modeling 
# one outputs one slope for all th different groups and then they just have different y intercepts

# Categorical vs continuous 
# year- can be either
# benefits to categorical year based on food availability 
# to explain crashes in 07 or 08
# then you can site papers that have more data than you. 


# multi colinearity 
# multiple variables that are smmilar together can be problematic 
#depending on goals and the statistics. 
# lot of phiolosphy that goes into building models
# these things persist throughout larger and more complex models. 

#build multiple regression
penguins_lm3 = penguins %>%
  filter(!is.na(bill_depth_mm),
         !is.na(bill_length_mm),
         !is.na(species))
lm3<- lm(bill_depth_mm~bill_length_mm + species, data = penguins_lm3)
summary(lm3)
summary(penguins_lm3) # automatically assigns Adelie as the first one 
# so the summary of the model is comparing chinstrap and gentoo intercepts relative to adelie


#how to access summary results and the coeffiecints 
coef(lm3)
coef(lm3)[2]
broom::tidy(lm3) # now it gives you a data frame and you can print this out to a csv. and save as a variable 
#look up tidy.lm if looking for documentation

broom::tidy(lm3, conf.int = TRUE) # now it gives you a data frame and you can print this out to a csv. and save as a variable 
#look up tidy.lm if looking for documentation

lm3_coefs <- broom::tidy(lm3, conf.int = TRUE) %>%
  mutate_if(is.numeric, round, 2)
# now can print that out based on our questions and our journal 

# visualize 
#cheater wayy using ggiraph 
ggPredict(lm3, se = TRUE, interactive = TRUE)
# lines span full range of species all the same slope, diff intercepts 
# interactive you can hover over point and lines to see equation of lines 
# can see what row number a point is in
# this can be nice for showing colleuges on the fly for looking at certain points 
# and intercepts 
# theres a nice vingette that erinn links too that goes through this on the internet 


#visualize using base R 
lm3_predictions<- predict(lm3, interval="confidence")
head(lm3_predictions) # now we have the y's predicted and the confidence interval 

lm3_predictions <- cbind(penguins_lm3, lm3_predictions)
# now we have the true data and the model predictions 


ggplot(data=lm3_predictions)+
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm, color = species))+
  geom_line(aes(x=bill_length_mm, y=fit, color=species))
# the r predict line only goes through the rang of data that we have 
# the ggiraph version went the full range of the whole data set


ggplot(data=lm3_predictions)+
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm, color = species))+
  geom_line(aes(x=bill_length_mm, y=fit, color=species))+
  geom_ribbon(aes(x=bill_length_mm, ymin=lwr, ymax=upr, fill= species), alpha=0.3)+
  theme_bw()


#give predict() new data that is outside of our current range 
newdata_bill_length <- seq(from=min(penguins_lm3$bill_length_mm), to=max(penguins_lm3$bill_length_mm), by=01)
# by 0.1 because we want alot of info around where our conf int get larger
# we could just use two points but thats no good

newdata= expand.grid(bill_length_mm = newdata_bill_length, species=unique(penguins_lm3$species))
head(newdata)
tail(newdata)

newdata_predict_lm3 <- cbind(newdata, predict(lm3, interval = "confidence", newdata=newdata)) #has to be structured in the same way
head(newdata_predict_lm3)

#visualize with new data
ggplot()+
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm, color=species), data=penguins_lm3)+
  geom_line(aes(x=bill_length_mm, y=fit, color=species), data=newdata_predict_lm3)+
  geom_ribbon(aes(x=bill_length_mm, ymax=upr, ymin=lwr, fill=species), alpha=0.3, data=newdata_predict_lm3)


#one more time but the tidyverse way
#generating model predictions 
lm3_predict_tidy <- lm3 %>%
  broom::augment(penguins_lm3, se_fit=TRUE, interval = "confidence") #add the se, 

head(lm3_predict_tidy)
glimpse(lm3_predict_tidy)

#plot same graph with the tidyverse data
ggplot()+
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm, color=species), data=penguins_lm3)+
  geom_line(aes(x=bill_length_mm, y=.fitted, color=species), data=lm3_predict_tidy)+
  geom_ribbon(aes(x=bill_length_mm, ymax=.upper, ymin=.lower, fill=species), alpha=0.3, data=lm3_predict_tidy)


#generate and predict new data using the tidyverse
library(tidyr)
newdata <- penguins_lm3 %>%
  tidyr::expand(bill_length_mm, species)# making sure the full data set matches with evry possible combo of data set



lm3_predict <- lm3 %>%
  broom::augment(newdata=newdata, se_fit=TRUE, interval="confidence") #READ THE DOCuMENTATION


head(lm3_predict)
head(newdata)
tail(newdata)

########################
# 2022-03-03


#doing models with interactions today 
# length + length:species
# does not look at the specific terms of the interaction on their own 
# but the interaction of depth ~ length*species 

#interaction model 
lm4 = lm(bill_depth_mm ~ bill_length_mm * species, data = penguins_lm3)
summary(lm4)
summary(lm3) 

AIC(lm3, lm4) #AIC goes beyond linear models 
# the log likliehood that beyond the model the data is real
# there is a penalty for log likeliehood in complexity 
#consecutive models 
#a smaller AIC is bettter
# a difference in 2 units in AIC is statisitcally significant 
# normally AIC is a delta AIC a change in  AIC

#step function
step(lm4)
# will remove one term at a time and will tell you is the simpler model is more parismonious 
# difference in AICs is preserved 

bm=step(lm4) # haha need to text kenz
summary(bm)


#visualize interaction model 
ggPredict(lm4, se=TRUE, interactive = TRUE)


# visualize 

lm4_predict <- lm4 %>%
  augment(se_fit=TRUE, interval="confidence")
head(lm4_predict)

#residuals are useful for model diagnostics 

ggplot(data=lm4_predict)+
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm, color=species)) +
  geom_line(aes(x=bill_length_mm, y=.fitted, color=species))+
  geom_ribbon(aes(x=bill_length_mm, ymin=.lower, ymax=.upper, fill=species), alpha=0.3) +
  theme_bw()




# multiple continous variables 

gentoo = penguins %>%
  filter(species == "Gentoo")
head(gentoo)

lm1 = lm(bill_depth_mm ~ bill_length_mm, data = gentoo)
lm2 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm, data=gentoo)
lm3 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm + body_mass_g, data=gentoo)

summary(lm3)
vif(lm3) #varience inflation factor in car() package
# looking for a vif below 3 (include a citation somebody et al. 2020)
# when trying to asses colinearity and reviewers are asking about it 

step(lm3)
#this time the output is shorter
# because it decided that removing on eof these variables was not parsimonious 

bm=step(lm3)
summary(bm)


AIC(lm1, lm2, lm3)


#visualize 
# how are we going to visualize 4 deminsions ???
#3d plots not a fan 

#making lots ofo 2d plots and then hold the extra variables at their median vaules
# can also hold at interquartile range 
# now we have to build a data set 
# that holds the extra values at their medians

gentoo = penguins %>%
  filter(species=="Gentoo", 
         !is.na(flipper_length_mm))

head(gentoo) #botched it with left in NAs
#visualize 
newdata = gentoo%>%
  select(bill_length_mm) %>%
  mutate(flipper_length_mm = median(gentoo$flipper_length_mm),
         body_mass_g = median(gentoo$body_mass_g))
head(newdata)


lm3_predict = lm3 %>%
  augment(newdata = newdata, interval= "confidence")

head(lm3_predict)


ggplot() +
  geom_point(data=gentoo, aes(x = bill_length_mm, y = bill_depth_mm)) + # original data
  geom_ribbon(data=lm3_predict, aes(x = bill_length_mm, ymin = .lower, ymax = .upper), alpha = .15) +
  geom_line(data=lm3_predict, aes(y = .fitted, x =bill_length_mm), size = 1)+
  annotate("text", x=57, y=13, label= paste0("flipper length = ", median(gentoo$flipper_length_mm, na.rm=TRUE), "mm")) +
  annotate("text", x=57, y=13.2, label= paste0("body mass = ", median(gentoo$body_mass_g, na.rm=TRUE), "g")) 


  #Exercise 5.3 
#bill depth vs flipper length while holding bill length and body mass constant at medians  
newdata2 = gentoo%>%
  select(bill_depth_mm) %>%
  mutate(bill_length_mm = median(gentoo$flipper_length_mm),
         body_mass_g = median(gentoo$body_mass_g))
head(newdata2) 

#okay so now I want the fitted predictions of flipper_length
lm3_predict = lm3 %>%
  augment(newdata = newdata, se_fit=TRUE, interval="confidence")

head(lm3_predict)  


ggplot(data=lm3_predict) 
##########

#the answer
#Exercise 5.3 
#bill depth vs flipper length while holding bill length and body mass constant at medians  
newdata2 = gentoo%>%
  select(flipper_length_mm) %>%
  mutate(bill_length_mm = median(gentoo$bill_length_mm),
         body_mass_g = median(gentoo$body_mass_g))
head(newdata2) 

#okay so now I want the fitted predictions of flipper_length
lm3_predict = lm3 %>%
  augment(newdata2 = newdata2, interval="confidence")

head(lm3_predict)  


ggplot(data=lm3_predict) +
  geom_line(aes(x = flipper_length_mm, y=.fitted)) +
  geom_ribbon(aes(x= flipper_length_mm,  ymin = .lower, ymax = .upper), alpha = .3) +
  geom_point(aes(x = flipper_length_mm, y =bill_depth_mm ), data =gentoo)
  # that was ugly af and wrong



#ANOVA
penguin_lm = lm(body_mass_g ~species + sex, data = penguins)
summary(penguin_lm)
anova(penguin_lm)

penguin_aov=aov(body_mass_g ~species + sex, data = penguins) #would not run on a linear model 
summary(penguin_aov) 
penguins %>%
  group_by(sex) %>%
  summarize(mean=mean(body_mass_g))
#by using simple summary statistics we can see males are larger and females are smaller 

TukeyHSD(penguin_aov)
