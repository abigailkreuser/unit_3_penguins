# 2022-02-22
# AMK

# the paired t-test is if the two distributions are not independent
# paired data - urchins before and after
# just run the same test with parameter paired =TRUE
# independent - unpaired t-tests, independent sampling


install.packages("rstatix")
library(rstatix) # has its own filter fxn, but loaded in tidyverse last
library(palmerpenguins)
library(tidyverse)


# need to fully investigate data before running stats to not contribute 
# to junk science

# looking at the literature, (eol) and using a mass from a paper to compare

head(penguins)
glimpse((penguins))


# plot body mass histograms
ggplot()+
  geom_histogram(data=penguins, aes(x=body_mass_g, fill = species))


#in comparison to somebody et al 5500g seems high compared to our histogram
gentoo = penguins %>%
  filter(species == "Gentoo")
mean(gentoo$body_mass_g, na.rm =TRUE)

#two ways to remove NAs
gentoo = penguins %>%
  filter(species == "Gentoo",
         !is.na(body_mass_g))
mean(gentoo$body_mass_g)


sd(gentoo$body_mass_g)


ggplot()+
  geom_histogram(data=gentoo, aes(x=body_mass_g))
#erin doesn't put alot of weight on outliers or removing them 
#because outliers are still removed by arbitralily set threshhold

#have some mechanism - for a gut check for like 999s and misread data
#thats different than outliers - a well weighed penguin- that just ate a lepoard seal

#QQ plot
ggplot()+
  stat_qq(aes(sample=body_mass_g), data=gentoo)

#132 body massses of penguins, the x axis is theoretical in a perfect world where the data would fall
#if 1 to 1 line then your data  is pretty normal
#if it looks like an S shape then your data is not normal

gentoo_body_mass_g_lit=5500 # from paper XXX et al 2010
t.test(gentoo$body_mass_g, mu=gentoo_body_mass_g_lit)
#mu is the expected mean


#tidyverse version
t_test_results = gentoo %>%
  t_test(body_mass_g~1, mu=5500)
t_test_results


#if you like words for 1 t-test
#tidyverse version is nice for running 100 t-tests and putting it all in one data frame


# compare Gentoo and Adelie body mass 
data_for_t_test = penguins %>%
  filter(species %in% c("Gentoo", "Adelie"))
summary(data_for_t_test) # through this way of filtering 
# r still remembers chinstrap is a factor 


data_for_t_test = penguins %>%
  filter(species %in% c("Gentoo", "Adelie"),
         !is.na(body_mass_g)) %>%
  select(species, body_mass_g) %>%
  droplevels() #forgets chinstraps were in the data set 
summary(data_for_t_test) 
tail(data_for_t_test)

data_for_t_test %>%
  group_by(species) %>% #running a summary on every single group
  summarize(mean_body_mass_g = mean(body_mass_g),
            sd_body_mass_g = sd(body_mass_g))
ggplot()+
  geom_histogram(data=data_for_t_test, aes(x=body_mass_g))+
  facet_wrap(~species)

ggplot()+
  geom_histogram(data=data_for_t_test, aes(x=body_mass_g))+
  facet_wrap(~species, scales="free") #normaly cscale= fix
# but free re asseses the scale for each panel 


#QQ plots
ggplot() + 
  stat_qq(data=data_for_t_test, aes(sample=body_mass_g))+ 
  facet_wrap(~species, scales = "free")
# small s shape with the adelies 


# t-test are gentoo and adelie body masses significantly different?
data_for_t_test %>% levene_test(body_mass_g~species) # if p < 0.05variences are unequal
# now we can go ahead with the "students t-test"

t.test(data_for_t_test$body_mass_g ~ data_for_t_test$species)
# immediately says it is running the welches t-test, because 
# r does not trust us to check variance 

#students t-test
t.test(data_for_t_test$body_mass_g ~ data_for_t_test$species, var.equal = TRUE)


# dplyr-friendly version:
data_for_t_test %>% 
  t_test(body_mass_g ~ species)

#if running a paired test, need to make sure that the observations
#line up correctly in a data set 
# like 1 to 1


# Exercise 3.1
#looking to see if Adelie flipper lengths vary between sexes

#subset data 


head(penguins)
adelie_flipper <- penguins %>%
  filter(species == "Adelie", 
         !is.na(sex)) %>%
  select(species, flipper_length_mm, sex) %>%
  droplevels()

# check data
head(adelie_flipper)


# plot histogram
ggplot()+
  geom_histogram(data = adelie_flipper, aes(x= flipper_length_mm))+
  facet_wrap(~sex)

#use levene test to check variences
adelie_flipper %>% levene_test(flipper_length_mm ~sex)

# run t-test to determine if male and females differ in flipper length?
flipper_t_test_results <- adelie_flipper %>% 
  t_test(flipper_length_mm ~ sex)
flipper_t_test_results



