#2022-02-22

#run correlations bout bill length vs. depth 


#using data from t-test scripts 

library(tidyverse)
library(palmerpenguins)

gentoo = penguins %>% 
  filter(species=="Gentoo")

# Exploratory data analysis:
glimpse(gentoo)
summarize(gentoo)
ggplot() +
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm), data=gentoo)



#corelations 
cor(x=gentoo$bill_length_mm, y=gentoo$bill_depth_mm)
# the number of a corelation can be anywhere from -1 to 1 
# if it is 0 than there is no correlation 
cor.test(x=gentoo$bill_length_mm, y=gentoo$bill_depth_mm)


# for all species 
cor.test(x=penguins$bill_length_mm, y=penguins$bill_depth_mm)
# results are curious 


# correltation matrix
head(gentoo)
cor(gentoo[,c(3:6)])# need to fix the parenthese typing 

library(GGally)


gentoo %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  ggpairs()


gentoo %>%
  select(-species, -island, -sex, -year) %>%
  GGally::ggpairs() #GGally do the lexical scoping to say what packae it is coming from 

penguins %>%
  select(ends_with("_mm"), body_mass_g) %>% #grabs the columns that are mm measurements 
  ggpairs()
# theres some corelations that are positive and negative, whciih helps us realize we should do some grouping 

# break it up by species to get idea of whats going on 
penguins %>%
  select(ends_with("_mm"), body_mass_g, species) %>% #grabs the columns that are mm measurements 
  ggpairs(aes(color=species))

#tyler