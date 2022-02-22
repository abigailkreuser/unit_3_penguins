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

iinstall.packages("GGally")
library(GGally)


gentoo %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  ggpairs()
