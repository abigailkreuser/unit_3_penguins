# 2022-02-15
# AMK


#installed on work mac
install.packages("tidyverse")
install.packages("palmerpenguins")


#load
library("tidyverse")
library("palmerpenguins")


tidyverse_packages()

# our intro to the dyplr package
# all about manipulating our data
# we will be visualizing with ggplot2 thursday

# broom and purr for modeling 
# erin started using it consistently in 2016?

#Hadley Wickham - theres a lot of help in R because he is the 
# chief scientist 
# saucy personality 

# tools are harder to teach - BS
# too dis-similar to other programming languages 


# Workflow 
# Import -> tidy -> Transform then eventually communicate 
# tidy - column- variable row - obs 

#tibbles are behind the scenes 
# some older functions don't work with tibbles 

# tidy group_by()- each year 
# uc santa barbara - leading in the field of data science and R 


head(penguins)
summary(penguins)
dim(penguins)


#tidyverse way 
glimpse(penguins)
# in rows -don't love 


#start playing with the dplyr package 

#base r
gentoo = penguins[penguins$species == "Gentoo",]
head(gentoo)

#tidyverse
gentoo = filter(penguins, species == "Gentoo") # with dplyr
head(gentoo)


gentoo_ladies = filter(penguins, species == "Gentoo", sex == "female")
head(gentoo_ladies)


# THE PIPE 
# you can string together functions instead of nesting them within one another

#h(g(f(x)))

# same as 
# x %>%
#  f() %>% # first we do f 
#  g() %>% # then we do g
#  h() %>% # then we do h with what was calculated above 
  
  
gentoo_ladies = penguins %>% 
  filter(species=="Gentoo", sex=="female") #should write with alot of vertical space
head(gentoo_ladies)


gentoo_ladies = penguins %>%
  filter(species == "Gentoo",
         sex == "female", 
         bill_length_mm > 44 , 
         body_mass_g < 5000)

head(gentoo_ladies)
dim(gentoo_ladies)


# can reindent lines!! so its readable!!!!! 


gentoo_ladies = penguins %>%
  filter(species == "Gentoo",
         sex == "female") %>%
  summarize(mean_body_mass_g = mean(body_mass_g),
            sd_body_mass_g = sd(body_mass_g)) 

# for all female gentoos take their body mass and store as column
gentoo_ladies


# Exercise 1.1 
# chinstrap ony 
# >200 mm flipper length
# what is the sex ratio summary() to examine 

summary(penguins)

chinstrap_only = penguins %>%
  filter(species == "Chinstrap")
summary(chinstrap_only)
# sex ratio 34:34

chinstrap_200mmFlipper = penguins %>%
  filter(species == "Chinstrap",
         flipper_length_mm > 200)
summary(chinstrap_200mmFlipper)
# sex ratio 1:17 female:male

penguin_stats_by_sex = penguins %>%
  filter(!is.na(sex)) %>%
  group_by(species, sex) %>%
  summarize(count = n(),
            mean_body_mass_g = mean(body_mass_g),
            sd_body_mass_g = sd(body_mass_g))
penguin_stats_by_sex

write_csv(penguin_stats_by_sex, file = "data/processed/Table1_penguins_stats_by_sex.csv")



penguins_for_america = penguins %>%
  mutate(body_mass_lb = body_mass_g * 0.0022) # 0.0022 lb/g

glimpse(penguins_for_america)


penguins %>%
  distinct(island) %>%
  arrange(island)

penguins_mass_sort = penguins %>% 
  arrange(desc(body_mass_g))
penguins_mass_sort


penguins_no_morph = penguins %>%
  select(species, sex, island, year)
penguins_no_morph


penguins_no_flipper = penguins %>%
  select(-flipper_length_mm)
glimpse(penguins_no_flipper)

#text book for the class is written mainly for the tidy verse 



#Exercise 1.3 
# mean bill length in inches -murica
# Adelie penguins on Dream island or biscoe 
# Sd
# compar to Torgersen island 

head(penguins)

penguins_for_america = penguins %>%
  mutate(body_mass_lb = body_mass_g * 0.0022, # 0.0022 lb/g
         bill_length_in = bill_length_mm * 0.0393701)
penguins_for_america

glimpse(penguins_for_america)

# subset Adelie and seperate islands
adelie_dream_biscoe_mean_bill <- penguins_for_america %>%
  filter(island != "Torgersen", 
         !is.na(bill_length_in )) %>%
  summarize(bill_mean_in = mean(bill_length_in),
            bill_sd_in = sd(bill_length_in))
  
adelie_dream_biscoe_mean_bill

adelie_torgersen_mean_bill <- penguins_for_america %>%
  filter(island == "Torgersen", 
         !is.na(bill_length_in )) %>%
  summarize(bill_mean_in = mean(bill_length_in),
            bill_sd_in = sd(bill_length_in))

adelie_torgersen_mean_bill

# bill length is smaller on torgersen!




