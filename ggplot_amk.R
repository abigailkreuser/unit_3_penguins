# 2022-02-17
# HACKATHON

# education data? 

# ggplot introduction 
# penguins and dyplyr 

install.packages("tidyverse")
library(tidyverse)
install.packages("palmerpenguins")
library(palmerpenguins)


head(penguins)
glimpse(penguins)
summary(penguins)
tail(penguins)

# name spaces when packages use the same name
# use find("filter")

find("filter")

my_data = penguins %>%
  dplyr::select(sex, species) 

# are you accessing the fxn you thinkn your accesing??
# be explicit with :: symbol to say what package you want the name space from
# also use the find() to see what you are using

# scatterplot
# adding a layer with a + that means stringing together 
# before in script we have only been using the whole chunk but
# plus sign is needed ini rmd to plot all on one
# geom geometry -  i want to add points on my plot 
# aes asthetics - assign the x and assign the y

ggplot() +
  geom_point(data=penguins, aes(x=flipper_length_mm, y=body_mass_g),
             color = "red")
ggplot() +
  geom_point(data=penguins, aes(x=flipper_length_mm, y=body_mass_g, color = species))

ggplot() +
  geom_point(data=penguins, aes(x=flipper_length_mm, 
                                y=body_mass_g, 
                                color = species, 
                                shape=species))
# changing color with in aes() assigns to types of data, outside is all 
ggplot(data=penguins, aes(x=flipper_length_mm, y=body_mass_g)) + #since this is all im working with today otherwise put it in the geom part 
  geom_point(data=penguins, aes(x=flipper_length_mm, 
                                y=body_mass_g, 
                                color = species, 
                                shape=species)) +
  geom_smooth(aes(x=flipper_length_mm, y=body_mass_g))



ggplot(data=penguins, aes(x=flipper_length_mm, y=body_mass_g)) + #since this is all im working with today otherwise put it in the geom part 
  geom_point(aes(color = species, 
                 shape=species)) +
  geom_smooth()+
  xlab("Flipper length (mm)") +
  ylab("Body mass (g)")+
  ggtitle("Penguins are CUTE XD")

# erin is not really into smoothers 

#looking for the NAs that the warning said is removed
summary(penguins)

#if I wanted to search for those NAs and see whats going on
penguins %>%
  filter(is.na(flipper_length_mm)) %>%
  
  
  summarize(num_nas = n())

penguins %>%
  filter(is.na(flipper_length_mm)) %>%
  
  
  summarize(num_nas = n())


head(penguins)

adelie <-penguins %>%
  filter(species == 'Adelie')
head(adelie)


summary(adelie)
ggplot() +
  geom_point(data=adelie, aes(x=bill_depth_mm, y=bill_length_mm, color = island))




# line plots !!!! 
penguin_ts = penguins %>%
  group_by(species, year) %>%
  summarize(count = n())

ggplot(data=penguin_ts)+
  geom_line(aes(x=year, y=count, color=species))


# histogram
ggplot()+
  geom_histogram(data=penguins %>% filter(species == 'Adelie'), aes(x=flipper_length_mm))


ggplot()+
  geom_histogram(data=penguins, aes(x=flipper_length_mm, color = species))
#dumb its the outline

ggplot()+
  geom_histogram(data=penguins, aes(x=flipper_length_mm, fill = species))
# right now they are stacked on top of eachother 


ggplot()+
  geom_histogram(data=penguins, aes(x=flipper_length_mm, fill = species),
                 position = "identity")
# can kind of see the normal dist,
# idnetity - mapping three seperate histograms that now overlap
# they each have their own identity 
# before they were stacked as one and did not have their own identity


ggplot()+
  geom_histogram(data=penguins, aes(x=flipper_length_mm, fill = species),
                 position = "identity", alpha=.7)
# alpha shows the detail of the plots overlapping eachother 

ggplot()+
  geom_histogram(data=penguins, aes(x=flipper_length_mm, fill = species),
                 position = "identity", alpha=.7)+
  scale_fill_manual(values = c("darkorange", "darkorchid", "cyan4"))

                    




# box plots !!!!

ggplot(data=penguins)+
  geom_boxplot(aes(y=flipper_length_mm)) # not alot of info here 
# want to split up by species 

ggplot(data=penguins)+
  geom_boxplot(aes(x=species, y=flipper_length_mm))
# erin wants to see the data the points on top of the box plot

ggplot(data=penguins)+
  geom_boxplot(aes(x=species, y=flipper_length_mm))+
  geom_jitter(aes(x=species, y=flipper_length_mm, color = species))
  
  
  
# jitter adds some random placeent across an axis
  # points would put it all in a line 


# want narrower, change width this looks nice 
ggplot(data=penguins)+
  geom_boxplot(aes(x=species, y=flipper_length_mm))+
  geom_jitter(aes(x=species, y=flipper_length_mm, color = species), width = 0.2)



ggplot(data=penguins)+
  geom_boxplot(aes(x=species, y=flipper_length_mm))+
  geom_jitter(aes(x=species, y=flipper_length_mm, color = species), width = 0.2)+
  ggsave(filename="figures/flipperLengthBoxPlot.png")

# OR so not apart of a plot

ggplot(data=penguins)+
  geom_boxplot(aes(x=species, y=flipper_length_mm))+
  geom_jitter(aes(x=species, y=flipper_length_mm, color = species), width = 0.2)
ggsave(filename="figures/flipperLengthBoxPlot.png", 
       dpi=300, device="png", width=6, height =4.5, units="in")
# by default it is grabbing what was last plotted 


flipper_box=ggplot(data=penguins)+
  geom_boxplot(aes(x=species, y=flipper_length_mm))+
  geom_jitter(aes(x=species, y=flipper_length_mm, color = species), width = 0.2)
ggsave(plot=flipper_box, filename="figures/flipperLengthBoxPlot.png", 
       dpi=300, device="png", width=6, height =4.5, units="in")
#so you can name the plot!!!  



# erin normally use a png because its lossless 
# powerpoint - pdfs can look like junk/ if you dont move the pdf with it 
# resolution dpi can be saved 




# bar plots 
ggplot()+
  geom_bar(aes(x=sex, fill = species), data=penguins) + 
  facet_wrap(~species) # a slick tool, a unique plot for each variable
                        # hand it a function of with the tilda ~


ggplot()+
  geom_bar(aes(x=sex, fill = species), data=penguins) + 
  facet_wrap(~species, nrow = 3) # a slick tool, a unique plot for each variable
# hand it a function of with the tilda ~


ggplot()+
  geom_bar(aes(x=island, fill = species), data=penguins) + 
  facet_wrap(~species) # a slick tool, a unique plot for each variable
# hand it a function of with the tilda ~


ggplot()+
  geom_bar(aes(x=island, fill = species), data=penguins) + 
  facet_wrap(~species, ncol=1)+
  coord_flip()
# depending on what story you want to tell may be how you want to
# represent 
# coord_flip- if sepecies names are long at the bottom etc



ggplot()+
  geom_bar(aes(x=island, fill = species), data=penguins) + 
  facet_wrap(~species, ncol=1)+
  coord_flip() +
  theme_classic()
#themes can be changed!!! 

#stack overflow is helpful for cusdtomizign your graphics 
# but not for analyzing your data!! 
#at bottom of tutorial. 

#Erin Coding books??
# gganimate is sexyyyyyyy