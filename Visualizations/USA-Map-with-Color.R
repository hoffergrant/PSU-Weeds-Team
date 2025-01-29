#Script for creating maps
#Author: Grant Hoffer
#Created: 1-14-2025
#Last Updated: 1-16-2025

####### Workspace management between versions or devices #######
#setwd("F:/Wallace Lab/GROW Data Analysis") #set working directory
#rm(IL_R6_wide) #remove extra or unwanted objects from global environment

# load packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("tidyverse","ggplot2","ggeffects","dplyr","ggsignif",'maps', 'sf')
ipak(packages)

####### Map creation #######
#This code requires downloading a GeoJSON file and saving to your device:
#Downloaded from this link: https://eric.clst.org/tech/usgeojson/ ; select 'US States' '20m' 'GeoJSON'
map <- sf::read_sf('F:/Wallace Lab/GROW Data Analysis/gz_2010_us_040_00_20m.json') #Reading shapefile of United States
map <- map %>% #Marking specific states to color differently from the rest
  mutate(include = case_when(NAME == 'Pennsylvania' | NAME == 'Delaware' | NAME == 'Ohio' | NAME == 'Nebraska' | NAME == 'Illinois' |
                               NAME == 'Maryland' | NAME == 'North Carolina' | NAME == 'Georgia' | NAME == 'Kentucky' ~ 1,
                             .default = 0)) 
fig.map <- ggplot(map) +
  geom_sf(color = "white", aes(fill = include)) +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50)) + #Limit map to only contiguous U.S.
  theme(legend.position = "none") +
  scale_fill_gradient(low ='wheat3', high = 'olivedrab4') + #Choose any colors; I like the earthy contrast between these; Scale_fill_gradient only allows for two colors, I think. Other functions allow more
  ggtitle('GROW Legacy Project 2023 to 2025') +
  theme(plot.title = element_text(size = 24)) +
  theme(axis.text.x = element_blank(), #Can remove these if you want the tick marks and lat/long coordinates
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
fig.map
#ggsave(file="F:/Wallace Lab/GROW Data Analysis/Figures/grow.map.jpg", scale=1.5, width=4.0, height=3.0, units="in", dpi=1200) #Uncomment to save after done with changes