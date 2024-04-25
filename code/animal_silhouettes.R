# Getting Silhouttes of Animals

#Author(s): Reilly O'Connor
#Version: 2024-04-12

#Pkgs
library(tidyverse)
library(RColorBrewer)
library(beepr)
library(rphylopic)
library(rsvg)
library(grid)

##### Code #####
#Create data frame for img coords
df <- data.frame(x = c(2, 4), y = c(10, 20))

ggplot(df) +
  geom_phylopic(aes(x = x, y = y, name = "desmodus rotundus"), fill = "#DB6961",color="black", size = 10) +
  coord_cartesian(xlim = c(1,3), ylim = c(5, 30)) +
  theme_void()