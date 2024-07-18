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

##deer
ggplot(df) +
  geom_phylopic(aes(x = x, y = y, name = "odocoileus hemionus"), fill = "#339933",color="black", size = 10) +
  coord_cartesian(xlim = c(1,3), ylim = c(5, 30)) +
  theme_void()

##dungeness crab
ggplot(df) +
  geom_phylopic(aes(x = x, y = y, name = "metacarcinus magister"), fill = "#CC9966",color="black", size = 10) +
  coord_cartesian(xlim = c(1,3), ylim = c(5, 30)) +
  theme_void()


##scallop
ggplot(df) +
  geom_phylopic(aes(x = x, y = y, name = "pectinidae"), fill = "#003366",color="black", size = 10) +
  coord_cartesian(xlim = c(1,3), ylim = c(5, 30)) +
  theme_void()

##clam
ggplot(df) +
  geom_phylopic(aes(x = x, y = y, name = "Leukoma staminea"), fill = "#CC9966",color="black", size = 10) +
  coord_cartesian(xlim = c(1,3), ylim = c(5, 30)) +
  theme_void()

##sea urchin
ggplot(df) +
  geom_phylopic(aes(x = x, y = y, name = "Strongylocentrotus"), fill = "#CC9966",color="black", size = 10) +
  coord_cartesian(xlim = c(1,3), ylim = c(5, 30)) +
  theme_void()



##black bear
ggplot(df) +
  geom_phylopic(aes(x = x, y = y, name = "ursus americanus"), fill = "#339933",color="black", size = 12, remove_background = TRUE) +
  coord_cartesian(xlim = c(1,3), ylim = c(5, 30)) +
  theme_void()

##chinook salmon
ggplot(df) +
  geom_phylopic(aes(x = x, y = y, name = "oncorhynchus tshawytscha"), fill = "#FF9999",color="black", size = 10) +
  coord_cartesian(xlim = c(1,3), ylim = c(5, 30)) +
  theme_void()


##trout
ggplot(df) +
  geom_phylopic(aes(x = x, y = y, name = "Salvelinus"), fill = "#FF9999",color="black", size = 10) +
  coord_cartesian(xlim = c(1,3), ylim = c(5, 30)) +
  theme_void()

##smelt
ggplot(df) +
  geom_phylopic(aes(x = x, y = y, name = "osmeridae"), fill = "#FF9999",color="black", size = 10) +
  coord_cartesian(xlim = c(1,3), ylim = c(5, 30)) +
  theme_void()

##duck
ggplot(df) +
  geom_phylopic(aes(x = x, y = y, name = "anatidae"), fill = "#339933",color="black", size = 10) +
  coord_cartesian(xlim = c(1,3), ylim = c(5, 30)) +
  theme_void()


##halibut
ggplot(df) +
  geom_phylopic(aes(x = x, y = y, name = "Pleuronectidae"), fill = "#003366",color="black", size = 10) +
  coord_cartesian(xlim = c(1,3), ylim = c(5, 30)) +
  theme_void()


##seal
ggplot(df) +
  geom_phylopic(aes(x = x, y = y, name = "otariidae"), fill = "#003366",color="black", size = 10) +
  coord_cartesian(xlim = c(1,3), ylim = c(5, 30)) +
  theme_void()

##rockfish
ggplot(df) +
  geom_phylopic(aes(x = x, y = y, name = "Sebastes melanops"), fill = "#003366",color="black", size = 10) +
  coord_cartesian(xlim = c(1,3), ylim = c(5, 30)) +
  theme_void()


##herring
ggplot(df) +
  geom_phylopic(aes(x = x, y = y, name = "clupea pallasii"), fill = "#003366",color="black", size = 10) +
  coord_cartesian(xlim = c(1,3), ylim = c(5, 30)) +
  theme_void()

##king crab (this is spider crab....most look a like but still not right species)
ggplot(df) +
  geom_phylopic(aes(x = x, y = y, name = "Inachus dorsettensis"), fill = "#003366",color="black", size = 10) +
  coord_cartesian(xlim = c(1,3), ylim = c(5, 30)) +
  theme_void()

##shrimp (not exact right species)
ggplot(df) +
  geom_phylopic(aes(x = x, y = y, name = "Caridina babaulti"), fill = "#003366",color="black", size = 10) +
  coord_cartesian(xlim = c(1,3), ylim = c(5, 30)) +
  theme_void()

##cod
ggplot(df) +
  geom_phylopic(aes(x = x, y = y, name = "Gadus chalcogrammus"), fill = "#003366",color="black", size = 10) +
  coord_cartesian(xlim = c(1,3), ylim = c(5, 30)) +
  theme_void()

##hare
ggplot(df) +
  geom_phylopic(aes(x = x, y = y, name = "lepus"), fill = "#339933",color="black", size = 10) +
  coord_cartesian(xlim = c(1,3), ylim = c(5, 30)) +
  theme_void()

##berries
ggplot(df) +
  geom_phylopic(aes(x = x, y = y, name = "vaccinium"), fill = "#339933",color="black", size = 10) +
  coord_cartesian(xlim = c(1,3), ylim = c(5, 30)) +
  theme_void()

##fern
ggplot(df) +
  geom_phylopic(aes(x = x, y = y, name = "Osmunda"), fill = "#339933",color="black", size = 10) +
  coord_cartesian(xlim = c(1,3), ylim = c(5, 30)) +
  theme_void()

plants <- browse_phylopic("vaccinium")
