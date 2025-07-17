##Join species harvest characteristics from chugach to tongass to create complete species list 

library(tidyverse)
library(readxl)


tongass <- read_excel("data/harvest_species_list_characteristics_5.xlsx")

chugach_sp_list <- read.csv("data/chugach_harvest_species_list.csv")

sp_all <- full_join(tongass, chugach_sp_list, by = c("Taxa_lvl1", "Taxa_lvl2", "Taxa_lvl3", "Taxa_lvl4", "Taxa_lvl5", "Habitat"))

write.csv(sp_all, "data/combined_species_list.csv")


##testing/looking in chugach data
chugach <- read.csv("data/intermediate_data/chugach_harvest_data_clean.csv")

test <- chugach %>%
  filter(Taxa_lvl5 == "Tuna_Mackerel")
