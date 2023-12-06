##Taxonomic names for species list

library(tidyverse)
library(data.table)
library(taxize)
library(readxl)


##read in cleaned dataframe saved to intermediate files
setwd("~/Desktop/Wild Foods Repo/")
df <- read.csv("intermediate_files/harvest_data_clean.csv") 


##going to try to see if just removing the unknown works, so is just lowest taxonomic name
df$Taxa_lvl5_b <- str_remove(df$Taxa_lvl5, "Unknown ")
df$Taxa_lvl4_b <- str_remove(df$Taxa_lvl4, "Unknown ")


##create species list

sp_list <- df %>%
  distinct(Taxa_lvl1, Taxa_lvl2, Taxa_lvl3, Taxa_lvl4_b, Taxa_lvl5_b, Habitat) 


write.csv(sp_list, "intermediate_files/harvest_species_list.csv")




##unknown large land mammals -- only in Haines 1983, there are # harvested, but no estimated harvest weights because there is no conversion unit... 
##there may be certain things like this we can't get around..but this does somehow need to be taken into account when comparing food webs... 
##I think Sea bass are the same as black rockfish

test <- df %>%
  filter(Taxa_lvl3 == "Crab")

test_plot <- ggplot(test, aes(x = Taxa_lvl4_b, y = Estimated_Total_Pounds_Harvested)) +
  geom_boxplot()

##256 unique taxa 
tax_name <- tax_name(sp_list$Species, get = c("Species", "Genus", "Family"), db = "itis") 


##working trophic info, looking into certain things

test <- df %>%
  filter(Taxa_lvl5_b == "Cod")

##may want to add genus back in for birds.. 