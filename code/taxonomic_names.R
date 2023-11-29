##Taxonomic names for species list

library(tidyverse)
library(data.table)
library(taxize)

##read in cleaned dataframe saved to intermediate files
setwd("~/Desktop/Wild Foods Repo/")
df <- read.csv("intermediate_files/harvest_data_clean.csv") 

##lets see if can sort unknowns, want to remove the unknown part
df_test <- df %>%
  filter(grepl("Unknown", Species))

test <- df %>%
  filter(Site_Year_Code == "Angoon_1987")

##going to try to see if just removing the unknown works, so is just lowest taxonomic name
df$Species_2 <- str_remove(df$Species, "Unknown ")


##create species list
sp_list <- df %>%
  distinct(General_Category, General_Category_lvl2, Family, Species, Species_2, Habitat) 





write.csv(sp_list, "intermediate_files/harvest_species_list.csv")
##258 unique taxa 
tax_name <- tax_name(sp_list$Species, get = c("Species", "Genus", "Family"), db = "itis") 


##working trophic info, looking into certain things 

##may want to add genus back in for birds.. 