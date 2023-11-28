##Taxonomic names for species list

library(tidyverse)
library(data.table)
library(taxize)

##read in cleaned dataframe saved to intermediate files
setwd("~/Desktop/Wild Foods Repo/")
df <- read.csv("intermediate_files/harvest_data_clean.csv")

##create species list
sp_list <- df %>%
  distinct(General_Category, General_Category_lvl2, Family, Species, Habitat) #%>%
 # filter(!grepl("Unknown", Species))

write.csv(sp_list, "intermediate_files/harvest_species_list.csv")
##245 unique taxa 
tax_name <- tax_name(sp_list$Species, get = c("Species", "Genus", "Family"), db = "itis") 
