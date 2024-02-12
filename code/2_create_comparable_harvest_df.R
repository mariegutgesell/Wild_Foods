##Determining lowest common taxonomic unit across communities/across years 
##Create working dataframe of comparable taxa across years/communities 

library(tidyverse)
##import cleaned harvest data and trophic info
setwd("~/Desktop/Wild Foods Repo/")
source("code/1_dataframe_formation.R")
rm(list = ls()[!ls() %in% c("df_final")])
trophic_df <- read_excel("data/harvest_species_list_characteristics_5.xlsx", sheet = 2)

##filter to only species harvested in harvest data, select only columns needed for food web interaction, then join trophic data
##note: in future will likely recalculate the harvest metrics, but lets not worry about that now -- also still need to fix those where conversion unit is 0 but did harvest resources
harvest_df <- df_final %>%
  select(Site_Year_Code, Taxa_lvl1, Taxa_lvl2, Taxa_lvl3, Taxa_lvl4, Taxa_lvl5, Sampled_households:Est_Comm_Population, Conversion_Units_To_Pounds:Estimated_Total_Pounds_Harvested, Percapita_Pounds_Harvested:Estimated_Amount_Harvested) %>%
  filter(!if_all(Percent_Using:Estimated_Amount_Harvested, ~ .x == 0))

harvest_df$Taxa_lvl5 <- str_remove(harvest_df$Taxa_lvl5, "Unknown ")
harvest_df$Taxa_lvl4 <- str_remove(harvest_df$Taxa_lvl4, "Unknown ")

##join trophic info for the lowest common taxa across all years and communities
df <- harvest_df %>%
  left_join(trophic_df %>% select(Taxa_lvl1:Lowest_Common_Taxon_lvl, Scientific_Name, Habitat, Habitat_2, Trophic_Level, Trophic_Category), by = c("Taxa_lvl1", "Taxa_lvl2", "Taxa_lvl3", "Taxa_lvl4", "Taxa_lvl5")) %>%
  select(Site_Year_Code:Taxa_lvl5, Lowest_Common_Taxon_Name:Trophic_Category, Sampled_households:Estimated_Amount_Harvested) %>%
  filter(!Lowest_Common_Taxon_Name %in% c("Not_comparable", "NA"))

str(df)

test <- harvest_df %>%
  filter(Taxa_lvl3 == "Shark")

##calculate harvest metrics for the lowest common taxon, to be used for harvest analysis moving forward
##for Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Percapita_Pounds_Harvested, Number_of_Resource_Harvested, Estimated_Amount_Harvested want to sum by 
##How to handle the percents? take average? i think that might make most dense... 

unique_trophic_common_taxa <- df %>%
  ungroup() %>%
  select(Site_Year_Code, Lowest_Common_Taxon_Name, Scientific_Name:Trophic_Category) %>%
  distinct()

unique_surveys <- df %>%
  ungroup() %>%
  select(Site_Year_Code, Sampled_households:Est_Comm_Population) %>%
  distinct()

df_common_taxa_percents <- df %>%
  select(Site_Year_Code:Scientific_Name, Sampled_households:Estimated_Amount_Harvested) %>%
  group_by(Site_Year_Code, Lowest_Common_Taxon_Name, Scientific_Name) %>%
  ##need to filter out NAs
  summarise_at(vars(Percent_Using, Percent_Attempting_to_Harvest, Percent_Harvesting, Percent_Receiving, Percent_Giving), list(mean = mean))


df_common_taxa_harvest <- df %>%
  select(Site_Year_Code:Lowest_Common_Taxon_Name, Scientific_Name, Sampled_households:Estimated_Amount_Harvested) %>%
  group_by(Site_Year_Code, Lowest_Common_Taxon_Name, Scientific_Name) %>%
  ##Need to filter out NAs
  summarise_at(vars(Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested, Estimated_Amount_Harvested), list(sum = sum))

df_common_taxa <- left_join(df_common_taxa_percents, df_common_taxa_harvest, by = c("Site_Year_Code", "Lowest_Common_Taxon_Name", "Scientific_Name")) %>%
  left_join(unique_trophic_common_taxa, by = c("Site_Year_Code", "Lowest_Common_Taxon_Name", "Scientific_Name")) %>%
  left_join(unique_surveys, by = "Site_Year_Code")

##something i think is off, there should be the same number of rows in the harvest sums as unique trophic/common taxa... 

##need to go back and double check, but for now just move on... 
write.csv(df_common_taxa, "data/intermediate_data/comparable_harvest_df.csv")
