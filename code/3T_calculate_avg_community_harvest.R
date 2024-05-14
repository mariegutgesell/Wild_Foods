##Calculate average harvest across time for each community 

library(tidyverse)
library(vegan)
library(factoextra)

##import cleaned harvest data and trophic info that is comparable across all years
df <- read.csv("data/intermediate_data/tongass_comparable_harvest_df.csv") %>%
  filter(!Site_Year_Code %in% c("Hoonah_2016") )


##Calculate proportions, do transformations and normalize percapita harvest and total harvest 
df_comm_avg <- df %>%
  dplyr::select(Site_Year_Code, Habitat, Lowest_Common_Taxon_Name, Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum) %>% ##total harvest proportion and percapita harvest proportion are the same
   filter(!is.na(Estimated_Total_Pounds_Harvested_sum)) %>%
   filter(!is.na(Percapita_Pounds_Harvested_sum)) %>%
  separate(Site_Year_Code, c("Site", "Year"), sep = "_") %>%
  group_by(Site, Habitat, Lowest_Common_Taxon_Name) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum), list(avg = mean)) 

df_comm_avg_total <- df_comm_avg %>%
  group_by(Site) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum_avg, Percapita_Pounds_Harvested_sum_avg), list(total = sum))

df_comm_avg <- df_comm_avg %>%
   filter(Percapita_Pounds_Harvested_sum_avg != 0) %>%
  left_join(df_comm_avg_total, by = "Site") %>%
  mutate(Total_Harvest_prop = (Estimated_Total_Pounds_Harvested_sum_avg/Estimated_Total_Pounds_Harvested_sum_avg_total)*100) %>%
  mutate(Percapita_Harvest_prop = (Percapita_Pounds_Harvested_sum_avg/Percapita_Pounds_Harvested_sum_avg_total)*100) %>%
  mutate(Percapita_Harvest_prop_log = log(Percapita_Harvest_prop)) %>%
  mutate(Percapita_Harvest_prop_sqrt = sqrt(Percapita_Harvest_prop)) %>%
  mutate(Total_Harvest_prop_log = log(Total_Harvest_prop)) %>%
  mutate(Total_Harvest_prop_sqrt = sqrt(Total_Harvest_prop)) %>%
  group_by(Site) %>%
  mutate(Percapita_Harvest_prop_scale = scale(Percapita_Harvest_prop)) %>%
  mutate(Total_Harvest_prop_scale = scale(Total_Harvest_prop))
  
  
##To think about:
##removing 1987/1983 surveys before calculating average
##removing anything that is unknown before calculating average/proportions 
