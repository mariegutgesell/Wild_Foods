##Food Web Interaction Matrix and Visualization

library(readxl)

##import cleaned harvest data and trophic info
setwd("~/Desktop/Wild Foods Repo/")
source("code/1_dataframe_formation.R")
rm(list = ls()[!ls() %in% c("df_final")])
trophic_df <- read_excel("data/harvest_species_list_characteristics_5.xlsx")

##remove unknown and filter to only species harvested in harvest data, select only columns needed for food web interaction, then join trophic data
##note: in future will likely recalculate the harvest metrics, but lets not worry about that now -- also still need to fix those where conversion unit is 0 but did harvest resources
harvest_df <- df_final %>%
  select(Site_Year_Code, Taxa_lvl1, Taxa_lvl2, Taxa_lvl3, Taxa_lvl4, Taxa_lvl5, Percapita_Pounds_Harvested, Estimated_Total_Pounds_Harvested) %>%
  filter(!if_all(Percapita_Pounds_Harvested:Estimated_Total_Pounds_Harvested, ~ .x == 0))
  
harvest_df$Taxa_lvl5 <- str_remove(harvest_df$Taxa_lvl5, "Unknown ")
harvest_df$Taxa_lvl4 <- str_remove(harvest_df$Taxa_lvl4, "Unknown ")

df <- harvest_df %>%
  left_join(trophic_df %>% select(Taxa_lvl4, Taxa_lvl5, Scientific_Name, Lowest_Taxonomic_Resolution, Habitat, Trophic_Level, Trophic_Category), by = c("Taxa_lvl4", "Taxa_lvl5")) %>%
  select(Site_Year_Code, Taxa_lvl1, Taxa_lvl2, Taxa_lvl3, Taxa_lvl4, Taxa_lvl5, Scientific_Name, Lowest_Taxonomic_Resolution, Habitat, Trophic_Level, Trophic_Category,Percapita_Pounds_Harvested, Estimated_Total_Pounds_Harvested)

str(df)
##for each community-year want to sum by habitat, and then trophic category
df_sum <- df %>%
  group_by(Site_Year_Code, Habitat, Trophic_Level) %>%
  summarise(across(where(is.numeric), sum))


##how are there 0's for percapita harvest but estimated total harvest is not 0? 

##Plotting frequency of interactions
hist <- ggplot(df, aes(x = Percapita_Pounds_Harvested)) +
  geom_histogram() +
  facet_wrap(~Site_Year_Code) 
hist

##working out how to plot food web -- start w/ 1 community-year
angoon_2012 <- df_sum %>%
  filter(Site_Year_Code == "Angoon_2012")

library(cheddar)
data(TL86)
PlotWebByLevel(TL86)
