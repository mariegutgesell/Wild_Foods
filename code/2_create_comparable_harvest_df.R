##Determining lowest common taxonomic unit across communities/across years 
##Create working dataframe of comparable taxa across years/communities 

library(tidyverse)
##import cleaned harvest data and trophic info
#setwd("~/Desktop/Wild Foods Repo/")
df_final <- read.csv("data/intermediate_data/harvest_data_clean.csv")

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


mean_func <- function(x){
  x1 <- x %>%
    select(Site_Year_Code:Lowest_Common_Taxon_Name, Scientific_Name, Percent_Using) %>%
    filter(!is.na(Percent_Using)) %>%
    group_by(Site_Year_Code, Lowest_Common_Taxon_Name, Scientific_Name) %>%
    summarise_at(vars(Percent_Using), list(Percent_Using_mean = mean))
  
  x2 <- x %>%
    select(Site_Year_Code:Lowest_Common_Taxon_Name, Scientific_Name, Percent_Attempting_to_Harvest) %>%
    filter(!is.na(Percent_Attempting_to_Harvest)) %>%
    group_by(Site_Year_Code, Lowest_Common_Taxon_Name, Scientific_Name) %>%
    summarise_at(vars(Percent_Attempting_to_Harvest), list(Percent_Attempting_to_Harvest_mean = mean))
  
  x3 <- x %>%
    select(Site_Year_Code:Lowest_Common_Taxon_Name, Scientific_Name, Percent_Harvesting) %>%
    filter(!is.na(Percent_Harvesting)) %>%
    group_by(Site_Year_Code, Lowest_Common_Taxon_Name, Scientific_Name) %>%
    summarise_at(vars(Percent_Harvesting), list(Percent_Harvesting_mean = mean))
  
  x4 <- x %>%
    select(Site_Year_Code:Lowest_Common_Taxon_Name, Scientific_Name, Percent_Receiving) %>%
    filter(!is.na(Percent_Receiving)) %>%
    group_by(Site_Year_Code, Lowest_Common_Taxon_Name, Scientific_Name) %>%
    summarise_at(vars(Percent_Receiving), list(Percent_Receiving_mean = mean))
  
  x5 <- x %>%
    select(Site_Year_Code:Lowest_Common_Taxon_Name, Scientific_Name, Percent_Giving) %>%
    filter(!is.na(Percent_Giving)) %>%
    group_by(Site_Year_Code, Lowest_Common_Taxon_Name, Scientific_Name) %>%
    summarise_at(vars(Percent_Giving), list(Percent_Giving_mean = mean))
  
  x_final <- full_join(x1, x2, by = c("Site_Year_Code", "Lowest_Common_Taxon_Name", "Scientific_Name")) %>%
    full_join(x3, by = c("Site_Year_Code", "Lowest_Common_Taxon_Name", "Scientific_Name")) %>%
    full_join(x4, by = c("Site_Year_Code", "Lowest_Common_Taxon_Name", "Scientific_Name")) %>%
    full_join(x5, by = c("Site_Year_Code", "Lowest_Common_Taxon_Name", "Scientific_Name"))
}

df_mean <- mean_func(df)


sum_func <- function(x){
  x1 <- x %>%
    select(Site_Year_Code:Lowest_Common_Taxon_Name, Scientific_Name, Reported_Pounds_Harvested) %>%
    filter(!is.na(Reported_Pounds_Harvested)) %>%
    group_by(Site_Year_Code, Lowest_Common_Taxon_Name, Scientific_Name) %>%
    summarise_at(vars(Reported_Pounds_Harvested), list(Reported_Pounds_Harvested_sum = sum))
  
  x2 <- x %>%
    select(Site_Year_Code:Lowest_Common_Taxon_Name, Scientific_Name, Estimated_Total_Pounds_Harvested) %>%
    filter(!is.na(Estimated_Total_Pounds_Harvested)) %>%
    group_by(Site_Year_Code, Lowest_Common_Taxon_Name, Scientific_Name) %>%
    summarise_at(vars(Estimated_Total_Pounds_Harvested), list(Estimated_Total_Pounds_Harvested_sum = sum))
  
  x3 <- x %>%
    select(Site_Year_Code:Lowest_Common_Taxon_Name, Scientific_Name, Percapita_Pounds_Harvested) %>%
    filter(!is.na(Percapita_Pounds_Harvested)) %>%
    group_by(Site_Year_Code, Lowest_Common_Taxon_Name, Scientific_Name) %>%
    summarise_at(vars(Percapita_Pounds_Harvested), list(Percapita_Pounds_Harvested_sum = sum))
  
  x4 <- x %>%
    select(Site_Year_Code:Lowest_Common_Taxon_Name, Scientific_Name, Number_Of_Resource_Harvested) %>%
    filter(!is.na(Number_Of_Resource_Harvested)) %>%
    group_by(Site_Year_Code, Lowest_Common_Taxon_Name, Scientific_Name) %>%
    summarise_at(vars(Number_Of_Resource_Harvested), list(Number_Of_Resource_Harvested_sum = sum))
  
  x5 <- x %>%
    select(Site_Year_Code:Lowest_Common_Taxon_Name, Scientific_Name, Estimated_Amount_Harvested) %>%
    filter(!is.na(Estimated_Amount_Harvested)) %>%
    group_by(Site_Year_Code, Lowest_Common_Taxon_Name, Scientific_Name) %>%
    summarise_at(vars(Estimated_Amount_Harvested), list(Estimated_Amount_Harvested_sum = sum))
  
  x_final <- full_join(x1, x2, by = c("Site_Year_Code", "Lowest_Common_Taxon_Name", "Scientific_Name")) %>%
    full_join(x3, by = c("Site_Year_Code", "Lowest_Common_Taxon_Name", "Scientific_Name")) %>%
    full_join(x4, by = c("Site_Year_Code", "Lowest_Common_Taxon_Name", "Scientific_Name")) %>%
    full_join(x5, by = c("Site_Year_Code", "Lowest_Common_Taxon_Name", "Scientific_Name"))
}

df_sum <- sum_func(df)



##Join all data together

df_common_taxa <- left_join(df_mean, df_sum, by = c("Site_Year_Code", "Lowest_Common_Taxon_Name", "Scientific_Name")) %>%
  left_join(unique_trophic_common_taxa, by = c("Site_Year_Code", "Lowest_Common_Taxon_Name", "Scientific_Name")) %>%
  left_join(unique_surveys, by = "Site_Year_Code") %>%
  select(Site_Year_Code, Lowest_Common_Taxon_Name, Scientific_Name, Habitat:Est_Comm_Population, Percent_Using_mean:Estimated_Amount_Harvested_sum)

##something i think is off, there should be the same number of rows in the harvest sums as unique trophic/common taxa... 

##need to go back and double check, but for now just move on... 
write.csv(df_common_taxa, "data/intermediate_data/comparable_harvest_df.csv")

