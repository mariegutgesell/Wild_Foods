##Join tongass and chugach data together and create comparable harvest 


library(tidyverse)
library(readxl)

#setwd("~/Desktop/Wild Foods Repo/")

##import cleaned harvest data and join tongass and chugach together

df_t <- read.csv("data/intermediate_data/tongass_harvest_data_clean.csv")%>% ##csv from 1T_dataframe_formation.R
  filter(!if_all(Percent_Using:Mean_Grams_Percapita_Harvest, ~ .x == 0)) %>%##remove rows where all values are 0 (nothing was harvested or shared in community)
  mutate(Forest = "Tongass")

df_c <- read.csv("data/intermediate_data/chugach_harvest_data_clean.csv") %>% ##csv from 1C_dataframe_formation.R
  filter(!if_all(Percent_Using:Mean_Grams_Percapita_Harvest, ~ .x == 0)) %>%
  mutate(Forest = "Chugach")


harvest_df <- rbind(df_t, df_c) %>%
  dplyr::select(Forest, Project_Name, Site_Year_Code, Taxa_lvl1, Taxa_lvl2, Taxa_lvl3, Taxa_lvl4, Taxa_lvl5, Resource_Code, Resource_Name, Sampled_households:Most_Rep_Year, Conversion_Units_To_Pounds, Resource_Harvest_Units, Percent_Using:Percapita_Pounds_Harvested, Number_Of_Resource_Harvested:Mean_Grams_Percapita_Harvest) %>%
  filter(!if_all(Percent_Using:Mean_Grams_Percapita_Harvest, ~ .x == 0)) ##remove rows where all values are 0 (nothing was harvested or shared in community)


##import harvest characteristic data

trophic_df <- read_excel("data/harvest_species_list_characteristics_5.xlsx", sheet = 4)


##join trophic info for the lowest common taxa across all years and communities
df <- harvest_df %>%
  left_join(trophic_df %>% select(Taxa_lvl1:Lowest_Common_Taxon_lvl, Scientific_Name, Habitat, Habitat_2, Trophic_Level, Trophic_Category), by = c("Taxa_lvl1", "Taxa_lvl2", "Taxa_lvl3", "Taxa_lvl4", "Taxa_lvl5")) %>%
  select(Forest, Site_Year_Code:Taxa_lvl5, Lowest_Common_Taxon_Name:Trophic_Category, Sampled_households:Estimated_Amount_Harvested) %>%
  filter(!Lowest_Common_Taxon_Name %in% c("Not_comparable", "NA"))

##need to figure out what species eel is 


##calculate harvest metrics for the lowest common taxon, to be used for harvest analysis moving forward
unique_trophic_common_taxa <- df %>%
  ungroup() %>%
  select(Forest, Site_Year_Code,Taxa_lvl1, Taxa_lvl2, Lowest_Common_Taxon_Name, Habitat:Trophic_Category) %>%
  distinct()

unique_surveys <- df %>%
  ungroup() %>%
  select(Forest, Site_Year_Code, Sampled_households:Est_Comm_Population) %>%
  distinct()

##For percents, am taking the average over time
mean_func <- function(x){
  x1 <- x %>%
    select(Forest, Site_Year_Code:Lowest_Common_Taxon_Name, Scientific_Name, Percent_Using) %>%
    filter(!is.na(Percent_Using)) %>%
    group_by(Forest, Site_Year_Code,  Lowest_Common_Taxon_Name) %>%
    summarise_at(vars(Percent_Using), list(Percent_Using_mean = mean))
  
  x2 <- x %>%
    select(Forest, Site_Year_Code:Lowest_Common_Taxon_Name, Scientific_Name, Percent_Attempting_to_Harvest) %>%
    filter(!is.na(Percent_Attempting_to_Harvest)) %>%
    group_by(Forest, Site_Year_Code, Lowest_Common_Taxon_Name) %>%
    summarise_at(vars(Percent_Attempting_to_Harvest), list(Percent_Attempting_to_Harvest_mean = mean))
  
  x3 <- x %>%
    select(Forest, Site_Year_Code:Lowest_Common_Taxon_Name, Scientific_Name, Percent_Harvesting) %>%
    filter(!is.na(Percent_Harvesting)) %>%
    group_by(Forest, Site_Year_Code, Lowest_Common_Taxon_Name) %>%
    summarise_at(vars(Percent_Harvesting), list(Percent_Harvesting_mean = mean))
  
  x4 <- x %>%
    select(Forest, Site_Year_Code:Lowest_Common_Taxon_Name, Scientific_Name, Percent_Receiving) %>%
    filter(!is.na(Percent_Receiving)) %>%
    group_by(Forest, Site_Year_Code, Lowest_Common_Taxon_Name) %>%
    summarise_at(vars(Percent_Receiving), list(Percent_Receiving_mean = mean))
  
  x5 <- x %>%
    select(Forest, Site_Year_Code:Lowest_Common_Taxon_Name, Scientific_Name, Percent_Giving) %>%
    filter(!is.na(Percent_Giving)) %>%
    group_by(Forest, Site_Year_Code, Lowest_Common_Taxon_Name) %>%
    summarise_at(vars(Percent_Giving), list(Percent_Giving_mean = mean))
  
  x_final <- full_join(x1, x2, by = c("Forest", "Site_Year_Code", "Lowest_Common_Taxon_Name")) %>%
    full_join(x3, by = c("Forest", "Site_Year_Code", "Lowest_Common_Taxon_Name")) %>%
    full_join(x4, by = c("Forest", "Site_Year_Code", "Lowest_Common_Taxon_Name")) %>%
    full_join(x5, by = c("Forest", "Site_Year_Code", "Lowest_Common_Taxon_Name"))
}

df_mean <- mean_func(df)

##for Reported_Pounds_Harvested, Estimated_Total_Pounds_Harvested, Percapita_Pounds_Harvested, Number_of_Resource_Harvested, Estimated_Amount_Harvested want to sum by 
sum_func <- function(x){
  x1 <- x %>%
    select(Forest, Site_Year_Code:Lowest_Common_Taxon_Name, Scientific_Name, Reported_Pounds_Harvested) %>%
    filter(!is.na(Reported_Pounds_Harvested)) %>%
    group_by(Forest, Site_Year_Code, Lowest_Common_Taxon_Name) %>%
    summarise_at(vars(Reported_Pounds_Harvested), list(Reported_Pounds_Harvested_sum = sum))
  
  x2 <- x %>%
    select(Forest, Site_Year_Code:Lowest_Common_Taxon_Name, Scientific_Name, Estimated_Total_Pounds_Harvested) %>%
    filter(!is.na(Estimated_Total_Pounds_Harvested)) %>%
    group_by(Forest, Site_Year_Code, Lowest_Common_Taxon_Name) %>%
    summarise_at(vars(Estimated_Total_Pounds_Harvested), list(Estimated_Total_Pounds_Harvested_sum = sum))
  
  x3 <- x %>%
    select(Forest, Site_Year_Code:Lowest_Common_Taxon_Name, Scientific_Name, Percapita_Pounds_Harvested) %>%
    filter(!is.na(Percapita_Pounds_Harvested)) %>%
    group_by(Forest, Site_Year_Code, Lowest_Common_Taxon_Name) %>%
    summarise_at(vars(Percapita_Pounds_Harvested), list(Percapita_Pounds_Harvested_sum = sum))
  
  x4 <- x %>%
    select(Forest, Site_Year_Code:Lowest_Common_Taxon_Name, Scientific_Name, Number_Of_Resource_Harvested) %>%
    filter(!is.na(Number_Of_Resource_Harvested)) %>%
    group_by(Forest, Site_Year_Code, Lowest_Common_Taxon_Name) %>%
    summarise_at(vars(Number_Of_Resource_Harvested), list(Number_Of_Resource_Harvested_sum = sum))
  
  x5 <- x %>%
    select(Forest, Site_Year_Code:Lowest_Common_Taxon_Name, Scientific_Name, Estimated_Amount_Harvested) %>%
    filter(!is.na(Estimated_Amount_Harvested)) %>%
    group_by(Forest, Site_Year_Code, Lowest_Common_Taxon_Name) %>%
    summarise_at(vars(Estimated_Amount_Harvested), list(Estimated_Amount_Harvested_sum = sum))
  
  x_final <- full_join(x1, x2, by = c("Forest", "Site_Year_Code", "Lowest_Common_Taxon_Name")) %>%
    full_join(x3, by = c("Forest", "Site_Year_Code", "Lowest_Common_Taxon_Name")) %>%
    full_join(x4, by = c("Forest", "Site_Year_Code", "Lowest_Common_Taxon_Name")) %>%
    full_join(x5, by = c("Forest", "Site_Year_Code", "Lowest_Common_Taxon_Name"))
}

df_sum <- sum_func(df)

##Join all data together

df_common_taxa <- left_join(df_mean, df_sum, by = c("Forest", "Site_Year_Code", "Lowest_Common_Taxon_Name")) %>%
  left_join(unique_trophic_common_taxa, by = c("Forest" , "Site_Year_Code", "Lowest_Common_Taxon_Name")) %>%
  left_join(unique_surveys, by =  c("Forest", "Site_Year_Code")) %>%
  select(Forest, Site_Year_Code, Lowest_Common_Taxon_Name, Habitat:Est_Comm_Population, Percent_Using_mean:Estimated_Amount_Harvested_sum)


write.csv(df_common_taxa, "data/intermediate_data/TC_comparable_harvest_df.csv")

test <- df_common_taxa %>%
  filter(Site_Year_Code == "Port San Juan (Evans Island)_1984")

