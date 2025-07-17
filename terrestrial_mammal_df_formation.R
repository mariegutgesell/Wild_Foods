##code to get terrestrial harvest data for Lizzy



library(tidyverse)
library(readxl)

c <- read.csv("data/intermediate_data/chugach_harvest_data_clean.csv") %>%
  select(Site_Year_Code:Percent_Giving, Estimated_Total_Pounds_Harvested, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested) %>%
  mutate(Forest = "Chugach")
t <- read.csv("data/intermediate_data/tongass_harvest_data_clean.csv") %>%
  select(Site_Year_Code:Percent_Giving, Estimated_Total_Pounds_Harvested, Percapita_Pounds_Harvested, Number_Of_Resource_Harvested) %>%
  mutate(Forest = "Tongass")

df <- rbind(c, t)

df_terr <- df %>%
  separate(Site_Year_Code, into = c("Community", "Year"), sep = "_", remove = FALSE) %>%
  filter(Taxa_lvl1 == "Land Mammals") %>%
  filter(!if_all(Estimated_Total_Pounds_Harvested:Number_Of_Resource_Harvested, ~ .x == 0)) %>%
  select(Forest, Community, Year, Site_Year_Code, Taxa_lvl1, Taxa_lvl2, Taxa_lvl5, Sampled_households:Number_Of_Resource_Harvested)



large_land_mammals <- df_terr %>%
  filter(Taxa_lvl2 == "Large Land Mammals") %>%
  select(Taxa_lvl2, Taxa_lvl5) %>%
  distinct()

small_land_mammals <- df_terr %>%
  filter(Taxa_lvl2 == "Small Land Mammals") %>%
  select(Taxa_lvl2, Taxa_lvl5) %>%
  distinct()

#write.csv(df_terr, "data/terrestrial_mammal_data.csv")


##Look at total amounts in latest year, checking for amount of muskox/bison 
pop_df <- read_excel("data/CSIS_Community_Demographics.xlsx", sheet = 1) %>%
  filter(Year == 2020) %>%
  select(Community:Community_Population)

##read in comprehensive survey demographics
survey_demographics <- read_excel("data/CSIS_SurveyData_Demographics.xlsx", sheet = 2) %>%
  unite(Site_Year_Code, c(Community, Year), sep = "_", remove = FALSE) 

##Determine the surveys from the most representative year, make sure have all surveys/communities want to use for this analysis, remove double years
latest_surveys <- survey_demographics %>%
  filter(Most_Rep_Year == "Yes") %>%
  filter(Site_Year_Code != "Beecher Pass_1987") %>%
  filter(Site_Year_Code != "Hoonah_2016") %>%
  filter(Site_Year_Code != "Yakutat_2000") %>%
  filter(Site_Year_Code != "Klukwan_1996") %>%
  rename(Harvest_Survey_Year = "Year") %>%
  left_join(pop_df, by = c("Community")) %>%
  select(Site_Year_Code, Community, Year, Community_Population)

##reduce dataframe to only focus on years/sites using for this project 
df_2 <- df_terr %>%
  dplyr::filter(Site_Year_Code %in% latest_surveys$Site_Year_Code) %>% ##selects only years where a comprehensive survey was done
#  dplyr::filter(!grepl("Marine Mammals", Project_Name))%>%
  left_join(latest_surveys, by = c("Site_Year_Code", "Community")) %>%
  rename(Community_Population_2020 = "Community_Population")
df_2$Community_Population_2020 <- as.numeric(df_2$Community_Population_2020)
str(df_2)



large_land_mammals_sum <- df_2 %>%
  filter(Taxa_lvl2 == "Large Land Mammals") %>%
  group_by(Taxa_lvl5) %>%
  filter(!is.na(Percapita_Pounds_Harvested)) %>%
  select(Forest, Site_Year_Code, Taxa_lvl1:Taxa_lvl5, Percapita_Pounds_Harvested, Community_Population_2020) %>%
  mutate(est_total_lbs_2020 = Percapita_Pounds_Harvested*Community_Population_2020) %>% 
  mutate(est_total_kgs_2020 = est_total_lbs_2020*0.45359237) %>%
  filter(est_total_kgs_2020 != "0")
  
  

total_large_land_mammals <- large_land_mammals_sum %>%
  ungroup() %>%
  filter(Taxa_lvl5 != "Muskox") %>%
  summarise_at(vars(est_total_kgs_2020), list(sum)) %>%
  mutate(est_total_mt_2020 = est_total_kgs_2020/1000)





##total 2020 population: 
#35915