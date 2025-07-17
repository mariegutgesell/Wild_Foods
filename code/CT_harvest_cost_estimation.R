##Harvest Cost Estimation -- Tongass and Chugach
##March 7, 2025
##Marie Gutgesell 


library(readxl)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(data.table)
library(networkD3)

#setwd("/Users/mariegutgesell/Desktop/Wild Foods Repo/")
#getwd()
##Read in estimated population size data -- latest year I have is 2020 for all communities in Chugach and Tongass
pop_df <- read_excel("data/CSIS_Community_Demographics.xlsx", sheet = 1) %>%
  filter(Year == 2020) %>%
  dplyr::select(Community:Community_Population)



##Read in cleaned Tongass and Chugach data -- then will select most representative year
df <- read.csv("data/intermediate_data/TC_comparable_harvest_df.csv") %>% 
  dplyr::select(Forest:Est_Comm_Population, Percapita_Pounds_Harvested_sum) %>%
  separate(Site_Year_Code, into = c("Community", "Harvest_Survey_Year"), sep = "_", remove = FALSE)

sp_info <- read_excel("data/harvest_species_list_characteristics_5.xlsx")


test <- df 
##read in comprehensive survey demographics
survey_demographics <- read_excel("data/CSIS_SurveyData_Demographics.xlsx", sheet = 2) %>%
  unite(Site_Year_Code, c(Community, Year), sep = "_", remove = FALSE) 

##Determine the surveys from the most representative year, make sure have all surveys/communities want to use for this analysis, remove double years
latest_surveys <- survey_demographics %>%
  filter(Most_Rep_Year == "Yes") %>%
  filter(Site_Year_Code != "Beecher Pass_1987") %>% ##don't have a population estimate for 
  filter(Site_Year_Code != "Hoonah_2016") %>%
  filter(Site_Year_Code != "Yakutat_2000") %>%
  filter(Site_Year_Code != "Klukwan_1996") %>%
  rename(Harvest_Survey_Year = "Year") %>%
  left_join(pop_df, by = c("Community")) %>%
 dplyr::select(Site_Year_Code, Community, Year, Community_Population)


##note: don't have any 2020 population estimates for beecher pass, port san juan, or myers chuck
##reduce dataframe to only focus on years/sites using for this project 
df_2 <- df %>%
  dplyr::filter(Site_Year_Code %in% latest_surveys$Site_Year_Code) %>% ##selects only latest surveys
 # dplyr::filter(!grepl("Marine Mammals", Project_Name))%>%
  left_join(latest_surveys, by = c("Site_Year_Code", "Community")) %>%
  rename(Community_Population_2020 = "Community_Population")
df_2$Community_Population_2020 <- as.numeric(df_2$Community_Population_2020)
str(df_2)
##calculate total pounds and kgs harvested based on per capita harvest weights from representative year and 2020 population
df_3 <- df_2 %>%
  dplyr::select(Forest, Site_Year_Code, Community, Harvest_Survey_Year, Habitat, Lowest_Common_Taxon_Name, Percapita_Pounds_Harvested_sum, Community_Population_2020) %>%
  mutate(est_total_lbs_2020 = Percapita_Pounds_Harvested_sum*Community_Population_2020) %>% 
  mutate(est_total_kgs_2020 = est_total_lbs_2020*0.45359237) %>%
  filter(est_total_kgs_2020 != "0")

test <- df_3 %>%
  select(Community) %>%
  unique()
##43 communities, 3 w/o 2020  population size estimates 



##Read in cost estimate data
cost_df <- read_excel("data/TC_comparable_sp_list_cost_estimation.xlsx") %>%
  dplyr::select(Lowest_Common_Taxon_Name, Group, `Food Category`, Cost_per_lb)

df_4 <- left_join(df_3, cost_df, by = "Lowest_Common_Taxon_Name")


cost_est_2020 <- df_4 %>%
  mutate(total_cost_est_2020 = est_total_lbs_2020*Cost_per_lb,
   total_cost_est_2020_mil = total_cost_est_2020/1000000) %>%
    mutate(servings = est_total_kgs_2020/0.150)


total_cost_2020 <- cost_est_2020 %>%
  group_by(Forest) %>%
  summarise_at(vars(total_cost_est_2020), list(sum = sum))


total_pop_2020 <- cost_est_2020 %>%
  dplyr::select(Forest, Community, Community_Population_2020) %>%
  unique() %>%
  group_by(Forest) %>%
  summarise_at(vars(Community_Population_2020), list(total = sum))

percapita_cost <- left_join(total_cost_2020, total_pop_2020, by = "Forest") %>%
  mutate(percapita_cost_2020 = sum/total)
  total_cost_2020$sum/total_pop_2020$total

  
##total cost per community
  community_cost_est <- cost_est_2020 %>%
    group_by(Forest, Community, Harvest_Survey_Year, Community_Population_2020) %>%
    summarise_at(vars(est_total_kgs_2020, total_cost_est_2020, total_cost_est_2020_mil, servings), list(sum=sum)) %>%
    mutate(total_percapita_cost = total_cost_est_2020_sum/Community_Population_2020,
           percapita_servings = servings_sum/Community_Population_2020, 
           total_percapita_kg = est_total_kgs_2020_sum/Community_Population_2020) 
  
write.csv(cost_est_2020, "CT_2020_harvest_estimates_cost_servings.csv")
write.csv(community_cost_est, "CT_2020_harvest_estimates_cost_servings_community_sum.csv")
#1
#109,904,915 - total estimated cost of food replacement 2020  - based on grocery prices in juneau
cost_est_2020$Habitat <- ordered(cost_est_2020$Habitat,
                                 levels = c("Freshwater_Anadromous", "Marine", "Nearshore", "Terrestrial"))

cost_est_2020$Group <- ordered(cost_est_2020$Group, 
                               levels = c(
                                 "Salmon",
                                 "Char",
                                 "Trout",
                                 "Other Freshwater/Anadromous Fish",
                                 "Non-Halibut Fish",
                                 "Halibut",
                                 "Marine Invertebrates",
                                 "Marine Mammals",
                                 "Offshore Crab",
                                 "Nearshore Crab",
                                 "Mollusc",
                                 "Herring Roe",
                                 "Seaweed/Kelp",
                                 "Nearshore Invertebrates",
                                 "Berries",
                                 "Other Large Land Mammals",
                                 "Deer",
                                 "Birds/Eggs",
                                 "Plants/Greens",
                                 "Small Land Mammals"
                               ) )


cost_est_2020 %>%
  filter(total_cost_est_2020 != 0) %>%
  filter(Forest == "Tongass") %>%
  ggplot(aes(x = Habitat, y = total_cost_est_2020_mil, group = Group, fill = Group)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c(
    "salmon",
    "pink3",
    "pink4",
    "pink",
    "#0b559f",
    "#bad6eb",
    "#89bedc",
    "#2b7bba",
    "blue",
    "brown",
    "#fedfc0",
    "#fd8c3b",
    "#e95e0d",
    "#b63c02",
    "#e1f3dc",
    "#bce4b5",
    "#8ed08b",
    "#56b567",
    "#2c944c",
    "#05712f"
  )) +
  theme_classic() +
  ylab("Estimated Total Cost per year (millions)") +
theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), legend.text = element_text(size = 14))



cost_est_2020 %>%
  filter(total_cost_est_2020 != 0) %>%
  ggplot(aes(x = Forest, y = total_cost_est_2020_mil, fill = Forest)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("aquamarine", "darkgreen"))+
  theme_classic() +
  ylab("Estimated Total Cost per year (millions)") +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16), legend.position = "none")
  

##total cost per habitat in 2020 for tognass

habitat_total_cost_2020 <- cost_est_2020 %>%
  filter(Forest == "Tongass") %>%
  group_by(Habitat) %>%
  summarise_at(vars(total_cost_est_2020), list(sum))

species_total_cost_2020 <- cost_est_2020 %>%
  filter(Forest == "Tongass") %>%
  group_by(Habitat, Lowest_Common_Taxon_Name) %>%
  summarise_at(vars(total_cost_est_2020), list(sum))



##Creating Rank of Fish Species (for Zia)
fish_total_harvest <- df_3 %>%
  filter(Habitat %in% c("Freshwater_Anadromous", "Marine")) %>%
  group_by(Lowest_Common_Taxon_Name) %>%
  summarise_at(vars(est_total_kgs_2020), list(sum)) %>%
  filter(!Lowest_Common_Taxon_Name %in% c("Box Crab","King Crab", "Octopus", "Scallop", "Seal", "Shark", "Shrimp", "Skates", "Spawnouts", "Squid", "Stellar Sea Lion")) %>%
  filter(!grepl("Unknown", Lowest_Common_Taxon_Name))


fish_harvest_rank <- fish_total_harvest %>%
  mutate(harvest_rank = rank(-est_total_kgs_2020, ties.method = "min")) 


write.csv(fish_harvest_rank, "2020_fish_harvest_ranks.csv")

fish_rank_plot <- fish_harvest_rank %>%
  ggplot(aes(x = harvest_rank, y= est_total_kgs_2020, fill = "lightblue")) +
  geom_col() +
#  scale_fill_manual(values = clrs2)+
  theme_classic() +
  #  facet_wrap(~Site) +
  ylab("Estimated Total 2020 Harvest (kgs)") +
  xlab("Species") +
  theme(axis.text.x = element_text(size = 18),axis.text.y = element_text(size = 18),axis.title.y=element_text(size = 20),axis.title.x=element_text(size = 20),  text = element_text(family = "Avenir"), legend.position = "none")

fish_rank_plot
