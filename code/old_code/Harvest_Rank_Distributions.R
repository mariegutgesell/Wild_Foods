##Harvest Proportion Rank Distribution

library(ggplot2)
library(tidyverse)
library(ggridges)

##import cleaned harvest data and trophic info that is comparable across all years
df <- read.csv("data/intermediate_data/comparable_harvest_df.csv")

ggplot(df, aes(x = Percapita_Pounds_Harvested_sum, y = after_stat(density), group = Site_Year_Code)) +
  geom_histogram()+
  geom_density() +
  facet_wrap(~Site_Year_Code, scale = "free")


test <- df %>%
  filter(Lowest_Common_Taxon_Name == "Walrus")
##Normalize percapita harvest and total harvest 
total_harvest <- df %>%
  select(Site_Year_Code, Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum) %>%
  filter(Percapita_Pounds_Harvested_sum != 0) %>%
  group_by(Site_Year_Code) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum), list(total = sum))


df_2 <- df %>%
  left_join(total_harvest, by = "Site_Year_Code") %>%
  filter(!is.na(Estimated_Total_Pounds_Harvested_sum)) %>%
  filter(!is.na(Percapita_Pounds_Harvested_sum)) %>%
  mutate(Total_Harvest_prop = (Estimated_Total_Pounds_Harvested_sum/Estimated_Total_Pounds_Harvested_sum_total)*100) %>%
  mutate(Percapita_Harvest_prop = (Percapita_Pounds_Harvested_sum/Percapita_Pounds_Harvested_sum_total)*100) %>%
  mutate(Percapita_Harvest_prop_log = log(Percapita_Harvest_prop)) %>%
  mutate(Percapita_Harvest_prop_sqrt = sqrt(Percapita_Harvest_prop)) %>%
  mutate(Total_Harvest_prop_log = log(Total_Harvest_prop)) %>%
  mutate(Total_Harvest_prop_sqrt = sqrt(Total_Harvest_prop)) %>%
  group_by(Site_Year_Code) %>%
  mutate(Percapita_Harvest_prop_scale = scale(Percapita_Harvest_prop)) %>%
  mutate(Total_Harvest_prop_scale = scale(Total_Harvest_prop)) #%>%

##For each community/year, create rank # based on proportion of total harvest
##first for total harvest
total_harvest_rank <- df_2 %>%
  select(Site_Year_Code:Trophic_Category, Estimated_Total_Pounds_Harvested_sum, Total_Harvest_prop) %>%
  group_by(Site_Year_Code) %>%
  mutate(harvest_rank = rank(-Total_Harvest_prop, ties.method = "min")) #%>%
  #group_by(Site_Year_Code, harvest_rank) %>%
 # mutate(unique_identifier = letter()) %>%
  #mutate(harvest_rank = paste0(harvest_rank, "", unique_identifier)) %>%
  #select(-unique_identifier)
    
  

ggplot(total_harvest_rank, aes(x = harvest_rank, y= Total_Harvest_prop, fill = Habitat)) +
  geom_col() +
  scale_fill_manual(values = c("salmon", "darkblue", "brown", "darkgreen"))+
  facet_wrap(~Site_Year_Code, scale = "free")

ggplot(total_harvest_rank, aes(x = harvest_rank, y= Total_Harvest_prop, fill = Habitat)) +
  geom_col() +
  scale_fill_manual(values = c("salmon", "darkblue", "brown", "darkgreen"))+
  facet_wrap(~Site_Year_Code)


test <- total_harvest_rank %>%
  filter(Site_Year_Code == "Angoon_1984")

total_harvest_rank %>%
  filter(Site_Year_Code == "Angoon_2012") %>%
  ggplot(aes(x = harvest_rank, y = Total_Harvest_prop, fill = Habitat)) +
  geom_col() +
  scale_fill_manual(values = c("salmon", "darkblue", "brown", "darkgreen"))
  




##percapita harvest rank
percapita_harvest_rank <- df_2 %>%
  select(Site_Year_Code:Trophic_Category, Percapita_Pounds_Harvested_sum, Percapita_Harvest_prop) %>%
  group_by(Site_Year_Code) %>%
  mutate(harvest_rank = rank(-Percapita_Harvest_prop, ties.method = "min"))

ggplot(percapita_harvest_rank, aes(x = harvest_rank, y= Percapita_Harvest_prop, fill = Habitat)) +
  geom_col() +
  scale_fill_manual(values = c("salmon", "darkblue", "brown", "darkgreen"))+
  facet_wrap(~Site_Year_Code, scale = "free")


test <- percapita_harvest_rank %>%
  filter(Site_Year_Code == "Angoon_2012")

percapita_harvest_rank %>%
  filter(Site_Year_Code == "Angoon_2012") %>%
  ggplot(aes(x = harvest_rank, y = Percapita_Harvest_prop, fill = Habitat)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("salmon", "darkblue", "brown", "darkgreen"))

##Harvest Removal Experiment
results <- data.frame(
  community = character(),
  species_removed = integer(),
  total_harvest = numeric(),
  stringsAsFactors = FALSE
)
# Function to calculate total harvest after removing a particular species

calculate_total_harvest <- function(df, species_to_remove) {
  total_harvest <- sum(df$Total_Harvest_prop)
  removed_harvest <- df$Total_Harvest_prop[df$Lowest_Common_Taxon_Name %in% species_to_remove]
  remaining_harvest <- total_harvest - sum(removed_harvest)
  return(remaining_harvest)
}

# Iterate over each community
communities <- unique(total_harvest_rank$Site_Year_Code)

for (community in communities) {
  cat("Community:", community, "\n")
  
  # Extract harvest data for the current community
  community_data <- subset(total_harvest_rank, Site_Year_Code == community)
  
  # Sort the harvest data within the community from greatest to lowest harvest
  sorted_species <- unique(community_data$Lowest_Common_Taxon_Name[order(-community_data$Total_Harvest_prop)])
  
  # Initialize variables to store results for the current community
  species_removed <- 0
  remaining_harvest <- sum(community_data$Total_Harvest_prop)
  
  # Sequential removals of species and calculation of total harvest
  for (species in sorted_species) {
    remaining_harvest <- calculate_total_harvest(community_data, species)
    species_removed <- species_removed + 1
    # Store results for the current community
    results <- rbind(results, data.frame(community = community, species_removed = species_removed, total_harvest = remaining_harvest))
    community_data <- community_data[community_data$Lowest_Common_Taxon_Name != species, ]
  }
}



##Plot Species Removal experiment


ggplot(results, aes(x = species_removed, y = total_harvest, group = community)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  #theme_classic() +
  facet_wrap(~community, scale = "free")
  

##trying to plot harvest distribution and results from removal experiment on same plot
##join dataframes together

results <- results %>%
  rename(Site_Year_Code = "community", harvest_rank = "species_removed")

total_harvest_prop <- results %>%
  select(Site_Year_Code) %>%
  distinct() %>%
  mutate(harvest_rank = 0) %>%
  mutate(total_harvest = 100)

results_2 <- rbind(results, total_harvest_prop)


ggplot(results_2, aes(x = harvest_rank, y = total_harvest, group = Site_Year_Code)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  #theme_classic() +
  facet_wrap(~Site_Year_Code, scale = "free")

ggplot(results_2, aes(x = harvest_rank, y = total_harvest, group = Site_Year_Code, color = Site_Year_Code)) +
  geom_point() +
  geom_smooth( se = FALSE) #+
  #theme_classic() +
 # facet_wrap(~Site_Year_Code, scale = "free")

 
total_harvest_prop_2 <- total_harvest_prop %>%
  select(Site_Year_Code, harvest_rank) %>%
  mutate(Taxa_lvl1 = NA, Taxa_lvl2 = NA, Taxa_lvl3 = NA, Lowest_Common_Taxon_Name = NA, Scientific_Name = NA, Habitat = NA, Habitat_2 = NA, Trophic_Level = NA, Trophic_Category = NA, Estimated_Total_Pounds_Harvested_sum = NA, Total_Harvest_prop = 100) %>%
  select(Site_Year_Code, Taxa_lvl1:Total_Harvest_prop, harvest_rank)

total_harvest_rank_2 <- rbind(total_harvest_rank, total_harvest_prop_2)


total_harvest_rank_joined <- left_join(total_harvest_rank_2, results_2, by = c("Site_Year_Code", "harvest_rank"))

ggplot() +
  geom_col(data = total_harvest_rank_joined, aes(x = harvest_rank, y = Total_Harvest_prop, fill = Habitat)) +
  geom_smooth(data = total_harvest_rank_joined, aes(x = harvest_rank, y = total_harvest), method = "lm") +
  stat_smooth(data = total_harvest_rank_joined, aes(x = harvest_rank, y = total_harvest)) +
  xlim(0, 60)+
  facet_wrap(~Site_Year_Code) 

##partly looks so different because magnitude of harvest is so different...need to normalize this, also make sure removed via proportions


##Based on distribution of harvest proportions -- Whitestone 1996 predict would have faster rate of decline, Yakutat_1984 slower decline.. 
total_harvest_rank$harvest_rank <- as.numeric(total_harvest_rank$harvest_rank)
total_harvest_rank %>%
  filter(Site_Year_Code == "Whitestone_1996") %>%
  ggplot(aes(x = harvest_rank, y = Total_Harvest_prop, fill = Habitat)) +
  geom_col(color = "black") +
  geom_text(aes(label = Lowest_Common_Taxon_Name), hjust = -0.25, angle = 90, size = 5) + 
  scale_fill_manual(values = c("#FF9999", "#003366", "#CC9966", "#339933")) +
  ylim(0, 40) +
  ylab("Harvest Proportion") +
  xlab("Harvest Rank") +
    theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15))




results_2 %>%
  filter(Site_Year_Code == "Whitestone_1996") %>%
  ggplot(aes(x = harvest_rank, y = total_harvest)) +
  geom_point() +
  geom_smooth( se = FALSE) 
  

total_harvest_rank %>%
  filter(Site_Year_Code == "Yakutat_1984") %>%
  ggplot(aes(x = harvest_rank, y = Total_Harvest_prop, fill = Habitat)) +
  geom_col(color = "black") +
  geom_text(aes(label = Lowest_Common_Taxon_Name), hjust = -0.25, angle = 90, size = 4) + 
  scale_fill_manual(values = c("#FF9999", "#003366", "#CC9966", "#339933")) +
  ylim(0, 40) +
  ylab("Harvest Proportion") +
  xlab("Harvest Rank") +
  theme_classic()+
  theme(legend.position = "none", axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15))


results_2 %>%
  filter(Site_Year_Code == "Yakutat_1984") %>%
  ggplot(aes(x = harvest_rank, y = total_harvest)) +
  geom_point() +
  geom_smooth( se = FALSE) 

total_harvest_rank %>%
  filter(Site_Year_Code == "Port Protection_1987") %>%
  ggplot(aes(x = harvest_rank, y = Total_Harvest_prop, fill = Habitat)) +
  geom_col(color = "black") +
  geom_text(aes(label = Lowest_Common_Taxon_Name), hjust = -0.25, angle = 90, size = 3) + 
  scale_fill_manual(values = c("salmon", "darkblue", "brown", "darkgreen")) +
  ylim(0, 40)

results_2 %>%
  filter(Site_Year_Code == "Port Protection_1987") %>%
  ggplot(aes(x = harvest_rank, y = total_harvest)) +
  geom_point() +
  geom_smooth( se = FALSE) 

test <- results_2 %>%
  filter(Site_Year_Code %in% c("Yakutat_1984", "Whitestone_1996"))

ggplot(test, aes(x = harvest_rank, y = total_harvest, group = Site_Year_Code, color = Site_Year_Code)) +
  geom_point() +
  geom_line()+
 # geom_smooth( se = FALSE) +
  ylab("Total Harvest Proportion") +
  xlab("Number of Species Lost") +
  theme_classic()+
  theme(legend.position = "none", axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15))

  
##could also calculate like a rate of change per species loss, because even if these curves look similar, see much more rapid decline in whitestone