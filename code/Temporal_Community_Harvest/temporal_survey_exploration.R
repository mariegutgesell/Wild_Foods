##Looking at temporal trends 

library(tidyverse)
library(readxl)
##import cleaned harvest data and trophic info that is comparable across all years
df <- read.csv("data/intermediate_data/tongass_harvest_data_clean.csv")
##
salmon <- df %>%
  filter(Taxa_lvl2 == "Salmon") %>%
  separate(Site_Year_Code, c(Community, Year), sep = "_", remove = FALSE) 

##look at percent using/% harvesting of salmon over time 
##all salmon and particular species 
##crab, salmon, deer 


##import harvest survey demographics, and select only communities w/ temporal replication
dem <- read_excel("data/CSIS_SurveyData_Demographics.xlsx", sheet = 2) %>%
  unite(Site_Year_Code, c(Community, Year), sep = "_", remove = FALSE) 

df <- df %>%
  left_join(dem, by = "Site_Year_Code") %>%
  filter(Temporal_Survey == "Y")


##Harvest species richness over time
sp_rich <- df %>%
  select(Community, Year, Habitat, Lowest_Common_Taxon_Name) %>%
  filter(!grepl("Unknown", Lowest_Common_Taxon_Name)) %>% ##remove unknown taxa from diversity calculations
  group_by(Community, Year) %>%
  count()

ggplot(sp_rich, aes(x = Year, y = n, group = Community, color = Community)) +
  geom_point() +
  geom_line() + 
  theme_classic() +
  ylab("Species Richness")+
  facet_wrap(~Community)

##slightly worried about the 1987 surveys, were they done on a smaller group of organisms? 

##Looking at trends in total harvest and proportion harvest across habitats through time 
total_harvest <- df %>%
  select(Community, Year, Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum) %>%
  filter(!is.na(Estimated_Total_Pounds_Harvested_sum)) %>%
  # filter(Percapita_Pounds_Harvested_sum != 0) %>%
  group_by(Community, Year) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum), list(total = sum))

habitat_harvest <- df %>%
  select(Community, Year, Habitat,Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum)%>%
  filter(!is.na(Estimated_Total_Pounds_Harvested_sum)) %>%
  group_by(Community, Year, Habitat) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum), list(habitat.total = sum))

df_2 <- habitat_harvest %>%
  left_join(total_harvest, by = c("Community", "Year")) %>%
  filter(!is.na(Estimated_Total_Pounds_Harvested_sum_habitat.total)) %>%
  mutate(Total_Harvest_prop = (Estimated_Total_Pounds_Harvested_sum_habitat.total/Estimated_Total_Pounds_Harvested_sum_total)*100) %>%
  mutate(Percapita_Harvest_prop = (Percapita_Pounds_Harvested_sum_habitat.total/Percapita_Pounds_Harvested_sum_total)*100)

ggplot(df_2, aes(x = Year, y = Estimated_Total_Pounds_Harvested_sum_total, group = Community, color = Community)) +
  geom_point() +
  geom_line() + 
  theme_classic() +
  ylab("Estimated Total Harvest (lbs)")+
  facet_wrap(~Community)

ggplot(df_2, aes(x = Year, y = Percapita_Pounds_Harvested_sum_total, group = Community, color = Community)) +
  geom_point() +
  geom_line() + 
  theme_classic() +
  ylab("Percapita Harvest (lbs)")+
  facet_wrap(~Community)

ggplot(df_2, aes(x = Estimated_Total_Pounds_Harvested_sum_total, y= Percapita_Pounds_Harvested_sum_total)) +
  geom_point() +
  theme_classic()


##Habitat proportion Plot
df_2$Habitat <- ordered(df_2$Habitat, 
                        levels = c("Marine", "Nearshore", "Freshwater_Anadromous", "Terrestrial"))

ggplot(df_2, aes(x = Year, y = Percapita_Harvest_prop, color = Habitat)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("#003366","#CC9966", "#FF9999","#339933"))+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),axis.text.y = element_text(size = 10),axis.title.y=element_text(size = 12), axis.title.x = element_blank(),  text = element_text(family = "Times New Roman"), strip.background = element_blank()) +
  facet_wrap(~Community)


ggplot(df_2, aes(x = Year, y = Percapita_Pounds_Harvested_sum_habitat.total, color = Habitat)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("#003366","#CC9966", "#FF9999","#339933"))+
  theme_classic() +
  xlab("Percapita Harvested (lbs)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),axis.text.y = element_text(size = 10),axis.title.y=element_text(size = 12), axis.title.x = element_blank(),  text = element_text(family = "Times New Roman"), strip.background = element_blank()) +
  facet_wrap(~Community)


