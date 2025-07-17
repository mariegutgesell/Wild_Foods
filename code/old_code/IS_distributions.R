##Energy flux (interaction strength proxy) distributions
library(ggplot2)
library(tidyverse)
library(ggridges)

##import cleaned harvest data and trophic info that is comparable across all years
df <- read.csv("data/intermediate_data/comparable_harvest_df.csv")

ggplot(df, aes(x = Percapita_Pounds_Harvested_sum, y = after_stat(density), group = Site_Year_Code)) +
  geom_histogram()+
  geom_density() +
  facet_wrap(~Site_Year_Code, scale = "free")


##Normalize percapita harvest and total harvest 
total_harvest <- df %>%
  select(Site_Year_Code, Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum) %>%
  filter(Percapita_Pounds_Harvested_sum != 0) %>%
  group_by(Site_Year_Code) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum), list(total = sum))


df_2 <- df %>%
  left_join(total_harvest, by = "Site_Year_Code") %>%
  mutate(Total_Harvest_prop = (Estimated_Total_Pounds_Harvested_sum/Estimated_Total_Pounds_Harvested_sum_total)*100) %>%
  mutate(Percapita_Harvest_prop = (Percapita_Pounds_Harvested_sum/Percapita_Pounds_Harvested_sum_total)*100) %>%
  mutate(Percapita_Harvest_prop_log = log(Percapita_Harvest_prop)) %>%
  mutate(Percapita_Harvest_prop_sqrt = sqrt(Percapita_Harvest_prop)) %>%
  mutate(Total_Harvest_prop_log = log(Total_Harvest_prop)) %>%
  mutate(Total_Harvest_prop_sqrt = sqrt(Total_Harvest_prop)) %>%
  group_by(Site_Year_Code) %>%
  mutate(Percapita_Harvest_prop_scale = scale(Percapita_Harvest_prop)) %>%
  mutate(Total_Harvest_prop_scale = scale(Total_Harvest_prop)) #%>%
#filter(!grepl("Unknown", Lowest_Common_Taxon_Name))

ggplot(df_2, aes(x = Percapita_Harvest_prop, y = after_stat(density), group = Site_Year_Code, color = Site_Year_Code)) +
 # geom_histogram(bins = 50) +
  geom_density() +
  facet_wrap(~Site_Year_Code) +
  geom_vline(aes(xintercept = mean(Percapita_Harvest_prop)), 
             linetype = "dashed", linewidth = 0.6,
             color = "#FC4E07")


ggplot(df_2, aes(x = Percapita_Harvest_prop, group = Site_Year_Code, color = Site_Year_Code)) +
  geom_freqpoly(bins = 30) +
  #facet_wrap(~Site_Year_Code) +
  theme(legend.position = "none") +
  xlim(0, 60)

 ggplot(df_2, aes(x = Percapita_Pounds_Harvested_sum, group = Site_Year_Code, color = Site_Year_Code)) +
  geom_freqpoly(bins = 15) +
  facet_wrap(~Site_Year_Code) +
  theme(legend.position = "none") +
  xlim(0, 200) 

ggplot(df_2, aes(x = Percapita_Harvest_prop, group = Habitat, color = Habitat)) +
  geom_freqpoly(bins = 15) +
 facet_wrap(~Site_Year_Code) +
 theme(legend.position = "none") +
  xlim(0, 100) 


ggplot(df_2, aes(y = Percapita_Pounds_Harvested_sum, x = Site_Year_Code, color = Site_Year_Code)) +
  geom_boxplot() +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(size = 12, angle = 45, hjust = 1))



ggplot(df_2, aes(y = Percapita_Harvest_prop_log, x = Site_Year_Code, color = Site_Year_Code)) +
  geom_boxplot() +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(size = 12, angle = 45, hjust = 1))


med <- df_2 %>%
  filter(!is.na(Percapita_Pounds_Harvested_sum)) %>%
  group_by(Site_Year_Code) %>%
  summarise(mean_percapita = mean(Percapita_Pounds_Harvested_sum),
            median_percapita = median(Percapita_Pounds_Harvested_sum))

ggplot(med, aes(x = Site_Year_Code, y = median_percapita)) +
  geom_point() +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(size = 12, angle = 45, hjust = 1))

ggplot(med, aes(x = Site_Year_Code, y = mean_percapita)) +
  geom_point() +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(size = 12, angle = 45, hjust = 1))


ggplot(df_2, aes(x = Percapita_Pounds_Harvested_sum, y = Site_Year_Code)) +
  geom_density_ridges(aes(fill = Site_Year_Code), scale = 1.2) +
  theme(legend.position = "none") +
  xlim(0,80)

library(ggpubr)
gghistogram(df_2, x = "Percapita_Pounds_Harvested_sum", bins = 15, add = "mean") +
  facet_wrap(~Site_Year_Code)


