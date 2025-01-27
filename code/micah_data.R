##Pulling out some data/figs for Micah Hahn

library(tidyverse)
library(ggplot2)

df <- read.csv("data/intermediate_data/TC_comparable_harvest_df.csv") %>%
  select(Forest:Est_Comm_Population, Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum) %>%
  separate(Site_Year_Code, c("Site", "Year"), sep = "_", remove = FALSE) %>%
  filter(Percapita_Pounds_Harvested_sum != 0) %>%
  filter(Site %in% c("Cordova", "Haines", "Klukwan"))


df_hab <- df %>%
  group_by(Site, Year, Habitat) %>%
  summarise_at(vars(Percapita_Pounds_Harvested_sum), list(percapita_lbs_habitat = sum))
df_hab$Year <- as.numeric(df_hab$Year)
df_total <- df %>%
  group_by(Site, Year) %>%
  summarise_at(vars(Percapita_Pounds_Harvested_sum), list(percapita_lbs_total = sum))
df_total$Year <- as.numeric(df_total$Year)

decadal_example1_plot <- ggplot() +
  geom_line(data = df_hab, aes(x = Year, y = percapita_lbs_habitat, group = Habitat, color = Habitat), linewidth = 1) +
  geom_point(data = df_hab, aes(x = Year, y = percapita_lbs_habitat, group = Habitat, color = Habitat), size =2) +
  # geom_bar(data = df_example_1, aes(x = Year, y = percapita_kg, group = Habitat, fill = Habitat), stat = "identity") +
  scale_color_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  geom_line(data = df_total, aes(x = Year, y = percapita_lbs_total, group = Site), linewidth=1) +
  geom_point(data = df_total, aes(x = Year, y = percapita_lbs_total, group = Site), size = 2) +
  theme_classic() + 
  ylab("Total Percapita Harvest\n(lb/person)") +
  scale_x_continuous(breaks = seq(1983, 2015, by = 5))+
  #  ylim(0,610) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"), legend.position  = "none") +
  facet_wrap(~Site)
decadal_example1_plot


##Seasonal Harvest Patterns
simulated_harvest_df <- read.csv("data/intermediate_data/temporal_simulated_harvest_distributions_harvest_percapita.csv") %>%
  select(date:habitat) %>%
  separate(site, c("site", "year"), sep = "_", remove = FALSE) %>%
  filter(site %in% c("Cordova", "Haines", "Klukwan"))

simulated_harvest_df %>%
  filter(site == "Klukwan") %>%
  ggplot(aes(x = date, y = harvest_amount, group = habitat, color = habitat)) +
  geom_line() +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Date", y = "Harvest Amount (lb/person)") +
  theme_classic()+
  facet_wrap(~year)


simulated_harvest_df %>%
  filter(site == "Haines") %>%
  ggplot(aes(x = date, y = harvest_amount, group = habitat, color = habitat)) +
  geom_line() +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Date", y = "Harvest Amount (lb/person)") +
  theme_classic()+
  facet_wrap(~year)

simulated_harvest_df %>%
  filter(site == "Cordova") %>%
  ggplot(aes(x = date, y = harvest_amount, group = habitat, color = habitat)) +
  geom_line() +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Date", y = "Harvest Amount (lb/person)") +
  theme_classic()+
  facet_wrap(~year)

##Harvest richness 
sp_rich <- df %>%
  select(Site, Year, Lowest_Common_Taxon_Name) %>%
  group_by(Site, Year) %>%
  count()
