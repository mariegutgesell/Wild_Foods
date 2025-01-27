##Calculate harvest diversity/coupling metrics for each site*year

library(tidyverse)
library(ggplot2)
library(ggrepel)
library(tibble)
library(vegan)

##source code that calculates average harvest across time for each community
source("code/Temporal_Community_Harvest/Tongass_Chugach/3CT_calculate_prop_harvest.R")
rm(list = ls()[!ls() %in% c("df_temp_avg")])


##Calculate richness, diversity, evenness of total harvest within communities across survey years
##calculate richness, sw diversity and evenness of harvest taxa
richness <- df_temp_avg %>%
  ungroup() %>%
  select(Forest, Site_Year_Code, Lowest_Common_Taxon_Name) %>%
  group_by(Forest, Site_Year_Code) %>%
  count() %>%
  rename(richness = "n")



df_temp_wide <- df_temp_avg %>%
  ungroup() %>%
  dplyr::select(Site_Year_Code, Lowest_Common_Taxon_Name, Total_Harvest_prop) %>%
  spread(key = Lowest_Common_Taxon_Name, value = Total_Harvest_prop)

df_temp_wide[is.na(df_temp_wide)] <- 0
df_temp_wide <- df_temp_wide %>%
  remove_rownames %>% 
  column_to_rownames(var="Site_Year_Code")
##select only numerical rows
df_temp_wide <- df_temp_wide %>%
  dplyr:: select(Abalone:Wolffish)


##SW diversity 
sw_div <- as.data.frame(diversity(df_temp_wide, index = "shannon"))
sw_div <- rownames_to_column(sw_div, "Site_Year_Code") %>%
  rename(sw_diversity = `diversity(df_temp_wide, index = "shannon")`)

head(sw_div)

temp_div <- left_join(richness, sw_div, by = "Site_Year_Code") 

##evenness
temp_div <- temp_div %>%
  mutate(evenness = sw_diversity/log(richness))

##calculate harvest habitat coupling metrics (diversity, evenness, SD) -----------
df_h <- df_temp_avg %>%
  #  select(Site_Year_Code, Habitat, Lowest_Common_Taxon_Name, Estimated_Total_Pounds_Harvested_sum:Total_Harvest_prop)
  group_by(Site_Year_Code, Habitat) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum, Total_Harvest_prop), list(total = sum))

test <- df_temp_avg %>%
  filter(Site_Year_Code == "Klukwan_1983")

##evenness based on proportion harvest (##habitat diversity/coupling values are the same for total harvest and prop harvest, so doesn't matter)
df_h_prop <- df_h %>%
  ungroup() %>%
  dplyr::select(Site_Year_Code, Habitat, Total_Harvest_prop_total) %>%
  spread(key = Habitat, value = Total_Harvest_prop_total)

df_h_prop[is.na(df_h_prop)] <- 0
df_h_prop <- df_h_prop %>%
  remove_rownames %>% 
  column_to_rownames(var="Site_Year_Code")
##select only numerical rows
df_h_prop <- df_h_prop %>%
  dplyr:: select(Freshwater_Anadromous:Terrestrial)


##SW diversity 
sw_div_prop <- as.data.frame(diversity(df_h_prop, index = "shannon"))
sw_div_prop <- rownames_to_column(sw_div_prop, "Site_Year_Code") %>%
  rename(sw_diversity = `diversity(df_h_prop, index = "shannon")`)

h_div_prop <- sw_div_prop %>%
  mutate(richness = 4) %>%
  mutate(evenness = sw_diversity/log(richness)) 

hist(h_div_prop$evenness)



##mean and sd of harvest proportion across habitats
h_df_mean <- df_h %>%
  select(Site_Year_Code, Habitat, Total_Harvest_prop_total) %>%
  group_by(Site_Year_Code) %>%
  summarise_at(vars(Total_Harvest_prop_total), list(avg = mean, sd = sd)) 


##combining mean, sd and diversity
h_df_all <- left_join(h_df_mean, h_div_prop, by = "Site_Year_Code") %>%
  mutate(evenness_log = log(evenness)) %>%
  mutate(sd_log = log(sd))



hist(h_df_all$sd)

hist(h_df_all$sd_log)

h_df_all <- h_df_all %>%
  dplyr::rename(sw_div_h = "sw_diversity", richness_h = "richness", evenness_h = "evenness", evenness_h_log = "evenness_log")


ggplot(h_df_all, aes(x = evenness_h, y = sd)) +
  geom_point() +
  theme_classic() +
  labs(x = "Habitat Eveness (Pielou)", y= "Habitat Eveness (SD of harvest proportion)")

ggplot(h_df_all, aes(x = evenness_h, y = sd_log)) +
  geom_point()



##Join habitat coupling metrics with harvest diversity metrics ------------
temp_div_all <- left_join(temp_div, h_df_all, by = "Site_Year_Code") %>%
  separate(Site_Year_Code, c("Site", "Year"), sep = "_", remove = FALSE)


##save csv
write.csv(temp_div_all, "data/intermediate_data/temporal_survey_harvest_diversity_metrics_proportion.csv")

##calculate mean richness, sw diversity and coupling across time 
temp_div_all_mean <- temp_div_all %>%
  group_by(Site) %>%
  summarise_at(vars(richness, sw_diversity, evenness, sd), list(mean = mean))


ggplot(temp_div_all, aes(x = Year, y = richness, group = Site, color = Site)) +
  geom_point() +
  geom_line() 

ggplot(temp_div_all, aes(x = Year, y = sw_diversity, group = Site, color = Site)) +
  geom_point() +
  geom_line() 

ggplot(temp_div_all, aes(x = Year, y = sd, group = Site, color = Site)) +
  geom_point() +
  geom_line() 

ggplot(temp_div_all, aes(x = Year, y = richness, group = Site, color = Site)) +
  geom_point() +
  geom_line() 


##looking at tatilek and chenega
temp_evos <- temp_div_all %>%
  filter(Site %in% c("Tatitlek", "Chenega")) %>%
  mutate(log_1_SD = log(1/sd))

ggplot(temp_evos, aes(x = Year, y = sw_diversity, group = Site, color = Site)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  ylab("Harvest Diversity (SW)")

ggplot(temp_evos, aes(x = Year, y = log_1_SD, group = Site, color = Site)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  ylab("Habitat Coupling")

