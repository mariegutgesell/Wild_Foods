##Calculate harvest diversity metrics 

library(tidyverse)
library(ggplot2)
library(ggrepel)
library(tibble)
library(caret)

##source code that calculates average harvest across time for each community
source("code/Average_Community_Harvest/Tongass_Chugach/3CT_calculate_avg_community_harvest.R")
rm(list = ls()[!ls() %in% c("df_comm_avg")])


df_comm_avg <- df_comm_avg %>%
  dplyr::select(Forest:Lowest_Common_Taxon_Name,Total_Harvest_prop) %>%
  mutate(Total_Harvest_prop = Total_Harvest_prop/100)


  
##calculate richness, sw diversity and evenness of harvest taxa
richness <- df_comm_avg %>%
  ungroup() %>%
  select(Forest, Site, Lowest_Common_Taxon_Name) %>%
  group_by(Forest, Site) %>%
  count() %>%
  rename(richness = "n")

df_comm_wide <- df_comm_avg %>%
  ungroup() %>%
  dplyr::select(Site, Lowest_Common_Taxon_Name, Total_Harvest_prop) %>%
  spread(key = Lowest_Common_Taxon_Name, value = Total_Harvest_prop)

df_comm_wide[is.na(df_comm_wide)] <- 0
df_comm_wide <- df_comm_wide %>%
  remove_rownames %>% 
  column_to_rownames(var="Site")
##select only numerical rows
df_comm_wide <- df_comm_wide %>%
  dplyr:: select(Abalone:Wolffish)


##SW diversity 
sw_div <- as.data.frame(diversity(df_comm_wide, index = "shannon"))
sw_div <- rownames_to_column(sw_div, "Site") %>%
  rename(sw_diversity = `diversity(df_comm_wide, index = "shannon")`)

head(sw_div)

comm_div <- left_join(richness, sw_div, by = "Site") 

##evenness
comm_div <- comm_div %>%
  mutate(evenness = sw_diversity/log(richness))



##calculate harvest habitat coupling metrics (diversity, evenness, SD) -----------
df_h <- df_comm_avg %>%
  group_by(Site, Habitat) %>%
  summarise_at(vars(Total_Harvest_prop), list(total = sum))

##evenness based on proportion harvest (##habitat diversity/coupling values are the same for total harvest and prop harvest, so doesn't matter)
df_h_prop <- df_h %>%
  ungroup() %>%
  dplyr::select(Site, Habitat, total) %>%
  spread(key = Habitat, value = total)

df_h_prop[is.na(df_h_prop)] <- 0
df_h_prop <- df_h_prop %>%
  remove_rownames %>% 
  column_to_rownames(var="Site")
##select only numerical rows
df_h_prop <- df_h_prop %>%
  dplyr:: select(Freshwater_Anadromous:Terrestrial)


##SW diversity 
sw_div_prop <- as.data.frame(diversity(df_h_prop, index = "shannon"))
sw_div_prop <- rownames_to_column(sw_div_prop, "Site") %>%
  rename(sw_diversity = `diversity(df_h_prop, index = "shannon")`)

h_div_prop <- sw_div_prop %>%
  mutate(richness = 4) %>%
  mutate(evenness = sw_diversity/log(richness)) 

hist(h_div_prop$evenness)



##mean and sd of harvest proportion across habitats
h_df_mean <- df_h %>%
  select(Site, Habitat, total) %>%
  group_by(Site) %>%
  summarise_at(vars(total), list(avg = mean, sd = sd)) 


##combining mean, sd and diversity
library(car)
h_df_all <- left_join(h_df_mean, h_div_prop, by = "Site") %>%
  mutate(evenness_log = log(evenness)) %>%
  mutate(sd_log = log(sd)) %>%
  mutate(log_1_SD = log(1/sd)) %>%
  mutate(sd_0_1 = 1-(sd/0.5)) %>%
  mutate(logit_sd_0_1 = logit(sd_0_1)) ##logit transformation appropriate for proportions and %'s 

hist(h_df_all$sd)
hist(h_df_all$sd_log)
hist(h_df_all$log_1_SD)
hist(h_df_all$sd_0_1)

hist(h_df_all$logit_sd_0_1)

h_df_all <- h_df_all %>%
  dplyr::rename(sw_div_h = "sw_diversity", richness_h = "richness", evenness_h = "evenness", evenness_h_log = "evenness_log")


ggplot(h_df_all, aes(x = evenness_h, y = sd)) +
  geom_point() +
  theme_classic() +
  labs(x = "Habitat Eveness (Pielou)", y= "Habitat Eveness (SD of harvest proportion)")

ggplot(h_df_all, aes(x = evenness_h, y = sd_log)) +
  geom_point()

##Trying out scaling SD (i.e., habitat coupling metric) between 0-1



ggplot(h_df_all, aes(x = log_1_SD, y= logit_sd_0_1)) +
  geom_point() +
  theme_classic()

##I think could add in a maximum and minimum standard deviation -- which for 4 habitats will always be the same, and then scale the other SD to this 

##minimum would be 0 (or 0.0001 so can log transform) and max will be... 50
##new equation:first, change % to prop, then  1-(SD/0.5) -- this gives range of 0-1, with 0 being minimal/no coupling, and 1 = perfectly even coupling
##then for stats/figure, do log transformation to improve normality 


##Join habitat coupling metrics with harvest diversity metrics ------------
comm_div_all <- left_join(comm_div, h_df_all, by = "Site") 


##save csv
write.csv(comm_div_all, "data/intermediate_data/harvest_diversity_metrics.csv")


##Looking at correlations between harvest characteristics
ggplot(comm_div_all, aes(x = sw_diversity, y = log(1/sd))) +
  geom_point() +
  geom_smooth(method = "lm")

lm <- lm(log(1/sd) ~ sw_diversity, comm_div_all)
summary(lm)

ggplot(comm_div_all, aes(x = richness, y = log(1/sd))) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(comm_div_all, aes(x = evenness, y = log(1/sd))) +
  geom_point() +
  geom_smooth(method = "lm")

##Plot proportion of harvest by habitat for each community
library(data.table)
df_h_prop <- rownames_to_column(df_h_prop, "Site")
df_h_prop_long <- melt(setDT(df_h_prop), id.vars = c("Site"), variable.name = "Habitat")


ggplot(df_h_prop_long, aes(y = value, x = Site, group = Habitat, fill = Habitat)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("#FF9999", "#003366", "#CC9966", "#339933")) +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


##harvest proportion pie charts
df_h_prop_long %>%
  filter(Site == "Metlakatla") %>%
  ggplot(aes(x = "", y = value,fill = Habitat)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#FF9999", "#003366", "#CC9966", "#339933")) +
  theme_void()


df_h_prop_long %>%
  filter(Site == "Klukwan") %>%
  ggplot(aes(x = "", y = value,fill = Habitat)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#FF9999", "#003366", "#CC9966", "#339933")) +
  theme_void()

