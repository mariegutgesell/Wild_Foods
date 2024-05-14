##Harvest Evenness across habitats

library(tidyverse)
library(ggplot2)
library(ggrepel)
library(tibble)

##source code that calculates average harvest across time for each community
source("code/3T_calculate_avg_community_harvest.R")
rm(list = ls()[!ls() %in% c("df_comm_avg")])

##calculate harvest proportions and total harvest by habitat
df_h <- df_comm_avg %>%
  group_by(Site, Habitat) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum_avg:Total_Harvest_prop), list(total = sum))

##evenness based on total harvest
df_h_total <- df_h %>%
  ungroup() %>%
  dplyr::select(Site, Habitat, Estimated_Total_Pounds_Harvested_sum_avg_total) %>%
  spread(key = Habitat, value = Estimated_Total_Pounds_Harvested_sum_avg_total)

df_h_total[is.na(df_h_total)] <- 0
df_h_total <- df_h_total %>%
  remove_rownames %>% 
  column_to_rownames(var="Site")
##select only numerical rows
df_h_total <- df_h_total %>%
  dplyr:: select(Freshwater_Anadromous:Terrestrial)


##SW diversity 
sw_div_total <- as.data.frame(diversity(df_h_total, index = "shannon"))
sw_div_total <- rownames_to_column(sw_div_total, "Site") %>%
  rename(sw_diversity = `diversity(df_h_total, index = "shannon")`)

h_div_total <- sw_div_total %>%
  mutate(richness = 4) %>%
  mutate(evenness = sw_diversity/log(richness))

hist(h_div_total$evenness)

##evenness based on percapita harvest
df_h_pc <- df_h %>%
  ungroup() %>%
  dplyr::select(Site, Habitat, Percapita_Pounds_Harvested_sum_avg_total) %>%
  spread(key = Habitat, value = Percapita_Pounds_Harvested_sum_avg_total)

df_h_pc[is.na(df_h_pc)] <- 0
df_h_pc <- df_h_pc %>%
  remove_rownames %>% 
  column_to_rownames(var="Site")
##select only numerical rows
df_h_pc <- df_h_pc %>%
  dplyr:: select(Freshwater_Anadromous:Terrestrial)


##SW diversity 
sw_div_pc <- as.data.frame(diversity(df_h_pc, index = "shannon"))
sw_div_pc <- rownames_to_column(sw_div_pc, "Site") %>%
  rename(sw_diversity = `diversity(df_h_pc, index = "shannon")`)

h_div_pc <- sw_div_pc %>%
  mutate(richness = 4) %>%
  mutate(evenness = sw_diversity/log(richness))

hist(h_div_pc$evenness)

##evenness based on proportion harvest
df_h_prop <- df_h %>%
  ungroup() %>%
  dplyr::select(Site, Habitat, Total_Harvest_prop_total) %>%
  spread(key = Habitat, value = Total_Harvest_prop_total)

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
  select(Site, Habitat, Total_Harvest_prop_total) %>%
  group_by(Site) %>%
  summarise_at(vars(Total_Harvest_prop_total), list(avg = mean, sd = sd)) 


##combining mean, sd and diversity
h_df_all <- left_join(h_df_mean, h_div_prop, by = "Site") %>%
  mutate(evenness_log = log(evenness))

hist(h_df_all$sd)


ggplot(h_df_all, aes(x = evenness, y = sd)) +
  geom_point() +
  theme_classic() +
  labs(x = "Habitat Eveness (Pielou)", y= "Habitat Eveness (SD of harvest proportion)")

ggplot(h_df_all, aes(x = evenness_log, y = sd)) +
  geom_point()

##Plot proportion of harvest by habitat for each community
library(data.table)
df_h_prop <- rownames_to_column(df_h_prop, "Site")
df_h_prop_long <- melt(setDT(df_h_prop), id.vars = c("Site"), variable.name = "Habitat")
  

ggplot(df_h_prop_long, aes(y = value, x = Site, group = Habitat, fill = Habitat)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("#FF9999", "#003366", "#CC9966", "#339933")) +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


##save csv
write.csv(h_df_all, "data/intermediate_data/harvest_diversity_by_habitat.csv")
