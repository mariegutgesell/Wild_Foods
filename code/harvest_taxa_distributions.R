##Harvest total across taxa -- across all communities combined
##Note: i think i have done this somewhere else already... but need to go through and clean up all scripts and improve workflow


library(ggplot2)
library(tidyverse)
library(ggridges)

##import cleaned harvest data and trophic info that is comparable across all years
df <- read.csv("data/intermediate_data/comparable_harvest_df.csv")

test <- df %>%
  filter(Lowest_Common_Taxon_Name == "Caribou")

sp_list <- df %>%
  select(Lowest_Common_Taxon_Name) %>%
  distinct()

##calculate total harvest of all species combined
total_harvest <- df %>%
  select(Lowest_Common_Taxon_Name, Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum) %>%
  filter(Percapita_Pounds_Harvested_sum != 0) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum), list(total = sum))

##calculate total harvest per taxa and proportion of total harvest
total_harvest_taxa <- df %>%
  select(Lowest_Common_Taxon_Name, Habitat, Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum) %>%
  filter(Percapita_Pounds_Harvested_sum != 0) %>%
  group_by(Lowest_Common_Taxon_Name, Habitat) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum), list(total = sum)) %>%
  mutate(total_est_pounds_harvested_all = 13200949, total_est_percapita_all = 18763.77) %>%
  mutate(est_pounds_harvested_prop = Estimated_Total_Pounds_Harvested_sum_total/total_est_pounds_harvested_all*100,
         percapita_prop = Percapita_Pounds_Harvested_sum_total/total_est_percapita_all*100) %>%
  ungroup() %>%
  mutate(harvest_rank = rank(-est_pounds_harvested_prop, ties.method = "min")) #%>%



##plot harvest distribution of taxa 
ggplot(total_harvest_taxa, aes(x = harvest_rank, y = est_pounds_harvested_prop, fill = Habitat)) +
  geom_col(color = "black") +
  geom_text(aes(label = Lowest_Common_Taxon_Name), hjust = -0.25, angle = 90, size = 4) + 
  scale_fill_manual(values = c("#FF9999", "#003366", "#CC9966", "#339933")) +
  ylim(0, 40) +
  ylab("Harvest Proportion") +
  xlab("Harvest Rank") +
  theme_classic()+
  theme(legend.position = "none", axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15))


##Determine total harvest proportion cumulatively -- want to determine when total harvest is 95% 
total_harvest_taxa <- total_harvest_taxa[order(-total_harvest_taxa$est_pounds_harvested_prop),]
total_harvest_taxa$harvest_total <- cumsum(total_harvest_taxa$est_pounds_harvested_prop)




