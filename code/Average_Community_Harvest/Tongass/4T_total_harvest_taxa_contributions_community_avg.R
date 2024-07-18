##Calculate total harvest of all taxa across all communities (averaged community harvest across years), rank by proportion of total harvest
##Use code to determine species that comprise 99% of harvest, average across communities

library(ggplot2)
library(tidyverse)
library(ggridges)

##import harvest data averaged within communities 
source("code/Average_Community_Harvest/Tongass/3T_calculate_avg_community_harvest.R")


##calculate total harvest of all species combined
total_harvest <- df_comm_avg %>%
  ungroup() %>%
  select(Lowest_Common_Taxon_Name, Estimated_Total_Pounds_Harvested_sum_avg, Percapita_Pounds_Harvested_sum_avg) %>%
  filter(Percapita_Pounds_Harvested_sum_avg != 0) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum_avg, Percapita_Pounds_Harvested_sum_avg), list(total = sum))

##calculate total harvest per taxa and proportion of total harvest and generate rank
total_harvest_taxa <- df_comm_avg %>%
  ungroup() %>%
  select(Lowest_Common_Taxon_Name, Habitat, Estimated_Total_Pounds_Harvested_sum_avg, Percapita_Pounds_Harvested_sum_avg) %>%
  filter(Percapita_Pounds_Harvested_sum_avg != 0) %>%
  group_by(Lowest_Common_Taxon_Name, Habitat) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum_avg, Percapita_Pounds_Harvested_sum_avg), list(total = sum)) %>%
  mutate(total_est_pounds_harvested_all =  4963302, total_est_percapita_all = 8916.142) %>%
  mutate(est_pounds_harvested_prop = Estimated_Total_Pounds_Harvested_sum_avg_total/total_est_pounds_harvested_all*100,
         percapita_prop = Percapita_Pounds_Harvested_sum_avg_total/total_est_percapita_all*100) %>%
  ungroup() %>%
  mutate(harvest_rank_total = rank(-est_pounds_harvested_prop, ties.method = "min")) %>%
  mutate(harvest_rank_percapita = rank(-percapita_prop, ties.method = "min"))


##plot harvest distribution of taxa -- total harvest proportion
ggplot(total_harvest_taxa, aes(x = harvest_rank_total, y = est_pounds_harvested_prop, fill = Habitat)) +
  geom_col(color = "black") +
  geom_text(aes(label = Lowest_Common_Taxon_Name), hjust = -0.25, angle = 90, size = 4) + 
  scale_fill_manual(values = c("#FF9999", "#003366", "#CC9966", "#339933")) +
  ylim(0, 40) +
  ylab("Harvest Proportion") +
  xlab("Harvest Rank") +
  theme_classic()+
  theme(legend.position = "none", axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15))

##plot harvest distribution of taxa -- percapita harvest proportion
ggplot(total_harvest_taxa, aes(x = harvest_rank_percapita, y = percapita_prop, fill = Habitat)) +
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
total_harvest_taxa <- total_harvest_taxa[order(-total_harvest_taxa$percapita_prop),]
total_harvest_taxa$percapita_harvest_total <- cumsum(total_harvest_taxa$percapita_prop)

##only difference in taxa composition that make up the 99%:
##when it is 99% of average percapita harvest, abalone and caribou not included (compared to average total harvest 99%) -- some different ordering but almost same composition
##when comparing 99% average total harvest to 99% of total harvest (not averaged across communities) -- also have geese and unknown non-salmon fish 
##
