###Phenology Figures -- for SFS talk and potentiall manuscript 


library(tidyverse)
library(ggplot2)

##Read in whichever harvest distributions (i.e., based on proportions, total lbs harvested or percapita)
##Proportion of total harvest
simulated_harvest_df <- read.csv("data/intermediate_data/simulated_harvest_distributions_harvest_percapita.csv")


##Calculating total harvest per date -----------------
total_simulated_harvest <- simulated_harvest_df %>%
  filter(!is.na(harvest_amount)) %>%
  group_by(site, date) %>%
  summarise_at(vars(harvest_amount), list(harvest_total = sum))


##Representative Communities: Klukwan and Metlaktla 
##Klukwan 
kluk <- simulated_harvest_df %>%
  filter(site == "Klukwan")
kluk_total <- total_simulated_harvest %>%
  filter(site == "Klukwan")

met <- simulated_harvest_df %>%
  filter(site == "Metlakatla")
met_total <- total_simulated_harvest %>%
  filter(site == "Metlakatla")

##Klukwan Phenology Plot
kluk_plot_1 <- ggplot() +
  geom_line(data = kluk, aes(x = date, y = harvest_amount, group = habitat, color = habitat)) +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Day of Year (J)", y = "Daily Percapita Harvest Amount") +
  theme_classic()+
 # ylim(0,1.75) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 10),axis.title.y=element_text(size = 12),  legend.position = "bottom", legend.title = element_text("Habitat"), text = element_text(family = "Times New Roman"))

kluk_plot_1

kluk_plot_2 <- ggplot() +
  geom_line(data = kluk, aes(x = date, y = harvest_amount, group = habitat, color = habitat)) +
  geom_line(data = kluk_total, aes(x = date, y = harvest_total), linewidth = 1, colour = "black") +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Day of Year", y = "Daily Harvest Amount\n(kg/person)") +
  theme_classic()+
  ylim(0,6.4) +
  theme(axis.text.x = element_text( size = 10),axis.text.y = element_text(size = 10),axis.title.y=element_text(size = 12), axis.title.x = element_text(size = 12), legend.position = "bottom", legend.title = element_text("Habitat"), text = element_text(family = "Times New Roman"))


kluk_plot_2

met_plot_1 <- ggplot() +
  geom_line(data = met, aes(x = date, y = harvest_amount, group = habitat, color = habitat)) +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Day of Year (J)", y = "Daily Harvest Amount (% total harvest)") +
  theme_classic()+
  # ylim(0,1.75) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 10),axis.title.y=element_text(size = 12),  legend.position = "bottom", legend.title = element_text("Habitat"), text = element_text(family = "Times New Roman"))

met_plot_1

met_plot_2 <- ggplot() +
  geom_line(data = met, aes(x = date, y = harvest_amount, group = habitat, color = habitat)) +
  geom_line(data = met_total, aes(x = date, y = harvest_total), linewidth = 1, color = "black") +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Day of Year", y = "Harvest Amount (%)") +
  theme_classic()+
  #ylim(0, 1.75) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),axis.text.y = element_text(size = 10),axis.title.y=element_text(size = 12), axis.title.x = element_blank(), legend.position = "bottom", legend.title = element_text("Habitat"), text = element_text(family = "Times New Roman"))

met_plot_2





ang <- simulated_harvest_df %>%
  filter(site == "Angoon")
ang_total <- total_simulated_harvest %>%
  filter(site == "Angoon")

ang_plot_2 <- ggplot() +
  geom_line(data = ang, aes(x = date, y = harvest_amount, group = habitat, color = habitat)) +
  geom_line(data = ang_total, aes(x = date, y = harvest_total), linewidth = 1, color = "black") +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Day of Year", y = "Daily Harvest Amount\n(kg/person)") +
  theme_classic()+
  ylim(0, 6.4) +
  theme(axis.text.x = element_text(size = 10),axis.text.y = element_text(size = 10),axis.title.y=element_text(size = 12), axis.title.x =element_text(size = 12) , legend.position = "bottom", legend.title = element_text("Habitat"), text = element_text(family = "Times New Roman"))

ang_plot_2



