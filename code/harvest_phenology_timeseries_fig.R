##Harvest PHenology/Time Series Figure

library(ggplot2)
library(tidyverse)
library(ggpubr)


###SEASONAL HARVEST PHENOLOGY ---------------

##Read in whichever harvest distributions (i.e., based on proportions, total lbs harvested or percapita)
##Proportion of total harvest
simulated_harvest_df <- read.csv("data/intermediate_data/simulated_harvest_distributions_harvest_percapita.csv") %>%
  mutate(harvest_amount_kg = harvest_amount*0.45359237)


##Calculating total harvest per date -----------------
total_simulated_harvest <- simulated_harvest_df %>%
  filter(!is.na(harvest_amount_kg)) %>%
  group_by(site, date) %>%
  summarise_at(vars(harvest_amount_kg), list(harvest_total = sum))


##Representative Communities: Klukwan and Metlaktla 
##Klukwan 
seasonal_example1 <- simulated_harvest_df %>%
  filter(site == "Klukwan")
seasonal_example1_total <- total_simulated_harvest %>%
  filter(site == "Klukwan")

##Klukwan Phenology Plot

seasonal_ex1_plot <- ggplot() +
  geom_line(data = seasonal_example1, aes(x = date, y = harvest_amount_kg, group = habitat, color = habitat)) +
  geom_line(data = seasonal_example1_total, aes(x = date, y = harvest_total), linewidth = 1, colour = "black") +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Day of Year", y = "Daily Harvest Amount\n(kg/person)") +
  theme_classic()+
#  ylim(0,6.4) +
  theme(axis.text.x = element_text( size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), legend.position = "none", legend.title = element_text("Habitat"), text = element_text(family = "Times New Roman"))
seasonal_ex1_plot


seasonal_example2 <- simulated_harvest_df %>%  filter(site == "Angoon")
seasonal_example2_total <- total_simulated_harvest %>%
  filter(site == "Angoon")

seasonal_ex2_plot <- ggplot() +
  geom_line(data = seasonal_example2, aes(x = date, y = harvest_amount_kg, group = habitat, color = habitat)) +
  geom_line(data = seasonal_example2_total, aes(x = date, y = harvest_total), linewidth = 1, color = "black") +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Day of Year", y = "Daily Harvest Amount\n(kg/person)") +
  theme_classic()+
#  ylim(0, 6.4) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x =element_text(size = 14) , legend.position = "none", legend.title = element_text("Habitat"), text = element_text(family = "Times New Roman"))
seasonal_ex2_plot

##Plot of all communities -- supplemental figure

ggplot() +
  geom_line(data = simulated_harvest_df, aes(x = date, y = harvest_amount_kg, group = habitat, color = habitat)) +
  geom_line(data = total_simulated_harvest, aes(x = date, y = harvest_total), linewidth = 1, color = "black") +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Day of Year", y = "Daily Harvest Amount\n(kg/person)") +
  theme_classic()+
  #  ylim(0, 6.4) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x =element_text(size = 14) , legend.position = "none", legend.title = element_text("Habitat"), text = element_text(family = "Times New Roman")) +
  facet_wrap(~site)

##DECADAL HARVEST TIME SERIES -----------------
df_1 <- read.csv("data/intermediate_data/temporal_harvest_phenology_summary_metrics.csv") %>%
  rename(Site_Year_Code = "site")

df_2 <- read.csv("data/intermediate_data/temporal_harvest_removal_results.csv") %>%
  select(Site_Year_Code, alpha)

df_hc <- left_join(df_1, df_2, by = "Site_Year_Code")

##select only sites w/ more than three years of data
df_hc <- df_hc %>%
  group_by(Site) %>%
  filter(n_distinct(Year) >= 3) %>%
  filter(Site_Year_Code != "Hoonah_2016") %>%
  filter(Site != "Valdez") 
source("code/Temporal_Community_Harvest/Tongass_Chugach/3CT_calculate_prop_harvest.R")


df_temp_avg <- df_temp_avg %>%
  filter(Site_Year_Code %in% df_hc$Site_Year_Code)

df_h <- df_temp_avg %>%
  #  select(Site_Year_Code, Habitat, Lowest_Common_Taxon_Name, Estimated_Total_Pounds_Harvested_sum:Total_Harvest_prop)
  group_by(Site_Year_Code, Habitat) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum, Total_Harvest_prop), list(total = sum)) %>%
  separate(Site_Year_Code, c("Site", "Year"), sep = "_", remove = FALSE) %>%
  filter(Site_Year_Code %in% df_hc$Site_Year_Code) %>%
  filter(Site_Year_Code != "Hoonah_2016") %>%
  mutate(percapita_kg = Percapita_Pounds_Harvested_sum_total*0.45359237)


#####EXAMPLE TIME SERIES FOR MS FIGURE
##angoon
df_example_1 <- df_h %>%
  filter(Site %in% c("Klukwan"))
df_sum_example_1 <- df_example_1 %>%
  group_by(Site, Year, Site_Year_Code) %>%
  summarise_at(vars(percapita_kg), list(total_percapita = sum))

decadal_example1_plot <- ggplot() +
  geom_line(data = df_example_1, aes(x = Year, y = percapita_kg, group = Habitat, color = Habitat)) +
  geom_point(data = df_example_1, aes(x = Year, y = percapita_kg, group = Habitat, color = Habitat)) +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  geom_line(data = df_sum_example_1, aes(x = Year, y = total_percapita, group = Site)) +
  geom_point(data = df_sum_example_1, aes(x = Year, y = total_percapita, group = Site)) +
  theme_classic() + 
  ylab("Total Percapita Harvest\n(kg/person)") +
#  ylim(0,610) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"), legend.position  = "none")
decadal_example1_plot

##klukwan
df_example_2 <- df_h %>%
  filter(Site %in% c("Angoon"))
df_sum_example_2 <- df_example_2 %>%
  group_by(Site, Year, Site_Year_Code) %>%
  summarise_at(vars(percapita_kg), list(total_percapita = sum))


decadal_example2_plot <- ggplot() +
  geom_line(data = df_example_2, aes(x = Year, y = percapita_kg, group = Habitat, color = Habitat)) +
  geom_point(data = df_example_2, aes(x = Year, y = percapita_kg, group = Habitat, color = Habitat)) +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  geom_line(data = df_sum_example_2, aes(x = Year, y = total_percapita, group = Site)) +
  geom_point(data = df_sum_example_2, aes(x = Year, y = total_percapita, group = Site)) +
  theme_classic() + 
  ylab("Total Percapita Harvest\n(kg/person)") +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"), legend.position  = "none")
decadal_example2_plot


##Final plot 
##Create figure legend for 3 sites 
data <- data.frame(
  Xdata = rnorm(4),
  Ydata = rnorm(4),
  LegendData = c("Freshwater/Anadromous", "Marine", "Nearshore", "Terrestrial")
)
data$LegendData <- factor(data$LegendData, levels = c("Freshwater/Anadromous", "Marine", "Nearshore", "Terrestrial"))
gplot <- ggplot(data, aes(Xdata, Ydata, fill = LegendData)) +
  geom_col() +
  scale_fill_manual(values = c("#FF9999","#003366","#CC9966", "#339933")) +
  theme_classic()+
  guides(fill = guide_legend(title = "Habitat")) +
  theme(legend.title = element_text(family = "Times New Roman"), legend.text = element_text(size = 12, family = "Times New Roman"), legend.position = "bottom")
gplot

leg_fig <- get_legend(gplot)


temporal_fig <- ggarrange(seasonal_ex1_plot, seasonal_ex2_plot, decadal_example1_plot, decadal_example2_plot, legend = "bottom", common.legend = TRUE, legend.grob = leg_fig,
                   labels = c("a)", "b)", "c)", "d)"),
                   ncol = 2, nrow = 2, font.label = list(colour = "black", size = 14, family = "Times New Roman"))
temporal_fig

###SUPPLEMENTAL FIGURES ---------
##Seasonal harvest phenology -- all communities 
ggplot() +
  geom_line(data = simulated_harvest_df, aes(x = date, y = harvest_amount_kg, group = habitat, color = habitat)) +
  geom_line(data = total_simulated_harvest, aes(x = date, y = harvest_total), linewidth = 1, color = "black") +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Day of Year", y = "Daily Harvest Amount\n(kg/person)") +
  theme_classic()+
  #  ylim(0, 6.4) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x =element_text(size = 14) , legend.position = "none", legend.title = element_text("Habitat"), text = element_text(family = "Times New Roman")) +
  facet_wrap(~site)


##Decadal Harvest -- all communities

df_h_summary <- df_h %>%
  group_by(Site, Year, Site_Year_Code) %>%
  summarise_at(vars(percapita_kg), list(total_percapita = sum))

ggplot() +
  geom_line(data = df_h, aes(x = Year, y = percapita_kg, group = Habitat, color = Habitat)) +
  geom_point(data = df_h, aes(x = Year, y = percapita_kg, group = Habitat, color = Habitat)) +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  geom_line(data = df_h_summary, aes(x = Year, y = total_percapita, group = Site)) +
  geom_point(data = df_h_summary, aes(x = Year, y = total_percapita, group = Site)) +
  theme_classic() + 
  ylab("Total Percapita Harvest\n(kg/person)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"), legend.position  = "none") +
  facet_wrap(~Site)

