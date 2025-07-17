##Looking at temporal (decadal) changes in relationships between harvest structure and stability

##
library(tidyverse)
library(ggplot2)

df_1 <- read.csv("data/intermediate_data/temporal_harvest_phenology_summary_metrics_percapita.csv") %>%
  rename(Site_Year_Code = "site")

df_2 <- read.csv("data/intermediate_data/temporal_harvest_removal_results_percapita.csv") %>%
  select(Site_Year_Code, alpha)

df_hc <- left_join(df_1, df_2, by = "Site_Year_Code") %>%
  separate(Site_Year_Code, into = c("Site", "Year"), sep = "_", remove = FALSE)

##select only sites w/ more than three years of data
df_hc <- df_hc %>%
  group_by(Site) %>%
  filter(n_distinct(Year) >= 3) %>%
  filter(Site_Year_Code != "Hoonah_2016") %>%
  filter(Site != "Valdez") ##removing because 3 surveys only span 3 years (want to look across decade)



##Exploring temporal trends in harvest structure characteristics and stability
ggplot(df_hc, aes(x = Year, y = richness, group = Site)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  facet_wrap(~Site)


ggplot(df_hc, aes(x = Year, y = sw_diversity, group = Site)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  facet_wrap(~Site)

ggplot(df_hc, aes(x = Year, y = evenness, group = Site)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  facet_wrap(~Site)

ggplot(df_hc, aes(x = Year, y = log_1_SD, group = Site)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  facet_wrap(~Site)

ggplot(df_hc, aes(x = Year, y = alpha, group = Site)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  facet_wrap(~Site)

ggplot(df_hc, aes(x = Year, y = harvest_total_cv, group = Site)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  facet_wrap(~Site)



##Looking at harvest phenology for communities w/ more than 3 years
simulated_harvest_df <- read.csv("data/intermediate_data/temporal_simulated_harvest_distributions_harvest_percapita.csv") %>%
  filter(site %in% df_hc$Site_Year_Code)

##Plotting all distributions for each community
ggplot(simulated_harvest_df, aes(x = date, y = harvest_amount, group = habitat, color = habitat)) +
  geom_line() +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Date", y = "Harvest Amount (%)", title = "Simulated Harvest Amounts Across Harvest Season") +
  facet_wrap(~site, scale = "free")


source("code/Temporal_Community_Harvest/Tongass_Chugach/3CT_calculate_prop_harvest.R")
df_temp_avg <- df_temp_avg %>%
  filter(Site_Year_Code %in% df_hc$Site_Year_Code)

df_h <- df_temp_avg %>%
  #  select(Site_Year_Code, Habitat, Lowest_Common_Taxon_Name, Estimated_Total_Pounds_Harvested_sum:Total_Harvest_prop)
  group_by(Site_Year_Code, Habitat) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum, Total_Harvest_prop), list(total = sum)) %>%
  separate(Site_Year_Code, c("Site", "Year"), sep = "_", remove = FALSE) %>%
  filter(Site_Year_Code %in% df_hc$Site_Year_Code) %>%
  filter(Site_Year_Code != "Hoonah_2016")


ggplot(df_h, aes(x = Year, y = Total_Harvest_prop_total, group = Habitat, color = Habitat)) +
  geom_point() +
  geom_line() +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  theme_classic() +
  facet_wrap(~Site)

ggplot(df_h, aes(x = Year, y = Estimated_Total_Pounds_Harvested_sum_total, group = Habitat, color = Habitat)) +
  geom_point() +
  geom_line() +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  theme_classic() +
  facet_wrap(~Site, scale = "free")

ggplot(df_h, aes(x = Year, y = Percapita_Pounds_Harvested_sum_total, group = Habitat, color = Habitat)) +
  geom_point() +
  geom_line() +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  theme_classic() +
  facet_wrap(~Site, scale = "free")



df_h_evos <- df_h %>%
  filter(Site %in% c("Chenega", "Tatitlek"))
df_sum_evos <- df_h_evos %>%
  group_by(Site, Year, Site_Year_Code) %>%
  summarise_at(vars(Percapita_Pounds_Harvested_sum_total), list(total_percapita = sum))

ggplot(df_h_evos, aes(x = Year, y = Percapita_Pounds_Harvested_sum_total, group = Habitat, color = Habitat)) +
  geom_point() +
  geom_line() +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  theme_classic() + 
  ylab("Percapita Harvest (lbs)")+
  facet_wrap(~Site, scale = "free")

ggplot() +
  geom_line(data = df_h_evos, aes(x = Year, y = Percapita_Pounds_Harvested_sum_total, group = Habitat, color = Habitat)) +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  geom_line(data = df_sum_evos, aes(x = Year, y = total_percapita, group = Site)) +
  theme_classic() + 
  ylab("Percapita Harvest (lbs)")+
  facet_wrap(~Site, scale = "free")

df_h %>%
  group_by(Site_Year_Code, Site, Year) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum_total), list(total_harvest = sum)) %>%
  ggplot( aes(x = Year, y =total_harvest, group = Site, color = Site)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  facet_wrap(~Site, scale = "free")

df_h %>%
  group_by(Site_Year_Code, Site, Year) %>%
  summarise_at(vars(Percapita_Pounds_Harvested_sum_total), list(percapita_harvest = sum)) %>%
  ggplot( aes(x = Year, y =percapita_harvest, group = Site, color = Site)) +
  geom_point() +
  geom_line() +
  theme_classic() #+
 #facet_wrap(~Site, scale = "free")

ggplot(df_temp_avg, aes(x = Year, y = Percapita_Pounds_Harvested_sum, group = Lowest_Common_Taxon_Name, color = Lowest_Common_Taxon_Name)) +
  geom_point() +
  geom_line() +
   theme_classic() +
  facet_wrap(~Community, scale = "free")

df_temp_avg %>%
  filter(Community == "Chenega") %>%
  ggplot(aes(x = Year, y = Estimated_Total_Pounds_Harvested_sum, group = Lowest_Common_Taxon_Name)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  facet_wrap(~Lowest_Common_Taxon_Name)


chenega <- df_temp_avg %>%
  filter(Community == "Chenega" & Year == 1989)


chenega2 <- df_temp_avg %>%
  filter(Community == "Chenega" & Year == 1985)
  


##Can we compare mean degree of coupling and cv of percapita harvest? (percapita controls for changes in population size effects)

harvest_cv <- df_temp_avg %>%
  group_by(Site_Year_Code, Community, Year) %>%
  summarise_at(vars(Percapita_Pounds_Harvested_sum), list(total_harvest = sum)) %>%
  ungroup() %>%
  group_by(Community) %>%
  mutate(mean_ph = mean(total_harvest),
         sd_ph = sd(total_harvest),
         cv_ph = sd_ph/mean_ph) %>%
  select(Community, mean_ph:cv_ph) %>%
  distinct()


mean_hc <- df_hc %>%
  select(Site, richness, sw_diversity, evenness, sd, log_1_SD) %>%
  group_by(Site) %>%
  summarise_at(vars(richness, sw_diversity, evenness, sd, log_1_SD), list(mean = mean)) %>%
  rename(Community ="Site")

mean_hc_cv <- left_join(mean_hc, harvest_cv, by = "Community") %>%
  filter(Community != "Klukwan")


ggplot(mean_hc_cv, aes(x = log_1_SD_mean, y = cv_ph)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Mean Habitat Coupling (log 1/SD)") +
  ylab("CV of Percapita Harvest Over Time") +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank())

lm1 <- lm(cv_ph ~ sd_mean, mean_hc_cv)
summary(lm1)

lm1 <- lm(cv_ph ~ log_1_SD_mean, mean_hc_cv)
summary(lm1)

ggplot(mean_hc_cv, aes(x = sw_diversity_mean, y = cv_ph)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Mean Harvest Diversity (SW)") +
  ylab("CV of Percapita Harvest Over Time") +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank())


lm3 <- lm(cv_ph ~ sw_diversity_mean, mean_hc_cv)
summary(lm3)









ggplot(mean_hc_cv, aes(x = richness_mean, y = cv_ph)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() +
  xlab("Mean Harvest Diversity (SW)") +
  ylab("CV of Percapita Harvest Over Time")

##remove valdez 
##look at correlation with TOTAL richness across surveys 


##Calculate CV of percapita harvest in each habitat
harvest_cv_hab <- df_h %>%
  group_by(Site, Habitat) %>%
  mutate(mean_ph = mean(Percapita_Pounds_Harvested_sum_total),
         sd_ph = sd(Percapita_Pounds_Harvested_sum_total),
         cv_ph = sd_ph/mean_ph) %>%
  select(Site, Habitat, mean_ph:cv_ph) %>%
  distinct()


mean_harvest_cv_hab <- harvest_cv_hab %>%
  ungroup() %>%
  group_by(Site) %>%
  summarise_at(vars(cv_ph), list(cv_ph_mean = mean)) %>%
  rename(Community = "Site")
  
harvest_cv_all <- left_join(harvest_cv, mean_harvest_cv_hab, by = "Community") %>%
  select(Community, cv_ph, cv_ph_mean) %>%
  pivot_longer(cols = c(cv_ph, cv_ph_mean), names_to = "CV_Type", values_to = "CV")

ggplot(harvest_cv_all, aes(x = CV_Type, y = CV)) +
  geom_boxplot() +
  theme_classic()

cv_ttest <- t.test(CV ~ CV_Type, harvest_cv_all)
cv_ttest


##within a given year, are seasonal CV, coupling, richness, alpha etc. correlated? / synchronized/autocorrelated?
ggplot(df_hc) +
  geom_line(data = df_hc, aes(x = Year, y= scale(harvest_total_cv), group = Site)) +
  geom_line(data = df_hc, aes(x = Year, y= scale(sd), group = Site, color = "red")) +
  facet_wrap(~Site)



ggplot(df_hc) +
  geom_line(data = df_hc, aes(x = Year, y= scale(harvest_total_cv), group = Site)) +
  geom_line(data = df_hc, aes(x = Year, y= scale(sw_diversity), group = Site, color = "red")) +
  facet_wrap(~Site)


lm <- lm(harvest_total_cv ~ log_1_SD + sw_diversity, df_hc) ##would want to add site as random factor 
summary(lm)

ggplot(df_hc, aes(x = sw_diversity, y = log_1_SD)) +
  geom_point() +
  geom_smooth(method = "lm")
##there is a significant correlation between sw and coupling -- community w/ more even coupling have higher diversity -- this is driven by pielous evenness 


ggplot(df_hc, aes(x = richness, y = log_1_SD)) +
  geom_point() +
  geom_smooth(method = "lm")

lm2 <- lm(harvest_total_cv ~ log_1_SD + richness, df_hc) ##would want to add site as random factor 
summary(lm2)



ggplot(df_hc, aes(x = richness, y = sd)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(df_hc, aes(x = evenness, y = sd)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(df_hc, aes(x = richness, y = evenness)) +
  geom_point() +
  geom_smooth(method = "lm")

