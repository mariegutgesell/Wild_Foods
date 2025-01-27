###Synchrony Figure 

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(codyn)
###2) AVERAGE HARVEST OVER TIME -- SYNCHRONY  
df_1 <- read.csv("data/intermediate_data/average_harvest_phenology_summary_metrics_percapita.csv") %>%
  select(Forest:synchrony_gross_habitat)
df_2 <- read.csv("data/intermediate_data/average_harvest_removal_results_percap.csv") %>%
  rename(site = "Site") %>%
  select(Forest:alpha)

avg_df <- left_join(df_1, df_2, by = c("Forest", "site", "richness", "sw_diversity", "evenness", "avg", "sd", "sw_div_h", "richness_h", "evenness_h", "evenness_h_log", "sd_log", "log_1_SD")) 
rm(list = ls()[!ls() %in% c("mean_hc_cv", "avg_df")])


##SYNCHRONY (GROSS) vs. SW diversity and coupling
avg_sync_coupling <- ggplot(avg_df, aes(x = log_1_SD, y = synchrony_gross_habitat)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Habitat Coupling (log 1/SD)") +
  ylab("Seasonal Synchrony\n(Gross)") +
  #  ylim(0.5, 1.55) +
  #  xlim(-3.7,-1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank())
avg_sync_coupling

avg_sync_diversity <- ggplot(avg_df, aes(x = sw_diversity, y = synchrony_gross_habitat)) +
  geom_point() +
#  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Harvest Diversity (SW)") +
  ylab("Seasonal Synchrony\n(Gross)") +
  #  xlim(1.9, 3) +
  #  ylim(0.5,1.55)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank())
avg_sync_diversity




avg_sync_cv <- ggplot(avg_df, aes(x = synchrony_gross_habitat, y = harvest_total_cv)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Seasonal Habitat Synchrony (Gross)") +
  ylab("Percapita Harvest CV\n(seasonal)") +
  #  ylim(0.5, 1.55) +
  #  xlim(-3.7,-1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank())
avg_sync_cv

seasonal_synchrony_fig <-  ggarrange(avg_sync_cv, 
                                     ggarrange(avg_sync_diversity, avg_sync_coupling, nrow = 1, ncol = 2, labels = c("b)", "c)"), font.label = list(colour = "black", size = 14, family = "Times New Roman")), 
nrow = 2, ncol = 1, labels = c("a)"), font.label = list(colour = "black", size = 14, family = "Times New Roman"))
seasonal_synchrony_fig


###Linear Regressions 
avg_sync_g_coupling_lm <- lm(synchrony_gross_habitat ~ log_1_SD, avg_df)
summary(avg_sync_g_coupling_lm)

avg_sync_g_diversity_lm <- lm(synchrony_gross_habitat ~ sw_diversity, avg_df)
summary(avg_sync_g_diversity_lm)

avg_sync_g_cv_lm <- lm(harvest_total_cv ~ synchrony_gross_habitat, avg_df)
summary(avg_sync_g_cv_lm)

avg_sync_l_coupling_lm <- lm(synchrony_loreau_habitat ~ log_1_SD, avg_df)
summary(avg_sync_l_coupling_lm)

avg_sync_l_diversity_lm <- lm(synchrony_loreau_habitat ~ sw_diversity, avg_df)
summary(avg_sync_l_diversity_lm)

avg_sync_l_cv_lm <- lm(harvest_total_cv ~ synchrony_loreau_habitat, avg_df)
summary(avg_sync_l_cv_lm)



#####DECADAL SYNCHRONY ACROSS HABITAT ---------------
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

source("code/Temporal_Community_Harvest/Tongass_Chugach/3CT_calculate_prop_harvest.R")


df_temp_avg <- df_temp_avg %>%
  filter(Site_Year_Code %in% df_hc$Site_Year_Code)

##Calculate total harvest per habitat each year
df_h <- df_temp_avg %>%
  #  select(Site_Year_Code, Habitat, Lowest_Common_Taxon_Name, Estimated_Total_Pounds_Harvested_sum:Total_Harvest_prop)
  group_by(Site_Year_Code, Habitat) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum, Total_Harvest_prop), list(total = sum)) %>%
  separate(Site_Year_Code, c("Site", "Year"), sep = "_", remove = FALSE) %>%
  filter(Site_Year_Code %in% df_hc$Site_Year_Code) %>%
  filter(Site_Year_Code != "Hoonah_2016") %>%
  mutate(percapita_kg = Percapita_Pounds_Harvested_sum_total*0.45359237) %>%
  rename(site = "Site")

###Calculate synchrony between habitats over time -- right this makes sense to do for aggregated ... not for each individual species.. 
str(df_h)
df_h$Year <- as.numeric(df_h$Year)
decadal_synchrony_loreau <- synchrony(df = df_h, time.var = "Year", species.var = "Habitat", abundance.var = "percapita_kg", replicate.var = "site", metric = "Loreau") %>%
  dplyr::rename(synchrony_loreau = "synchrony")

decadal_synchrony_gross <- synchrony(df = df_h, time.var = "Year", species.var = "Habitat", abundance.var = "percapita_kg", replicate.var = "site", metric = "Gross") %>%
  dplyr::rename(synchrony_gross = "synchrony")


decadal_synchrony_df <- left_join(decadal_synchrony_loreau, decadal_synchrony_gross, by = "site") %>%
  rename(Community = "site")

##Calculate mean, sd and CV of total harvest across time 
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

##Calculate mean harvest structure metrics, and join to CV of total harvest
mean_hc <- df_hc %>%
  select(Site, richness, sw_diversity, evenness, sd, log_1_SD) %>%
  group_by(Site) %>%
  summarise_at(vars(richness, sw_diversity, evenness, sd, log_1_SD), list(mean = mean)) %>%
  rename(Community ="Site")


mean_hc_cv <- left_join(mean_hc, harvest_cv, by = "Community") %>%
  left_join(decadal_synchrony_df)

####Plotting
##SYNCHRONY (GROSS) vs. SW diversity and coupling
dec_sync_coupling <- ggplot(mean_hc_cv, aes(x = log_1_SD_mean, y = synchrony_gross)) +
  geom_point() +
#  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Mean Habitat Coupling (log 1/SD)") +
  ylab("Decadal Synchrony\n(Gross)") +
  #  ylim(0.5, 1.55) +
  #  xlim(-3.7,-1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank())
dec_sync_coupling

dec_sync_diversity <- ggplot(mean_hc_cv, aes(x = sw_diversity_mean, y = synchrony_gross)) +
  geom_point() +
  #  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Mean Harvest Diversity (SW)") +
  ylab("Decadal Synchrony\n(Gross)") +
  #  xlim(1.9, 3) +
  #  ylim(0.5,1.55)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank())

dec_sync_diversity


dec_sync_cv <- ggplot(mean_hc_cv, aes(x = synchrony_gross, y = cv_ph)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Decadal Habitat Synchrony (Gross)") +
  ylab("Percapita Harvest CV\n(decadal)") +
  #  ylim(0.5, 1.55) +
  #  xlim(-3.7,-1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank())
dec_sync_cv

decadal_synchrony_fig <-  ggarrange(dec_sync_cv, 
                                     ggarrange(dec_sync_diversity, dec_sync_coupling, nrow = 1, ncol = 2, labels = c("b)", "c)"), font.label = list(colour = "black", size = 14, family = "Times New Roman")), 
                                     nrow = 2, ncol = 1, labels = c("a)"), font.label = list(colour = "black", size = 14, family = "Times New Roman"))
decadal_synchrony_fig


###GROSS SYNCHRONY FIG -- DECADAL AND AVERAGE SYNCHRONY VS CV and Portfolio effect boxplots
source("code/portfolio_effect_boxplot_fig.R")
#rm(list = ls()[!ls() %in% c("dec_sync_cv", "avg_sync_cv", "harvest_cv_all")])

seasonal_pf <- harvest_cv_all %>%
  filter(time_type == "Seasonal") %>%
  ggplot( aes(x = `CV Type`, y = CV, fill = `CV Type`)) +
  geom_boxplot() +
  scale_fill_manual(values = c( "white", "black"))+
  theme_classic() +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), strip.background = element_blank())
seasonal_pf

decadal_pf <- harvest_cv_all %>%
  filter(time_type == "Decadal") %>%
  ggplot( aes(x = `CV Type`, y = CV, fill = `CV Type`)) +
  geom_boxplot() +
  scale_fill_manual(values = c( "white", "black"))+
  theme_classic() +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_blank(), text = element_text(family = "Times New Roman"), strip.background = element_blank())
decadal_pf

avg_decadal_synchrony_fig <-  ggarrange(avg_sync_cv, seasonal_pf, dec_sync_cv, decadal_pf,
                                    nrow = 2, ncol = 2, labels = c("i)", "ii)", "i)", "ii)"), font.label = list(colour = "black", size = 14, family = "Times New Roman"))
avg_decadal_synchrony_fig


##########LOREAU -------------
####LOREAU -- AVERAGE
avg_sync_coupling <- ggplot(avg_df, aes(x = log_1_SD, y = synchrony_loreau)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Habitat Coupling (log 1/SD)") +
  ylab("Seasonal Synchrony (Loreau)") +
  #  ylim(0.5, 1.55) +
  #  xlim(-3.7,-1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank())
avg_sync_coupling

avg_sync_diversity <- ggplot(avg_df, aes(x = sw_diversity, y = synchrony_loreau)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Harvest Diversity (SW)") +
  ylab("Seasonal Synchrony (Loreau)") +
  #  xlim(1.9, 3) +
  #  ylim(0.5,1.55)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank())
avg_sync_diversity


avg_sync_cv <- ggplot(avg_df, aes(x = synchrony_loreau_habitat, y = harvest_total_cv)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Seasonal Habitat Synchrony (Loreau)") +
  ylab("Percapita Harvest CV\n(seasonal)") +
  #  ylim(0.5, 1.55) +
  #  xlim(-3.7,-1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank())
avg_sync_cv

seasonal_synchrony_fig <-  ggarrange(avg_sync_cv, 
                                     ggarrange(avg_sync_diversity, avg_sync_coupling, nrow = 1, ncol = 2, labels = c("b)", "c)"), font.label = list(colour = "black", size = 14, family = "Times New Roman")), 
                                     nrow = 2, ncol = 1, labels = c("a)"), font.label = list(colour = "black", size = 14, family = "Times New Roman"))
seasonal_synchrony_fig

####LOREAU -DECADAL
dec_sync_coupling <- ggplot(mean_hc_cv, aes(x = log_1_SD_mean, y = synchrony_loreau)) +
  geom_point() +
#  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Mean Habitat Coupling (log 1/SD)") +
  ylab("Decadal Synchrony (Loreau)") +
  #  ylim(0.5, 1.55) +
  #  xlim(-3.7,-1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank())
dec_sync_coupling

dec_sync_diversity <- ggplot(mean_hc_cv, aes(x = sw_diversity_mean, y = synchrony_loreau)) +
  geom_point() +
#  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Mean Harvest Diversity (SW)") +
  ylab("Decadal Synchrony (Loreau)") +
  #  xlim(1.9, 3) +
  #  ylim(0.5,1.55)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank())
dec_sync_diversity


dec_sync_cv <- ggplot(mean_hc_cv, aes(x = synchrony_loreau, y = cv_ph)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Decadal Habitat Synchrony (Loreau)") +
  ylab("Percapita Harvest CV\n(seasonal)") +
  #  ylim(0.5, 1.55) +
  #  xlim(-3.7,-1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank())
dec_sync_cv

decadal_synchrony_fig <-  ggarrange(dec_sync_cv, 
                                    ggarrange(dec_sync_diversity, dec_sync_coupling, nrow = 1, ncol = 2, labels = c("b)", "c)"), font.label = list(colour = "black", size = 14, family = "Times New Roman")), 
                                    nrow = 2, ncol = 1, labels = c("a)"), font.label = list(colour = "black", size = 14, family = "Times New Roman"))
decadal_synchrony_fig

loreau_syncrhony_fig <- ggarrange(avg_sync_cv, dec_sync_cv, nrow = 1, ncol = 2, labels = c("a)", "b)"), font.label = list(colour = "black", size = 14, family = "Times New Roman"))
loreau_syncrhony_fig
###Linear Regressions 
dec_sync_g_coupling_lm <- lm(synchrony_gross ~ log_1_SD_mean, mean_hc_cv)
summary(dec_sync_g_coupling_lm)

dec_sync_g_diversity_lm <- lm(synchrony_gross ~ sw_diversity_mean, mean_hc_cv)
summary(dec_sync_g_diversity_lm)

dec_sync_g_cv_lm <- lm(cv_ph ~ synchrony_gross, mean_hc_cv)
summary(dec_sync_g_cv_lm)

dec_sync_l_coupling_lm <- lm(synchrony_loreau ~ log_1_SD_mean, mean_hc_cv)
summary(dec_sync_l_coupling_lm)

dec_sync_l_diversity_lm <- lm(synchrony_loreau ~ sw_diversity_mean, mean_hc_cv)
summary(dec_sync_l_diversity_lm)

dec_sync_l_cv_lm <- lm(cv_ph ~ synchrony_loreau, mean_hc_cv)
summary(dec_sync_l_cv_lm)


avg_sync_l_cv_lm <- lm(harvest_total_cv ~ synchrony_loreau_habitat, avg_df)
summary(avg_sync_l_cv_lm)
