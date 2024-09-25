##Harvest Stability Figure 

library(tidyverse)
library(ggplot2)
library(ggpubr)

##1) DECADAL HARVEST CV AND MEAN HARVEST DIVERSITY AND COUPLING -------------------
##Import temporal harvest data
##Stability metric data 
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
  filter(Site != "Valdez") ##removing because 3 surveys only span 3 years (want to look across decade)

##Harvest data 
source("code/Temporal_Community_Harvest/Tongass_Chugach/3CT_calculate_prop_harvest.R")
df_temp_avg <- df_temp_avg %>%
  filter(Site_Year_Code %in% df_hc$Site_Year_Code)

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

##Mean harvest 
mean_hc <- df_hc %>%
  select(Site, richness, sw_diversity, evenness, sd, log_1_SD) %>%
  group_by(Site) %>%
  summarise_at(vars(richness, sw_diversity, evenness, sd, log_1_SD), list(mean = mean)) %>%
  rename(Community ="Site")

mean_hc_cv <- left_join(mean_hc, harvest_cv, by = "Community")

rm(list = ls()[!ls() %in% c("mean_hc_cv")])



###2) AVERAGE HARVEST OVER TIME -- 
df_1 <- read.csv("data/intermediate_data/average_harvest_phenology_summary_metrics.csv") %>%
  select(Forest:synchrony_gross)
df_2 <- read.csv("data/intermediate_data/average_harvest_removal_results.csv") %>%
  rename(site = "Site")

avg_df <- left_join(df_1, df_2, by = "site")
rm(list = ls()[!ls() %in% c("mean_hc_cv", "avg_df")])

##3) PLOTTING FIGURE 5 -------------
##3.1) Robustness vs. SW diversity and coupling
avg_robust_coupling <- ggplot(avg_df, aes(x = log_1_SD, y = alpha )) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Habitat Coupling (log 1/SD)") +
  ylab("Rate of Harvest\nDecline (alpha)") +
  xlim(-3.7,-1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank())
avg_robust_coupling

avg_robust_diversity <- ggplot(avg_df, aes(x = sw_diversity, y = alpha)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Harvest Diversity (SW)") +
  ylab("Rate of Harvest\nDecline (alpha)") +
  xlim(1.9, 3.0) + 
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank())
avg_robust_diversity

##3.2) Seasonal CV vs. SW diversity and coupling
avg_cv_coupling <- ggplot(avg_df, aes(x = log_1_SD, y = harvest_total_cv)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Habitat Coupling (log 1/SD)") +
  ylab("Percapita Harvest CV \n (seasonal)") +
  ylim(0.5, 1.55) +
  xlim(-3.7,-1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank())
avg_cv_coupling

avg_cv_diversity <- ggplot(avg_df, aes(x = sw_diversity, y = harvest_total_cv)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Harvest Diversity (SW)") +
  ylab("Percapita Harvest CV \n (seasonal)") +
  xlim(1.9, 3) +
  ylim(0.5,1.55)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank())
avg_cv_diversity

##3.3) Decadal CV vs. SW diversity and coupling
decadal_cv_coupling <- ggplot(mean_hc_cv, aes(x = log_1_SD_mean, y = cv_ph)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Mean Habitat Coupling (log 1/SD)") +
  ylab("Percapita Harvest CV \n (decadal)") +
  ylim(0, 0.63) +
  xlim(-3.7,-1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank())
decadal_cv_coupling

decadal_cv_diversity <- ggplot(mean_hc_cv, aes(x = sw_diversity_mean, y = cv_ph)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Mean Harvest Diversity (SW)") +
  ylab("Percapita Harvest CV \n (decadal)") +
  xlim(1.90, 3) + 
  ylim (0, 0.63) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank())
decadal_cv_diversity


##Combine into 1 figure
fig_5 <-  ggarrange(avg_robust_diversity, avg_robust_coupling, avg_cv_diversity, avg_cv_coupling, decadal_cv_diversity, decadal_cv_coupling, nrow = 3, ncol = 2, labels = c("a)", "b)", "c)", "d)", "e)", "f)"), font.label = list(colour = "black", size = 14, family = "Times New Roman"))
fig_5
###4) REGRESSION ANALYSIS ------------
avg_robust_div_lm <- lm(alpha ~ sw_diversity, avg_df)
summary(avg_robust_div_lm)

avg_robust_coupling_lm <- lm(alpha~ log_1_SD, avg_df)
summary(avg_robust_coupling_lm)

avg_cv_div_lm <- lm(harvest_total_cv ~ sw_diversity, avg_df)
summary(avg_cv_div_lm)

avg_cv_coupling_lm <- lm(harvest_total_cv ~ log_1_SD, avg_df)
summary(avg_cv_coupling_lm)

dec_cv_div_lm <- lm(cv_ph ~ sw_diversity_mean, mean_hc_cv)
summary(dec_cv_div_lm)

dec_cv_coupling_lm <- lm(cv_ph ~ log_1_SD_mean, mean_hc_cv)
summary(dec_cv_coupling_lm)



##5) PLOTTING SUPPLEMENTAL FIGURE -- Partitioning SW diversity into richness and evenness  ---------
##robustness
avg_robust_richness <- ggplot(avg_df, aes(x = richness, y = alpha)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Harvest Richness") +
  ylab("Rate of Harvest\nDecline (alpha)") +
 # xlim(1.9, 3.0) + 
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank())
avg_robust_richness

avg_robust_evenness <- ggplot(avg_df, aes(x = evenness, y = alpha)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Harvest Evenness") +
  ylab("Rate of Harvest\nDecline (alpha)") +
  # xlim(1.9, 3.0) + 
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank())
avg_robust_evenness

##seasonal CV
avg_cv_richness <- ggplot(avg_df, aes(x = richness, y = harvest_total_cv)) +
  geom_point() +
#  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Harvest Richness") +
  ylab("Percapita Harvest CV\n(seasonal)") +
  # xlim(1.9, 3.0) + 
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank())
avg_cv_richness

avg_cv_evenness <- ggplot(avg_df, aes(x = evenness, y = harvest_total_cv)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Harvest Evenness") +
  ylab("Percapita Harvest CV\n(seasonal)") +
  # xlim(1.9, 3.0) + 
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank())
avg_cv_evenness

##decadal CV
decadal_cv_richness <- ggplot(mean_hc_cv, aes(x = richness_mean, y = cv_ph)) +
  geom_point() +
#  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Mean Harvest Richness") +
  ylab("Percapita Harvest CV\n(decadal)") +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank())
decadal_cv_richness

decadal_cv_evenness <- ggplot(mean_hc_cv, aes(x = evenness_mean, y = cv_ph)) +
  geom_point() +
#  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  xlab("Mean Harvest Evenness") +
  ylab("Percapita Harvest CV\n(decadal)") +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank())
decadal_cv_evenness

supp_fig <-  ggarrange(avg_robust_richness, avg_robust_evenness, avg_cv_richness, avg_cv_evenness, decadal_cv_richness, decadal_cv_evenness, nrow = 3, ncol = 2, labels = c("a)", "b)", "c)", "d)", "e)", "f)"), font.label = list(colour = "black", size = 14, family = "Times New Roman"))
supp_fig


##6) REGRESSION ANALYSIS --- richness and evenness --------
avg_robust_rich_lm <- lm(alpha ~ richness, avg_df)
summary(avg_robust_rich_lm)

avg_robust_even_lm <- lm(alpha~ evenness, avg_df)
summary(avg_robust_even_lm)

avg_cv_rich_lm <- lm(harvest_total_cv ~ richness, avg_df)
summary(avg_cv_rich_lm)

avg_cv_even_lm <- lm(harvest_total_cv ~ evenness, avg_df)
summary(avg_cv_even_lm)

dec_cv_rich_lm <- lm(cv_ph ~ richness_mean, mean_hc_cv)
summary(dec_cv_rich_lm)

dec_cv_even_lm <- lm(cv_ph ~ evenness_mean, mean_hc_cv)
summary(dec_cv_even_lm)




