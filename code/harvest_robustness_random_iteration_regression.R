##Harvest Stability Regression -- Robustness to species loss, random harvest removal iterations

library(tidyverse)
library(ggplot2)
library(ggpubr)

###2) AVERAGE HARVEST OVER TIME -- 
df_1 <- read.csv("data/intermediate_data/average_harvest_phenology_summary_metrics_percapita.csv") %>%
  select(Forest:synchrony_gross)
df_2 <- read.csv("data/intermediate_data/1000_random_removal_slope_mean_sd.csv") %>%
  rename(site = "Community")  %>%
  select(site:slope_sd)

avg_df <- left_join(df_1, df_2, by = c("site"))

#rm(list = ls()[!ls() %in% c("mean_hc_cv", "avg_df")])
##3.1) Robustness vs. SW diversity and coupling
avg_robust_coupling <- ggplot(avg_df, aes(x = logit_sd_0_1, y = slope_mean)) +
  geom_point(size = 3) +
 # geom_smooth(method = "lm", color = "darkred") +
  # scale_color_manual(values = c("black", "cyan3", "orange"))+
  theme_classic() +
  xlab("Habitat Coupling (logit)") +
  ylab("Mean Slope") +
#  xlim(-3.7,-1.43)+
#  ylim(0.1, 0.42)+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank(), legend.position = "none")
avg_robust_coupling

avg_robust_diversity <- ggplot(avg_df, aes(x = sw_diversity, y = slope_mean)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "darkred") +
  #  scale_color_manual(values = c("black", "cyan3", "orange"))+
  theme_classic() +
  xlab("Harvest Diversity (SW)") +
  ylab("Mean Slope") +
#  xlim(1.92, 3.03) + 
#  ylim(0.1, 0.42)+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank(), legend.position = "none")
avg_robust_diversity


avg_robust_richness <- ggplot(avg_df, aes(x = richness, y = slope_mean)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "darkred") +
  #  scale_color_manual(values = c("black", "cyan3", "orange"))+
  theme_classic() +
  xlab("Harvest Richness") +
  ylab("Mean Slope") +
  #  xlim(1.92, 3.03) + 
  #  ylim(0.1, 0.42)+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank(), legend.position = "none")
avg_robust_richness

avg_robust_even <- ggplot(avg_df, aes(x = evenness, y = slope_mean)) +
  geom_point(size = 3) +
 # geom_smooth(method = "lm", color = "darkred") +
  #  scale_color_manual(values = c("black", "cyan3", "orange"))+
  theme_classic() +
  xlab("Harvest Evenness") +
  ylab("Mean Slope") +
  #  xlim(1.92, 3.03) + 
  #  ylim(0.1, 0.42)+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank(), legend.position = "none")
avg_robust_even


##Supplemental figure 
fig_S8 <-  ggarrange(avg_robust_diversity, avg_robust_coupling,  avg_robust_richness, avg_robust_even, nrow = 2, ncol = 2, labels = c("a)", "b)", "c)", "d)"), font.label = list(colour = "black", size = 14, family = "Times New Roman"))
fig_S8
##Regressions

lm_coupling <- lm(slope_mean ~ logit_sd_0_1, avg_df)
summary(lm_coupling)


lm_diversity <- lm(slope_mean ~ sw_diversity, avg_df)
summary(lm_diversity)

lm_richness <- lm(slope_mean ~ richness, avg_df)
summary(lm_richness)

lm_evenness <- lm(slope_mean ~ evenness, avg_df)
summary(lm_evenness)



##3.1) Robustness SD vs. SW diversity and coupling
avg_robust_coupling_sd <- ggplot(avg_df, aes(x = log_1_SD, y = slope_sd)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "darkred") +
  # scale_color_manual(values = c("black", "cyan3", "orange"))+
  theme_classic() +
  xlab("Habitat Coupling (log 1/SD)") +
  ylab("SD slope") +
  #  xlim(-3.7,-1.43)+
  #  ylim(0.1, 0.42)+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank(), legend.position = "none")
avg_robust_coupling_sd

avg_robust_diversity_sd <- ggplot(avg_df, aes(x = sw_diversity, y = slope_sd)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "darkred") +
  #  scale_color_manual(values = c("black", "cyan3", "orange"))+
  theme_classic() +
  xlab("Harvest Diversity (SW)") +
  ylab("SD Slope") +
  #  xlim(1.92, 3.03) + 
  #  ylim(0.1, 0.42)+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank(), legend.position = "none")
avg_robust_diversity_sd


avg_robust_richness_sd <- ggplot(avg_df, aes(x = richness, y = slope_sd)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "darkred") +
  #  scale_color_manual(values = c("black", "cyan3", "orange"))+
  theme_classic() +
  xlab("Harvest Richness") +
  ylab("SD Slope") +
  #  xlim(1.92, 3.03) + 
  #  ylim(0.1, 0.42)+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank(), legend.position = "none")
avg_robust_richness_sd

avg_robust_even_sd <- ggplot(avg_df, aes(x = evenness, y = slope_sd)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "darkred") +
  #  scale_color_manual(values = c("black", "cyan3", "orange"))+
  theme_classic() +
  xlab("Harvest Evenness") +
  ylab("SD Slope") +
  #  xlim(1.92, 3.03) + 
  #  ylim(0.1, 0.42)+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank(), legend.position = "none")
avg_robust_even_sd



##Mean vs SD in slopes

mean_sd <- ggplot(avg_df, aes(x = slope_mean, y = slope_sd)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "darkred") +
  #  scale_color_manual(values = c("black", "cyan3", "orange"))+
  theme_classic() +
  xlab("Mean slope") +
  ylab("SD Slope") +
  #  xlim(1.92, 3.03) + 
  #  ylim(0.1, 0.42)+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank(), legend.position = "none")
mean_sd



######Parameters from random removal, but best fit model (not all linear) ---------------
df_1 <- read.csv("data/intermediate_data/average_harvest_phenology_summary_metrics_percapita.csv") %>%
  select(Forest:synchrony_gross)
df_2 <- read.csv( "data/intermediate_data/random_removal_model_params_percap.csv") %>%
  rename(site = "community")  %>%
  select(term:alpha)

##For now, only select rate of change parameters
df_2 <- df_2 %>%
  filter(term %in% c("x", "scal", "log_alpha")) %>%
  group_by(site, model_type) %>%
  summarise_at(vars(estimate), list(mean_estimate = mean, sd_estimate = sd))

avg_df <- left_join(df_1, df_2, by = c("site")) 

#rm(list = ls()[!ls() %in% c("mean_hc_cv", "avg_df")])
##3.1) Robustness vs. SW diversity and coupling
avg_robust_coupling <- ggplot(avg_df, aes(x = log_1_SD, y = mean_estimate, group = model_type, color = model_type)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "darkred") +
  # scale_color_manual(values = c("black", "cyan3", "orange"))+
  theme_classic() +
  xlab("Habitat Coupling (log 1/SD)") +
  ylab("Mean Rate of Change") +
  #  xlim(-3.7,-1.43)+
  #  ylim(0.1, 0.42)+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank(), legend.position = "right") +
  facet_wrap(~model_type, scale = "free")
avg_robust_coupling

avg_robust_diversity <- ggplot(avg_df, aes(x = sw_diversity, y = mean_estimate, group = model_type, color = model_type)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "darkred") +
  #  scale_color_manual(values = c("black", "cyan3", "orange"))+
  theme_classic() +
  xlab("Harvest Diversity (SW)") +
  ylab("Mean Rate of Change") +
  #  xlim(1.92, 3.03) + 
  #  ylim(0.1, 0.42)+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank(), legend.position = "right") +
  facet_wrap(~model_type, scale = "free")
avg_robust_diversity


avg_robust_richness <- ggplot(avg_df, aes(x = richness, y = mean_estimate, group = model_type, color = model_type)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "darkred") +
  #  scale_color_manual(values = c("black", "cyan3", "orange"))+
  theme_classic() +
  xlab("Harvest Richness") +
  ylab("Mean Rate of Change") +
  #  xlim(1.92, 3.03) + 
  #  ylim(0.1, 0.42)+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank(), legend.position = "right") +
  facet_wrap(~model_type, scale = "free")
avg_robust_richness

avg_robust_even <- ggplot(avg_df, aes(x = evenness, y = mean_estimate, group = model_type, color = model_type)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "darkred") +
  #  scale_color_manual(values = c("black", "cyan3", "orange"))+
  theme_classic() +
  xlab("Harvest Evenness") +
  ylab("Mean Rate of Change") +
  #  xlim(1.92, 3.03) + 
  #  ylim(0.1, 0.42)+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank(), legend.position = "right") +
  facet_wrap(~model_type, scale = "free")
avg_robust_even

##Regressions


lm_coupling <- lm(mean_estimate ~ log_1_SD + model_type, avg_df)
summary(lm_coupling)


lm_diversity <- lm(mean_estimate ~ sw_diversity + model_type, avg_df)
summary(lm_diversity)

lm_richness <- lm(mean_estimate ~ richness + model_type, avg_df)
summary(lm_richness)

lm_evenness <- lm(mean_estimate ~ evenness, avg_df)
summary(lm_evenness)



##3.1) Robustness SD vs. SW diversity and coupling
avg_robust_coupling_sd <- ggplot(avg_df, aes(x = log_1_SD, y = sd_estimate, group = model_type, color = model_type)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "darkred") +
  # scale_color_manual(values = c("black", "cyan3", "orange"))+
  theme_classic() +
  xlab("Habitat Coupling (log 1/SD)") +
  ylab("SD rate of change") +
  #  xlim(-3.7,-1.43)+
  #  ylim(0.1, 0.42)+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank(), legend.position = "right") +
  facet_wrap(~model_type, scale = "free")
avg_robust_coupling_sd

avg_robust_diversity_sd <- ggplot(avg_df, aes(x = sw_diversity, y = sd_estimate, group = model_type, color = model_type)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "darkred") +
  #  scale_color_manual(values = c("black", "cyan3", "orange"))+
  theme_classic() +
  xlab("Harvest Diversity (SW)") +
  ylab("SD Slope") +
  #  xlim(1.92, 3.03) + 
  #  ylim(0.1, 0.42)+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank(), legend.position = "none") +
  facet_wrap(~model_type, scale = "free")
avg_robust_diversity_sd


avg_robust_richness_sd <- ggplot(avg_df, aes(x = richness, y = sd_estimate, group = model_type, color = model_type)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "darkred") +
  #  scale_color_manual(values = c("black", "cyan3", "orange"))+
  theme_classic() +
  xlab("Harvest Richness") +
  ylab("SD Slope") +
  #  xlim(1.92, 3.03) + 
  #  ylim(0.1, 0.42)+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank(), legend.position = "none") +
  facet_wrap(~ model_type, scale = "free")
avg_robust_richness_sd

avg_robust_even_sd <- ggplot(avg_df, aes(x = evenness, y = sd_estimate, group = model_type, color = model_type)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "darkred") +
  #  scale_color_manual(values = c("black", "cyan3", "orange"))+
  theme_classic() +
  xlab("Harvest Evenness") +
  ylab("SD Slope") +
  #  xlim(1.92, 3.03) + 
  #  ylim(0.1, 0.42)+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank(), legend.position = "none") +
  facet_wrap(~model_type, scale = "free")
avg_robust_even_sd



##Mean vs SD in slopes

mean_sd <- ggplot(avg_df, aes(x = mean_estimate, y = sd_estimate, group = model_type, color = model_type)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "darkred") +
  #  scale_color_manual(values = c("black", "cyan3", "orange"))+
  theme_classic() +
  xlab("Mean slope") +
  ylab("SD Slope") +
  #  xlim(1.92, 3.03) + 
  #  ylim(0.1, 0.42)+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14), axis.title.x = element_text(size = 14), text = element_text(family = "Times New Roman"), strip.background = element_blank(), legend.position = "right")
mean_sd




