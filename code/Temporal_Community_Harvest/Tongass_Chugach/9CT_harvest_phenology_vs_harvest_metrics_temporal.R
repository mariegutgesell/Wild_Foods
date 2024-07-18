##Compare harvest distributions stability and synchrony to diversity metrics 

library(tidyverse)

##Read in whichever harvest distributions (i.e., based on proportions, total lbs harvested or percapita)
##Proportion of total harvest
simulated_harvest_df <- read.csv("data/intermediate_data/temporal_survey_simulated_harvest_distributions_harvest_proportion.csv")

##Total lbs harvested
#simulated_harvest_df <- read.csv()

##Percapita lbs harvested
#simulated_harvest_df <- read.csv()


##Plotting all distributions for each community
ggplot(simulated_harvest_df, aes(x = date, y = harvest_amount, group = habitat, color = habitat)) +
  geom_line() +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Date", y = "Harvest Amount (%)", title = "Simulated Harvest Amounts Across Harvest Season") +
  facet_wrap(~site, scale = "free")




##Calculating total harvest per date -----------------
total_simulated_harvest <- simulated_harvest_df %>%
  filter(!is.na(harvest_amount)) %>%
  group_by(site, date) %>%
  summarise_at(vars(harvest_amount), list(harvest_total = sum))


ggplot(total_simulated_harvest, aes(x = date, y = harvest_total, group = site, color = site)) +
  geom_line() +
  labs(x = "Date", y = "Harvest Amount (%)", title = "Total Harvest Amounts Across Harvest Season") +
  theme_classic()

total_harvest_all <- total_simulated_harvest %>%
  filter(!is.na(harvest_total)) %>%
  group_by(date) %>%
  summarise_at(vars(harvest_total), list(harvest_mean = mean))

ggplot() +
  geom_path(data = total_harvest_all, aes(x = date, y = harvest_mean), color = "red", size = 3) +
  geom_path(data = total_simulated_harvest, aes(x = date, y = harvest_total, group = site), color = "black") +
  labs(x = "Date", y = "Harvest Amount (%)", title = "Simulated Harvest Amounts Across Harvest Season") +
  theme_classic()



ggplot() +
  geom_line(data = simulated_harvest_df, aes(x = date, y = harvest_amount, group = habitat, color = habitat)) +
  geom_line(data = total_simulated_harvest, aes(x = date, y = harvest_total)) +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Day of Year", y = "Harvest Amount (%)", title = "Simulated Harvest Amounts Across Harvest Season") +
  facet_wrap(~site)




##Plot harvest in order of increasing harvest date for each community
total_simulated_harvest <- total_simulated_harvest %>%
  group_by(site) %>%
  mutate(harvest_day = rank(harvest_total, ties.method = "min"))


ggplot(total_simulated_harvest, aes(x = harvest_day, y = harvest_total, group = site)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0.2776, linetype = "dashed", color = "red") +
  # scale_x_discrete(labels = function(x) gsub("date", "", x)) +
  facet_wrap(~site, scale = "free")


100/12 ##=8.33, so even harvest would mean need 8.33% of total harvest per month
8.33/30 ##=0.2776,  so even harvest across year would mean 0.~2776% of harvest per day
0.2*365

##Starting with calculating % of year where daily harvest is below 0.3% of total harvest (0.3% daily harvest every day would give equal harvest across time)
##0.1 as arbitrary proportion -- daily harvest makes up less than 0.1% of total annual harvest 
harvest_threshold <- total_simulated_harvest %>%
  group_by(site) %>%
  mutate(below_harvest_threshold = ifelse(harvest_total < quantile(0.2776), "Y", "N")) %>%
  filter(below_harvest_threshold == "Y") %>%
  count() %>%
  mutate(prop_days_below_threshold = (n/365)*100) 

##Calculate Synchrony between harvest taxa across season ---------------
library(codyn)
##add day of year 1-365 for each date 
dates <- simulated_harvest_df %>%
  select(date) %>%
  distinct() %>%
  mutate(date_num = 1:365)

simulated_harvest_df <- left_join(simulated_harvest_df, dates, by = "date")
simulated_harvest_df$date_num <- as.numeric(simulated_harvest_df$date_num)

##Synchrony using Loreau metric
synchrony_loreau <- synchrony(df = simulated_harvest_df, time.var = "date_num", species.var = "species", abundance.var = "harvest_amount", replicate.var = "site", metric = "Loreau") %>%
  dplyr::rename(synchrony_loreau = "synchrony")

synchrony_gross <- synchrony(df = simulated_harvest_df, time.var = "date_num", species.var = "species", abundance.var = "harvest_amount", replicate.var = "site", metric = "Gross") %>%
  dplyr::rename(synchrony_gross = "synchrony")


synchrony_df <- left_join(synchrony_loreau, synchrony_gross, by = "site")

ggplot(synchrony_df, aes(x = synchrony_loreau, y = synchrony_gross)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()
sync_lm_l_g <- lm(synchrony_gross ~ synchrony_loreau, synchrony_df)
summary(sync_lm_l_g)

##see bluthgen et al., 2016 for additional synchrony metrics, their approach was to calcualte all, look at relationships b/w and see if conclusions were same 


##Calculate variability metrics
##calculating sd and CV
sim_harv_cv <- total_simulated_harvest %>%
  group_by(site) %>%
  summarise(harvest_total_mean = mean(harvest_total), harvest_total_sd = sd(harvest_total), harvest_total_cv = harvest_total_sd/harvest_total_mean)

ggplot(sim_harv_cv, aes(x = reorder(site, -harvest_total_cv), y = harvest_total_cv)) +
  geom_col() +
  theme(legend.position = "none", axis.text.x = element_text(size = 8, angle = 45, hjust = 1), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15))

ggplot(sim_harv_cv, aes(x = reorder(site, -harvest_total_sd), y = harvest_total_sd)) +
  geom_col() +
  theme(legend.position = "none", axis.text.x = element_text(size = 8, angle = 45, hjust = 1), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15))

ggplot(sim_harv_cv, aes(x = reorder(site, -harvest_total_mean), y = harvest_total_mean)) +
  geom_col() +
  theme(legend.position = "none", axis.text.x = element_text(size = 8, angle = 45, hjust = 1), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15))


##compare seasonal CV/SD to harvest structure metrics --------------
comm_div <- read.csv("data/intermediate_data/temporal_survey_harvest_diversity_metrics.csv") %>%
  rename(site = "Site_Year_Code")
#rm(list = ls()[!ls() %in% c("comm_div_2", "h_df_all")])

comm_dv_cv <- left_join(comm_div, sim_harv_cv, by = "site") %>%
  left_join(harvest_threshold, by = "site")%>%
  left_join(synchrony_df, by = "site")

##CV vs. harvest diversity metrics ---------------
##testing if relationships hold w/o klukwan
#comm_dv_cv <- comm_dv_cv %>%
#  filter(site != "Klukwan")

ggplot(comm_dv_cv, aes(x = richness, y = harvest_total_cv)) +
  geom_point(aes(color = Forest)) +
  #geom_smooth(method = "lm") +
  theme_classic() +
  labs(x = "Harvest Richness", y = "Total Harvest CV") +
  ylim(0.45, 1.6)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"))


richness_cv_lm <- lm(harvest_total_cv ~ richness, comm_dv_cv)
summary(richness_cv_lm)

ggplot(comm_dv_cv, aes(x = richness, y = harvest_total_cv, color = Forest)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() +
  labs(x = "Harvest Richness", y = "Total Harvest CV") +
  ylim(0.45, 1.6)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"))



richness_cv_lm2 <- lm(harvest_total_cv ~ richness + Forest, comm_dv_cv)
summary(richness_cv_lm2)

ggplot(comm_dv_cv, aes(x = sw_diversity, y = harvest_total_cv)) +
  geom_point(aes(color = Forest)) +
  geom_smooth(method = "lm") +
  theme_classic() +
  labs(x = "Harvest Diversity (SW)", y = "Total Harvest CV") +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"))

sw_cv_lm <- lm(harvest_total_cv ~ sw_diversity, comm_dv_cv)
summary(sw_cv_lm)

ggplot(comm_dv_cv, aes(x = sw_diversity, y = harvest_total_cv, color = Forest)) +
  geom_point(aes(color = Forest)) +
  geom_smooth(method = "lm") +
  theme_classic()

sw_cv_lm2 <- lm(harvest_total_cv ~ sw_diversity + Forest, comm_dv_cv)
summary(sw_cv_lm2)

ggplot(comm_dv_cv, aes(x = evenness, y = harvest_total_cv)) +
  geom_point(aes(color = Forest)) +
  geom_smooth(method = "lm") +
  theme_classic() +
  labs(x = "Harvest Evenness (Pielou)", y = "Total Harvest CV") +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"))

even_cv_lm <- lm(harvest_total_cv ~ evenness, comm_dv_cv)
summary(even_cv_lm)

ggplot(comm_dv_cv, aes(x = evenness, y = harvest_total_cv, color = Forest)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()
even_cv_lm2 <- lm(harvest_total_cv ~ evenness + Forest, comm_dv_cv)
summary(even_cv_lm2)


ggplot(comm_dv_cv, aes(x = log(1/sd), y = harvest_total_cv)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() +
  labs(x = "Habitat Coupling (log 1/SD)", y = "Total Harvest CV") +
  ylim(0.45, 1.6)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"))


sd_h_cv_lm <- lm(harvest_total_cv ~ log(sd), comm_dv_cv)
summary(sd_h_cv_lm)

##test stats
comm_dv_cv <- comm_dv_cv %>%
  mutate(`1_logSD` = 1/(log(sd))) %>%
  mutate(log_1_SD = log(1/sd))

ggplot(comm_dv_cv, aes(x = log_1_SD, y = 1/harvest_total_cv)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkred") +
  theme_classic() +
  labs(x = "Habitat Coupling (log 1/SD)", y = "Total Harvest CV") +
  ylim(0.45, 1.6)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"))


sd_h_cv_lm <- lm((1/harvest_total_cv) ~ log_1_SD, comm_dv_cv)
summary(sd_h_cv_lm)


ggplot(comm_dv_cv, aes(x = log_1_SD, y = harvest_total_cv, color = Forest)) +
  geom_point(aes(color = Forest)) +
  geom_smooth(method = "lm") +
  theme_classic() +
  labs(x = "Habitat Coupling (log 1/SD)", y = "Total Harvest CV") +
  ylim(0.45, 1.6)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"))


sd_h_cv_lm2 <- lm(harvest_total_cv ~ log_1_SD + Forest, comm_dv_cv)

summary(sd_h_cv_lm2)


ggplot(comm_dv_cv, aes(x = evenness_h, y = harvest_total_cv)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

even_h_cv_lm <- lm(harvest_total_cv ~ evenness_h, comm_dv_cv)
summary(even_h_cv_lm)

ggplot(comm_dv_cv, aes(x = prop_days_below_threshold, y = harvest_total_cv))+
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

thresh_cv_lm <- lm(harvest_total_cv ~ prop_days_below_threshold, comm_dv_cv)
summary(thresh_cv_lm)

##save dataframe of harvest characteristics and phenology metrics
write.csv(comm_dv_cv, "data/intermediate_data/temporal_harvest_phenology_summary_metrics.csv")

##SD vs. harvest diversity metrics ----------
ggplot(comm_dv_cv, aes(x = richness, y = harvest_total_sd)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() 

richness_sd_lm <- lm(harvest_total_sd ~ richness, comm_dv_cv)
summary(richness_sd_lm)

ggplot(comm_dv_cv, aes(x = sw_diversity, y = harvest_total_sd)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()
sw_sd_lm <- lm(harvest_total_sd ~ sw_diversity, comm_dv_cv)
summary(sw_sd_lm)


ggplot(comm_dv_cv, aes(x = evenness, y = harvest_total_sd)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()
even_sd_lm <- lm(harvest_total_sd ~ evenness, comm_dv_cv)
summary(even_sd_lm)

ggplot(comm_dv_cv, aes(x = log(sd), y = harvest_total_sd)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

sd_h_sd_lm <- lm(harvest_total_sd ~ sd, comm_dv_cv)
summary(sd_h_sd_lm)

ggplot(comm_dv_cv, aes(x = evenness_h, y = harvest_total_sd)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

even_h_sd_lm <- lm(harvest_total_sd ~ evenness_h, comm_dv_cv)
summary(even_h_sd_lm)

##Mean vs. harvest diversity metrics ----------
ggplot(comm_dv_cv, aes(x = richness, y = harvest_total_mean)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() 

richness_mean_lm <- lm(harvest_total_mean ~ richness, comm_dv_cv)
summary(richness_mean_lm)

ggplot(comm_dv_cv, aes(x = sw_diversity, y = harvest_total_mean)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()
sw_mean_lm <- lm(harvest_total_mean ~ sw_diversity, comm_dv_cv)
summary(sw_mean_lm)


ggplot(comm_dv_cv, aes(x = evenness, y = harvest_total_mean)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()
even_mean_lm <- lm(harvest_total_mean ~ evenness, comm_dv_cv)
summary(even_mean_lm)

ggplot(comm_dv_cv, aes(x = log(sd), y = harvest_total_mean)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

sd_h_mean_lm <- lm(harvest_total_mean ~ sd, comm_dv_cv)
summary(sd_h_mean_lm)

ggplot(comm_dv_cv, aes(x = evenness_h, y = harvest_total_mean)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

even_h_mean_lm <- lm(harvest_total_mean ~ evenness_h, comm_dv_cv)
summary(even_h_mean_lm)

##Days below threshold vs. harvest metrics ---------
ggplot(comm_dv_cv, aes(x = richness, y = prop_days_below_threshold)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() 

richness_thresh_lm <- lm(prop_days_below_threshold ~ richness, comm_dv_cv)
summary(richness_thresh_lm)

ggplot(comm_dv_cv, aes(x = sw_diversity, y = prop_days_below_threshold)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()
sw_thresh_lm <- lm(prop_days_below_threshold ~ sw_diversity, comm_dv_cv)
summary(sw_thresh_lm)


ggplot(comm_dv_cv, aes(x = evenness, y = prop_days_below_threshold)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()
even_thresh_lm <- lm(prop_days_below_threshold ~ evenness, comm_dv_cv)
summary(even_thresh_lm)

ggplot(comm_dv_cv, aes(x = log(sd), y = prop_days_below_threshold)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

sd_h_thresh_lm <- lm(prop_days_below_threshold ~ log(sd), comm_dv_cv)
summary(sd_h_thresh_lm)

ggplot(comm_dv_cv, aes(x = evenness_h, y = prop_days_below_threshold)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

even_h_thresh_lm <- lm(prop_days_below_threshold ~ evenness_h, comm_dv_cv)
summary(even_h_thresh_lm)


##Plotting total harvest by habitat
total_simulated_harvest_habitat <- simulated_harvest_df %>%
  filter(!is.na(harvest_amount)) %>%
  group_by(site, date, habitat) %>%
  summarise_at(vars(harvest_amount), list(harvest_total = sum))


ggplot(total_simulated_harvest_habitat, aes(x = date, y = harvest_total, group = site, color = habitat)) +
  geom_line() +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Date", y = "Harvest Amount (%)", title = "Total Harvest Amounts Across Harvest Season") +
  theme_classic() +
  facet_wrap(~site)



##Synchrony (Gross metric) vs. Diversity/Harvesting Metrics ----
ggplot(comm_dv_cv, aes(x = richness, y = synchrony_gross)) +
  geom_point(aes(color = Forest)) +
  geom_smooth(method = "lm") +
  theme_classic() 

richness_sync_lm <- lm(synchrony_gross ~ richness, comm_dv_cv)
summary(richness_sync_lm)

ggplot(comm_dv_cv, aes(x = sw_diversity, y = synchrony_gross)) +
  geom_point(aes(color = Forest)) +
  geom_smooth(method = "lm") +
  theme_classic()
sw_sync_lm <- lm(synchrony_gross ~ sw_diversity, comm_dv_cv)
summary(sw_sync_lm)


ggplot(comm_dv_cv, aes(x = evenness, y = synchrony_gross)) +
  geom_point(aes(color = Forest)) +
  geom_smooth(method = "lm") +
  theme_classic()
even_sync_lm <- lm(synchrony_gross ~ evenness, comm_dv_cv)
summary(even_sync_lm)

ggplot(comm_dv_cv, aes(x = log_1_SD, y = synchrony_gross)) +
  geom_point(aes(color = Forest)) +
  geom_smooth(method = "lm") +
  theme_classic()

sd_h_sync_lm <- lm(synchrony_gross ~ log_1_SD, comm_dv_cv)
summary(sd_h_sync_lm)

ggplot(comm_dv_cv, aes(x = evenness_h, y = synchrony_gross)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

even_h_sync_lm <- lm(synchrony_gross ~ evenness_h, comm_dv_cv)
summary(even_h_sync_lm)


ggplot(comm_dv_cv, aes(y = harvest_total_cv, x = synchrony_gross)) +
  geom_point(aes(color = Forest)) +
  geom_smooth(method = "lm") +
  theme_classic()

cv_sync_lm <- lm(harvest_total_cv ~ synchrony_gross, comm_dv_cv)
summary(cv_sync_lm)

sy

##Synchrony (loreau metric) vs. Diversity/Harvesting Metrics ----
ggplot(comm_dv_cv, aes(x = richness, y = synchrony_loreau)) +
  geom_point(aes(color = Forest)) +
  geom_smooth(method = "lm") +
  theme_classic() 

richness_sync_lm <- lm(synchrony_loreau ~ richness, comm_dv_cv)
summary(richness_sync_lm)

ggplot(comm_dv_cv, aes(x = sw_diversity, y = synchrony_loreau)) +
  geom_point(aes(color = Forest)) +
  geom_smooth(method = "lm") +
  theme_classic() +
  labs(x = "Harvest Diversity (SW)", y = "Synchrony (Loreau)") +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"))

sw_sync_lm <- lm(synchrony_loreau ~ sw_diversity, comm_dv_cv)
summary(sw_sync_lm)


ggplot(comm_dv_cv, aes(x = evenness, y = synchrony_loreau)) +
  geom_point(aes(color = Forest)) +
  geom_smooth(method = "lm") +
  theme_classic()
even_sync_lm <- lm(synchrony_loreau ~ evenness, comm_dv_cv)
summary(even_sync_lm)

ggplot(comm_dv_cv, aes(x = log_1_SD, y = synchrony_loreau)) +
  geom_point(aes(color = Forest)) +
  geom_smooth(method = "lm") +
  theme_classic()+
  labs(x = "Habitat Coupling (log 1/SD)", y = "Synchrony (Loreau)") +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"))


sd_h_sync_lm <- lm(synchrony_loreau ~ log_1_SD, comm_dv_cv)
summary(sd_h_sync_lm)

ggplot(comm_dv_cv, aes(x = evenness_h, y = synchrony_loreau)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

even_h_sync_lm <- lm(synchrony_loreau ~ evenness_h, comm_dv_cv)
summary(even_h_sync_lm)

ggplot(comm_dv_cv, aes(y = harvest_total_cv, x = synchrony_loreau)) +
  geom_point(aes(color = Forest)) +
  geom_smooth(method = "lm") +
  theme_classic()

cv_sync_lm_2 <- lm(harvest_total_cv ~ synchrony_loreau, comm_dv_cv)
summary(cv_sync_lm_2)



##Habitat coupling vs. Richness
ggplot(comm_dv_cv, aes(y =log_1_SD, x = sw_diversity)) +
  geom_point(aes(color = Forest)) +
  geom_smooth(method = "lm") +
  theme_classic()
sd_sw <- lm(log_1_SD ~ sw_diversity, comm_dv_cv)
summary(sd_sw)

ggplot(comm_dv_cv, aes(y =log_1_SD, x = richness)) +
  geom_point(aes(color = Forest)) +
  geom_smooth(method = "lm") +
  theme_classic()
sd_rich <- lm(log_1_SD ~ richness, comm_dv_cv)
summary(sd_rich)


ggplot(comm_dv_cv, aes(y =log_1_SD, x = evenness)) +
  geom_point(aes(color = Forest)) +
  geom_smooth(method = "lm") +
  theme_classic()
sd_even <- lm(log_1_SD ~ evenness, comm_dv_cv)
summary(sd_even)


##Correlation matrix
library(Hmisc)
library(corrplot)
m1 <- comm_dv_cv %>%
  select(richness, sw_diversity, evenness, harvest_total_mean, harvest_total_sd, harvest_total_cv, prop_days_below_threshold, synchrony_gross, synchrony_loreau, log_1_SD)
str(m1)

corr_matrix <- cor(m1)
str(corr_matrix)
corrplot(corr_matrix, method = "number")

m2 <- rcorr(as.matrix(m1), type = c("pearson", "spearman"))

pval <- as.data.frame(m2[["P"]])

##Multiple linear regressions -- 
mlr1 <- lm(harvest_total_cv ~ sw_diversity + log_1_SD, comm_dv_cv)
summary(mlr1)

mlr2 <- lm(harvest_total_cv ~ sw_diversity + log_1_SD + synchrony_loreau, comm_dv_cv)
summary(mlr2)

mlr3 <- lm(harvest_total_cv ~ sw_diversity + log_1_SD + richness + evenness + synchrony_loreau, comm_dv_cv)
summary(mlr3)

mlr4 <- lm(harvest_total_cv ~ log_1_SD + synchrony_loreau, comm_dv_cv)
summary(mlr4)


##***results are different whether using loreau or gross metric...need to think about this carefully 

###Richness vs. 

##Remove species based on species rank, and measure influence on CV, SD and mean -----------
##For each community/year, create rank # based on proportion of total harvest
##first for total harvest
total_harvest_rank <- df_comm_avg %>%
  group_by(Site) %>%
  mutate(harvest_rank = rank(-Total_Harvest_prop, ties.method = "min")) %>%
  rename(site = "Site", habitat = "Habitat", species = "Lowest_Common_Taxon_Name") %>%
  select(site, habitat, species, harvest_rank)

simulated_harvest_prop_df <- left_join(simulated_harvest_prop_df, total_harvest_rank, by = c("site", "habitat", "species")) 

# Harvest Removal Experiment
results <- data.frame(
  community = character(),
  species_removed = integer(),
  harvest_cv = numeric(),
  harvest_sd = numeric(),
  harvest_mean = numeric(),
  stringsAsFactors = FALSE
)


# Function to calculate daily total harvest
calculate_daily_total <- function(df) {
  daily_totals <- aggregate(harvest_amount ~ date, data = df, sum)
  return(daily_totals)
}

# Function to calculate mean, sd, and cv of daily total harvest
calculate_daily_stats <- function(daily_totals) {
  harvest_mean <- mean(daily_totals$harvest_amount)
  harvest_sd <- sd(daily_totals$harvest_amount)
  harvest_cv <- harvest_sd / harvest_mean
  return(list(mean = harvest_mean, sd = harvest_sd, cv = harvest_cv))
}

# Iterate over each community

communities <- unique(simulated_harvest_prop_df$site)

for (community in communities) {
  cat("Community:", community, "\n")
  
  # Extract harvest data for the current community
  community_data <- subset(simulated_harvest_prop_df, site == community)
  
  # Sort the harvest data within the community from greatest to lowest harvest rank
  sorted_species <- unique(community_data$species[order(-community_data$harvest_rank)])
  
  # Calculate daily total harvest
  daily_totals <- calculate_daily_total(community_data)
  # Calculate mean, sd, and cv of daily total harvest
  stats <- calculate_daily_stats(daily_totals)
  
  # Calculate initial mean, sd, and cv
  initial_totals <- calculate_daily_total(community_data)
  initial_stats <- calculate_daily_stats(initial_totals)
  
  # Initialize variables to store results for the current community
  species_removed <- 0
  
  # Store initial statistics
  results <- rbind(results, data.frame(
    community = community,
    species_removed = species_removed,
    harvest_mean = initial_stats$mean,
    harvest_sd = initial_stats$sd,
    harvest_cv = initial_stats$cv
  ))
  
  # Sequential removals of species and calculation of total harvest
  for (species in sorted_species) {
    community_data_filtered <- community_data[community_data$species != species, ]
    daily_totals <- calculate_daily_total(community_data)
    # Calculate mean, sd, and cv of daily total harvest
    stats <- calculate_daily_stats(daily_totals)
    
    # Update variables for the next iteration
    species_removed <- species_removed + 1
    
    # Store results for the current community after species removal
    results <- rbind(results, data.frame(
      community = community,
      species_removed = species_removed,
      harvest_mean = stats$mean,
      harvest_sd = stats$sd,
      harvest_cv = stats$cv
    ))
  }
}



##Plot changes in CV with species removed
ggplot(results, aes(x = species_removed, y = harvest_cv, group = community)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~community)




