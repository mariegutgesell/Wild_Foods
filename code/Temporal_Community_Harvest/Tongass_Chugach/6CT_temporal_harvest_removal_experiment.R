##Harvest strength distributions and removal experiment -- community avg across years

library(tidyverse)
library(ggplot2)
library(ggrepel)

##source code that calculates average harvest across time for each community
source("code/Temporal_Community_Harvest/Tongass_Chugach/3CT_calculate_prop_harvest.R")
rm(list = ls()[!ls() %in% c("df_temp_avg")])

##rename to match code
df_comm_avg <- df_temp_avg 

##For each community/year, create rank # based on proportion of total harvest
##first for total harvest
total_harvest_rank <- df_comm_avg %>%
  group_by(Site_Year_Code) %>%
  mutate(harvest_rank = rank(-Total_Harvest_prop, ties.method = "min")) 

##Plot harvest distributions by community
ggplot(total_harvest_rank, aes(x = harvest_rank, y= Total_Harvest_prop, fill = Habitat)) +
  geom_col() +
  scale_fill_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  facet_wrap(~Site_Year_Code, scale = "free")


##Harvest Removal Experiment
results <- data.frame(
  community = character(),
  species_removed = integer(),
  total_harvest = numeric(),
  stringsAsFactors = FALSE
)
# Function to calculate total harvest after removing a particular species

calculate_total_harvest <- function(df, species_to_remove) {
  total_harvest <- sum(df$Total_Harvest_prop)
  removed_harvest <- df$Total_Harvest_prop[df$Lowest_Common_Taxon_Name %in% species_to_remove]
  remaining_harvest <- total_harvest - sum(removed_harvest)
  return(remaining_harvest)
}

# Iterate over each community
communities <- unique(total_harvest_rank$Site_Year_Code)

for (community in communities) {
  cat("Community:", community, "\n")
  
  # Extract harvest data for the current community
  community_data <- subset(total_harvest_rank, Site_Year_Code == community)
  
  # Sort the harvest data within the community from greatest to lowest harvest
  sorted_species <- unique(community_data$Lowest_Common_Taxon_Name[order(-community_data$Total_Harvest_prop)])
  
  # Initialize variables to store results for the current community
  species_removed <- 0
  remaining_harvest <- sum(community_data$Total_Harvest_prop)
  
  # Sequential removals of species and calculation of total harvest
  for (species in sorted_species) {
    remaining_harvest <- calculate_total_harvest(community_data, species)
    species_removed <- species_removed + 1
    # Store results for the current community
    results <- rbind(results, data.frame(community = community, species_removed = species_removed, total_harvest = remaining_harvest))
    community_data <- community_data[community_data$Lowest_Common_Taxon_Name != species, ]
  }
}

##add row for 0 species removed and 100% harvest
total_harvest_prop <- results %>%
  select(community) %>%
  distinct() %>%
  mutate(species_removed = 0) %>%
  mutate(total_harvest = 100)

results_2 <- rbind(results, total_harvest_prop)


##Plot Species Removal experiment
ggplot(results_2, aes(x = species_removed, y = total_harvest, group = community, color = community)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) #+
#theme_classic() +
#  facet_wrap(~community, scale = "free")


#library(geomtextpath)
##linear 
ggplot(results_2, aes(x = species_removed, y = total_harvest, group = community)) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_classic() +
  ylab("Proportion Total Harvest Remaining") +
  xlab("Number of Species Removed") 

##Plot harvest distributions and line of removal


##non-linear
ggplot(results_2, aes(x = species_removed, y = total_harvest, group = community)) +
  geom_point()+
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  theme_classic() +
  ylab("Proportion Total Harvest Remaining") +
  xlab("Number of Species Removed") 



##extract slope and relate to species richness --------NON-LINEAR --------------
##testing non-linear regression
##using a self-starting function, which is a special function for curve fitting that guesses its own start parameters 
##(using nls w/ exponential decay can lead to error of singular gradient if bad starting values are picked (and in our case will have unique starting values for each community))
##General exponential decay equation: y(t) ~ yf + (y0 - yf)e ^-at
##Exponential decay parameters: y0 = starting value, yf = final value, a = rate of change, t = time (or in our case # of species removed)

#nls_angoon <- nls(total_harvest ~ yf + (y0 - yf) * exp(-alpha * species_removed), data = angoon, start = list(y0 = 100, yf = 0, alpha = 1))
##this produces error of singular gradient, so using a selfStart function that wil guess its own start parameters 
##The asymptotic regression function, SSasymp is equivalent to exponential decay function, only difference is that instead of fitting the rate constant a directly, it searches for the logarithm of a
##Example SSasymp for angoon 


library(broom)

##Now doing for all communities
nls_all <- results_2 %>%
  nest(-community) %>%
  mutate(
    fit = map(data, ~nls(total_harvest ~ SSasymp(species_removed, yf, y0, log_alpha), data = .)),
    tidied = map(fit, tidy),
    augmented = map(fit, augment)
  )

##generate table of fit parameters: y0, yf, alpha
nls_all_param <- nls_all %>%
  unnest(tidied) %>%
  select(community, term, estimate) %>%
  spread(term, estimate) %>%
  mutate(alpha = exp(log_alpha))

##Plot curves for each community
augmented <- nls_all %>%
  unnest(augmented)

qplot(species_removed, total_harvest, data = augmented, geom = 'point', colour = community) +
  geom_line(aes(y=.fitted)) +
  theme(legend.position = "none") +
  facet_wrap(~community)

##Join non-linear parameters to harvest diversity metrics --------------
harvest_div <- read.csv("data/intermediate_data/temporal_survey_harvest_diversity_metrics.csv")
#rm(list = ls()[!ls() %in% c("nls_all_param", "harvest_div")])

nls_all_param <- nls_all_param %>%
  rename(Site_Year_Code = "community")

comm_div_2 <- left_join(harvest_div, nls_all_param, by = "Site_Year_Code") 
##save for future analysis
write.csv(comm_div_2, "data/intermediate_data/temporal_harvest_removal_results.csv")
##Relating harvest diversity indices with alpha (rate of harvest decline)
ggplot(comm_div_2, aes(x = sw_diversity, y = alpha)) +
  geom_point(aes(color = Site)) +
#  geom_line()+
  geom_smooth(method = "lm", se = FALSE)+
  theme_classic() +
  ylab("Rate of Harvest Decline") +
  xlab("Harvest Diversity (SW)") +
#  ylim(0.05, 0.42) +
  xlim(1, 3.5) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"))# +
  #facet_wrap(~Site)



ggplot(comm_div_2, aes(x =richness, y = alpha, group = Site)) + 
  geom_point()+
  geom_line() +
  # geom_smooth(method = "lm")+
  theme_classic() +
  ylab("Rate of Harvest Decline") +
  xlab("Harvest Richness") +
  #  ylim(0.05, 0.42) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman")) +
  facet_wrap(~Site)


ggplot(comm_div_2, aes(x =Year, y = alpha, group = Site)) + 
  geom_point()+
  geom_line() +
  # geom_smooth(method = "lm")+
  theme_classic() +
  ylab("Rate of Harvest Decline") +
  xlab("Time") +
  #  ylim(0.05, 0.42) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman")) +
  facet_wrap(~Site)


ggplot(comm_div_2, aes(x =Year, y = sw_diversity, group = Site)) + 
  geom_point()+
  geom_line() +
  # geom_smooth(method = "lm")+
  theme_classic() +
  ylab("Rate of Harvest Decline") +
  xlab("Harvest Diversity (SW)") +
  #  ylim(0.05, 0.42) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman")) #+

#  facet_wrap(~Site)

#formula = y ~ x + I(x^2)
richness_lm <- lm(alpha ~ richness, data = comm_div_2)
summary(richness_lm)

richness_lm2 <- lm(alpha ~ richness + Forest, data = comm_div_2)
summary(richness_lm2)

ggplot(comm_div_2, aes(x = sw_diversity, y = alpha)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkred")+
  theme_classic() +
  ylab("Rate of Harvest Decline") +
  xlab("Harvest Diversity (SW)") +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"))


sw_lm <- lm(alpha ~ sw_diversity, data = comm_div_2)
summary(sw_lm)

sw_lm2 <- lm(alpha ~ sw_diversity + Forest, data = comm_div_2)
summary(sw_lm2)

ggplot(comm_div_2, aes(x = evenness, y = alpha)) +
  geom_point(aes(color = Forest)) +
  geom_smooth(method = "lm")+
  theme_classic() +
  ylab("Rate of Harvest Decline") +
  xlab("Harvest Evenness (Pielou)") +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"))


even_lm <- lm(alpha ~ evenness, data = comm_div_2)
summary(even_lm)

even_lm2 <- lm(alpha ~ evenness + Forest, data = comm_div_2)
summary(even_lm2)

##habitat diversity vs. rate of harvest loss 
ggplot(comm_div_2, aes(x = sw_div_h, y = alpha)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic() +
  ylab("Rate of Harvest Decline") +
  xlab("Habitat Diversity (SW)")

sw_lm_h <- lm(alpha ~ sw_div_h, data = comm_div_2)
summary(sw_lm_h)
##this is very non-normally distributed, would need to transform SW diversity first

ggplot(comm_div_2, aes(x = evenness_h, y = alpha)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic() +
  ylab("Rate of Harvest Decline") +
  xlab("Habitat Evenness (Pielou)")

even_lm_h_1 <- lm(alpha ~ evenness_h, data = comm_div_2)
summary(even_lm_h_1)


ggplot(comm_div_2, aes(x = evenness_h_log, y = alpha)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic() +
  ylab("Rate of Harvest Decline") +
  xlab("Habitat Evenness (log Pielou)")

even_lm_h_2 <- lm(alpha ~ evenness_h_log, data = comm_div_2)
summary(even_lm_h_2)


ggplot(comm_div_2, aes(x = sd, y = alpha)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic() +
  ylab("Rate of Harvest Decline") +
  xlab("Habitat Evenness (SD of harvest proportion)")

even_lm_h_3 <- lm(alpha ~ sd, data = comm_div_2)
summary(even_lm_h_3)


ggplot(comm_div_2, aes(x = sqrt(evenness_h), y = alpha)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic() +
  ylab("Rate of Harvest Decline") +
  xlab("Habitat Evenness (sqrt Pielou)")




ggplot(comm_div_2, aes(x = sqrt(sd), y = alpha)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic() +
  ylab("Rate of Harvest Decline") +
  xlab("Habitat Evenness (sqrt SD of harvest proportion)")

even_lm_h_4 <- lm(alpha ~ sqrt(sd), data = comm_div_2)
summary(even_lm_h_4)

ggplot(comm_div_2, aes(x = 1/log(sd), y = alpha)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic() +
  ylab("Rate of Harvest Decline") +
  xlab("Habitat Coupling (1/logSD)") +
  ylim(0.05, 0.42) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"))


even_lm_h_5 <- lm(alpha ~ log(sd), data = comm_div_2)
summary(even_lm_h_5)

##testing 1/logSD
comm_div_2 <- comm_div_2 %>%
  mutate(`1_logSD` = 1/(log(sd))) %>%
  mutate(log_1_SD = log(1/sd))

ggplot(comm_div_2, aes(x = log_1_SD, y = alpha)) +
  geom_point(aes(color = Forest)) +
  geom_smooth(method = "lm")+
  theme_classic() +
  ylab("Rate of Harvest Decline") +
  xlab("Habitat Coupling (log 1/SD)") +
  ylim(0.05, 0.42) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"))


even_lm_h_5 <- lm(alpha ~ log_1_SD, data = comm_div_2)
summary(even_lm_h_5)


ggplot(comm_div_2, aes(x = Year, y = log_1_SD, color = Site)) +
  geom_point() +
  #geom_line()+
  geom_smooth(method = "lm", se = FALSE)+
  theme_classic() +
  xlab("Year") +
  ylab("Habitat Coupling (log 1/SD)") +
#  ylim(0.05, 0.42) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman")) #+
 # facet_wrap(~Site)

##harvest evenness vs. habitat eveneess
ggplot(comm_div_2, aes(x = evenness, y = evenness_h)) +
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic() +
  xlab("Harvest Eveness") +
  ylab("Habitat Evenness (Pielou)")


ggplot(comm_div_2, aes(x = evenness, y = sd)) +
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic() +
  xlab("Harvest Eveness") +
  ylab("Habitat Evenness (SD)")
evenness_sd_lm <- lm(sd ~ evenness, data = comm_div_2)
summary(evenness_sd_lm)

ggplot(comm_div_2, aes(x = sd, y = richness)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic()


comm_div_2 %>%
  filter(Site != "Klukwan") %>%
  ggplot(aes(x = sd, y = sw_diversity)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic()

