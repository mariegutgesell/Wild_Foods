##Harvest strength distributions and removal experiment -- community avg across years

library(tidyverse)
library(ggplot2)
library(ggrepel)

##source code that calculates average harvest across time for each community
source("code/Average_Community_Harvest/Tongass/3T_calculate_avg_community_harvest.R")
rm(list = ls()[!ls() %in% c("df_comm_avg")])

df_comm_avg <- df_comm_avg %>%
  filter(Total_Harvest_prop != 0)

##For each community/year, create rank # based on proportion of total harvest
##first for total harvest
total_harvest_rank <- df_comm_avg %>%
  group_by(Site) %>%
  mutate(harvest_rank = rank(-Total_Harvest_prop, ties.method = "min")) 

##Plot harvest distributions by community
ggplot(total_harvest_rank, aes(x = harvest_rank, y= Total_Harvest_prop, fill = Habitat)) +
  geom_col() +
  scale_fill_manual(values = c("salmon", "darkblue", "brown", "darkgreen"))+
  facet_wrap(~Site, scale = "free")

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
communities <- unique(total_harvest_rank$Site)

for (community in communities) {
  cat("Community:", community, "\n")
  
  # Extract harvest data for the current community
  community_data <- subset(total_harvest_rank, Site == community)
  
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



##non-linear
ggplot(results_2, aes(x = species_removed, y = total_harvest)) +
  geom_point()+
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  theme_classic() +
  ylab("Proportion Total Harvest Remaining") +
  xlab("Number of Species Removed") 

##extract slope and relate to species richness --------LINEAR
# List to store regression results
result_list <- split(results_2, results_2$community)
regression_results <- list()

# Perform linear regression for each community
for (community in names(result_list)) {
  community_df <- result_list[[community]]
  regression <- lm(total_harvest ~ species_removed, data = community_df)
  regression_results[[community]] <- summary(regression)
}

# Print regression results
for (community in names(regression_results)) {
  cat("Community:", community, "\n")
  print(regression_results[[community]])
  cat("\n")
}


# Initialize an empty dataframe to store results
results_df <- data.frame(Community = character(),
                         Coefficients = character(),
                         Value = numeric(),
                         stringsAsFactors = FALSE)

# Loop through each community's regression result
for (community in names(regression_results)) {
  summary <- regression_results[[community]]
  coefficients <- summary$coefficients
  
  # Store coefficients in the dataframe
  for (i in 1:nrow(coefficients)) {
    results_df <- rbind(results_df, 
                        data.frame(Community = community,
                                   Coefficients = rownames(coefficients)[i],
                                   Value = coefficients[i, "Estimate"]))
  }
}

##comparing slopes 
slopes <- results_df %>%
  filter(Coefficients == "species_removed") %>%
  select(Community, Value) %>%
  rename(slope = "Value", Site = "Community")

##calculate richness, sw diversity and evenness
richness <- df_comm_avg %>%
  ungroup() %>%
  select(Site, Lowest_Common_Taxon_Name) %>%
  group_by(Site) %>%
  count() %>%
  rename(richness = "n")

df_comm_wide <- df_comm_avg %>%
  ungroup() %>%
  dplyr::select(Site, Lowest_Common_Taxon_Name, Total_Harvest_prop) %>%
  spread(key = Lowest_Common_Taxon_Name, value = Total_Harvest_prop)

df_comm_wide[is.na(df_comm_wide)] <- 0
df_comm_wide <- df_comm_wide %>%
  remove_rownames %>% 
  column_to_rownames(var="Site")
##select only numerical rows
df_comm_wide <- df_comm_wide %>%
  dplyr:: select(Abalone:`Wilson's Snipe`)


##SW diversity 
library(tibble)
sw_div <- as.data.frame(diversity(df_comm_wide, index = "shannon"))
sw_div <- rownames_to_column(sw_div, "Site") %>%
  rename(sw_diversity = `diversity(df_comm_wide, index = "shannon")`)

head(sw_div)

comm_div <- left_join(slopes, richness, by = "Site") %>%
  left_join(sw_div, by = "Site")

##evenness
comm_div <- comm_div %>%
  mutate(evenness = sw_diversity/log(richness))



##Relating harvest diversity indices with slope (rate of harvest decline)
ggplot(comm_div, aes(x = richness, y = slope)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic() +
  ylab("Rate of Harvest Decline") +
  xlab("Harvest Richness")

richness_lm <- lm(slope ~ richness, data = comm_div)
summary(richness_lm)

ggplot(comm_div, aes(x = sw_diversity, y = slope)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic() +
  ylab("Rate of Harvest Decline") +
  xlab("Harvest Diversity (SW)")

sw_lm <- lm(slope ~ sw_diversity, data = comm_div)
summary(sw_lm)


ggplot(comm_div, aes(x = evenness, y = slope)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic() +
  ylab("Rate of Harvest Decline") +
  xlab("Harvest Evenness (Pielou)")

even_lm <- lm(slope ~ evenness, data = comm_div)
summary(even_lm)

##Plotting harvest distributions for communities at extremes
##steepest slope: elfin cove
total_harvest_rank %>%
  filter(Site == "Elfin Cove") %>%
  ggplot(aes(x = harvest_rank, y= Total_Harvest_prop, fill = Habitat)) +
  geom_col() +
  scale_fill_manual(values = c("#FF9999", "#003366", "#CC9966", "#339933"))+
  theme_classic() +
  geom_text(aes(label = Lowest_Common_Taxon_Name), hjust = -0.25, angle = 90, size = 3) +
  xlab("Harvest Rank") +
  ylab("Proportion of Total Harvest")
  

total_harvest_rank %>%
  filter(Site == "Yakutat") %>%
  ggplot(aes(x = harvest_rank, y= Total_Harvest_prop, fill = Habitat)) +
  geom_col() +
  scale_fill_manual(values = c("#FF9999", "#003366", "#CC9966", "#339933"))+
  theme_classic() +
  geom_text(aes(label = Lowest_Common_Taxon_Name), hjust = -0.25, angle = 90, size = 3) +
  xlab("Harvest Rank") +
  ylab("Proportion of Total Harvest")


total_harvest_rank %>%
  filter(Site == "Klukwan") %>%
  ggplot(aes(x = harvest_rank, y= Total_Harvest_prop, fill = Habitat)) +
  geom_col() +
  scale_fill_manual(values = c("#FF9999", "#003366", "#CC9966", "#339933"))+
  theme_classic() +
  geom_text(aes(label = Lowest_Common_Taxon_Name), hjust = -0.25, angle = 90, size = 3) +
  xlab("Harvest Rank") +
  ylab("Proportion of Total Harvest")

total_harvest_rank %>%
  filter(Site == "Hyder") %>%
  ggplot(aes(x = harvest_rank, y= Total_Harvest_prop, fill = Habitat)) +
  geom_col() +
  scale_fill_manual(values = c("#FF9999", "#003366", "#CC9966", "#339933"))+
  theme_classic() +
  geom_text(aes(label = Lowest_Common_Taxon_Name), hjust = -0.25, angle = 90, size = 3) +
  xlab("Harvest Rank") +
  ylab("Proportion of Total Harvest")

##Comparing each community to "average" community response
##linear model across all communities

ggplot() +
  geom_smooth(data = results_2, aes(x = species_removed, y = total_harvest, group = community), method = "loess", se = FALSE, color = "black") +
  geom_smooth(data = results_2, aes(x = species_removed, y = total_harvest), method = "loess", se = FALSE, color = "red") +
  theme_classic() +
  ylab("Proportion Total Harvest Remaining") +
  xlab("Number of Species Removed") 
##extract slope and relate to species richness --------NON-LINEAR
##testing non-linear regression
##using a self-starting function, which is a special function for curve fitting that guesses its own start parameters 
##(using nls w/ exponential decay can lead to error of singular gradient if bad starting values are picked (and in our case will have unique starting values for each community))
##General exponential decay equation: y(t) ~ yf + (y0 - yf)e ^-at
##Exponential decay parameters: y0 = starting value, yf = final value, a = rate of change, t = time (or in our case # of species removed)
##Example of exponential decay for 1 community --
angoon = results_2 %>% filter(community == "Angoon")
#nls_angoon <- nls(total_harvest ~ yf + (y0 - yf) * exp(-alpha * species_removed), data = angoon, start = list(y0 = 100, yf = 0, alpha = 1))
##this produces error of singular gradient, so using a selfStart function that wil guess its own start parameters 
##The asymptotic regression function, SSasymp is equivalent to exponential decay function, only difference is that instead of fitting the rate constant a directly, it searches for the logarithm of a
##Example SSasymp for angoon 
nls_angoon_2 <- nls(total_harvest ~ SSasymp(species_removed, yf, y0, log_alpha), data = angoon)
nls_angoon_2

library(broom)

qplot(species_removed, total_harvest, data = augment(nls_angoon_2)) + geom_line(aes(y = .fitted))

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

##Join non-linear parameters to harvest diversity metrics
nls_all_param <- nls_all_param %>%
  rename(Site = "community")

comm_div_2 <- left_join(nls_all_param, richness, by = "Site") %>%
  left_join(sw_div, by = "Site") %>%
  mutate(evenness = sw_diversity/log(richness))

##Relating harvest diversity indices with alpha (rate of harvest decline)
ggplot(comm_div_2, aes(x = richness, y = alpha)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic() +
  ylab("Rate of Harvest Decline") +
  xlab("Harvest Richness") +
  ylim(0.05, 0.42) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"))


richness_lm <- lm(alpha ~ richness, data = comm_div_2)
summary(richness_lm)

ggplot(comm_div_2, aes(x = sw_diversity, y = alpha)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic() +
  ylab("Rate of Harvest Decline") +
  xlab("Harvest Diversity (SW)")

sw_lm <- lm(alpha ~ sw_diversity, data = comm_div_2)
summary(sw_lm)


ggplot(comm_div_2, aes(x = evenness, y = alpha)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic() +
  ylab("Rate of Harvest Decline") +
  xlab("Harvest Evenness (Pielou)")

even_lm <- lm(alpha ~ evenness, data = comm_div_2)
summary(even_lm)


##Compare rate of harvest decline with evenness of harvest across habitats
h_df_all <- read.csv("data/intermediate_data/harvest_diversity_by_habitat.csv")
rm(list = ls()[!ls() %in% c("comm_div_2", "h_df_all")])


h_df_all <- h_df_all %>%
  dplyr::rename(sw_div_h = "sw_diversity", richness_h = "richness", evenness_h = "evenness", evenness_h_log = "evenness_log")

comm_div_2 <- left_join(comm_div_2, h_df_all, by = "Site")

write.csv(comm_div_2, "data/intermediate_data/community_harvest_diversity_metrics.csv")

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
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic() +
  ylab("Rate of Harvest Decline") +
  xlab("Habitat Coupling (log 1/SD)") +
  ylim(0.05, 0.42) +
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.y=element_text(size = 14),axis.title.x=element_text(size = 14),  text = element_text(family = "Times New Roman"))


even_lm_h_5 <- lm(alpha ~ log_1_SD, data = comm_div_2)
summary(even_lm_h_5)

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

