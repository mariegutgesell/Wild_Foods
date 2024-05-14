##Tongass -- community average harvest phenology -- total harvest amount

##Harvest Phenology
##Code to create harvest distributions for species based on harvest window 

library(tidyverse)
library(readxl)
library(lubridate)
library(ggridges)

source("code/3T_calculate_avg_community_harvest.R")
rm(list = ls()[!ls() %in% c("df_comm_avg")])

hp <- read_excel("data/harvest_taxa_phenology_99percent_harvest.xlsx", sheet = 1)

##select only species that make up 99% of harvest
df <- df_comm_avg %>%
  filter(Lowest_Common_Taxon_Name %in% hp$Lowest_Common_Taxon_Name)

df_hp <- left_join(df, hp, by = "Lowest_Common_Taxon_Name") %>%
  select(Site, Lowest_Common_Taxon_Name, Habitat, Estimated_Total_Pounds_Harvested_sum_avg, Percapita_Pounds_Harvested_sum_avg, Total_Harvest_prop, Harvest_Start, Harvest_End, Avg_Peak_Harvest)


##Convert dates to proper date format
df_hp$Harvest_Start <- as.Date(df_hp$Harvest_Start)
df_hp$Harvest_End <- as.Date(df_hp$Harvest_End)
df_hp$Avg_Peak_Harvest <- as.Date(df_hp$Avg_Peak_Harvest)


# 3. Calculate the duration of the harvest season for each species
df_hp <- df_hp %>%
  mutate(harvest_duration = as.numeric(Harvest_End - Harvest_Start))


# Define a function to distribute harvest amount across the harvest period
distribute_harvest <- function(amount, duration, peak_date) {
  # Calculate weights for distribution
  weights <- dnorm(1:duration, mean = peak_date, sd = 0.1 * duration) # Adjust the standard deviation as needed
  # Scale weights to sum to the total amount
  weights <- amount * weights / sum(weights)
  return(weights)
}

df_hp <- df_hp %>%
  filter(!is.na(harvest_duration)) %>%
  filter(harvest_duration > 0) ##for now just trying to see if can get working for non-negative dates (i.e., ones that don't go from fall-spring for example)


df_hp_2 <- df_hp %>%
  filter(!is.na(harvest_duration)) %>%
  filter(harvest_duration < 0)
# Apply the function to each row in the dataframe
simulated_harvest <- lapply(1:nrow(df_hp), function(i) {
  amount <- df_hp$Estimated_Total_Pounds_Harvested_sum_avg[i]
  duration <- df_hp$harvest_duration[i]
  peak_date <- as.numeric(df_hp$Avg_Peak_Harvest[i] - df_hp$Harvest_Start[i])
  Site <- df_hp$Site[i]
  Species <- df_hp$Lowest_Common_Taxon_Name[i]
  habitat <- df_hp$Habitat[i]
  # Distribute the harvest amount
  harvest_amount <- distribute_harvest(amount, duration, peak_date)
  # Create a dataframe with date and harvest amount
  data.frame(date = seq(df_hp$Harvest_Start[i], by = "day", length.out = duration),
             harvest_amount = harvest_amount, site = Site, species = Species, habitat = habitat)
})

# Combine the results into a single dataframe
simulated_harvest_df <- do.call(rbind, simulated_harvest) %>%
  mutate(month = lubridate::month(date, label = TRUE))

ggplot(simulated_harvest_df, aes(x = date, y = harvest_amount, color = habitat)) +
  geom_line() +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Date", y = "Harvest Amount (lbs)", title = "Simulated Harvest Distribution Across Harvest Season") +
  facet_wrap(~species)

ggplot(simulated_harvest_df, aes(x = date, y = harvest_amount, group = habitat, color = habitat)) +
  geom_line() +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Date", y = "Harvest Amount (lbs)", title = "Simulated Harvest Amounts Across Harvest Season") +
  facet_wrap(~site, scale = "free")


##Calculating total harvest per date
total_simulated_harvest <- simulated_harvest_df %>%
  filter(!is.na(harvest_amount)) %>%
  group_by(site, date) %>%
  summarise_at(vars(harvest_amount), list(harvest_total = sum))


ggplot(total_simulated_harvest, aes(x = date, y = harvest_total, group = site, color = site)) +
  geom_line() +
  labs(x = "Date", y = "Harvest Amount (total harvest lbs)", title = "Total Harvest Amounts Across Harvest Season") +
  theme_classic()

total_harvest_all <- total_simulated_harvest %>%
  filter(!is.na(harvest_total)) %>%
  group_by(date) %>%
  summarise_at(vars(harvest_total), list(harvest_mean = mean))

ggplot() +
  geom_line(data = total_harvest_all, aes(x = date, y = harvest_mean), color = "red", linewidth = 3) +
  geom_line(data = total_simulated_harvest, aes(x = date, y = harvest_total, group = site), color = "black") +
  labs(x = "Date", y = "Harvest Amount (total harvest lbs)", title = "Simulated Harvest Amounts Across Harvest Season") +
  theme_classic()



ggplot() +
  geom_line(data = simulated_harvest_df, aes(x = date, y = harvest_amount, group = habitat, color = habitat)) +
  geom_line(data = total_simulated_harvest, aes(x = date, y = harvest_total)) +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Date", y = "Harvest Amount (total harvest lbs)", title = "Simulated Harvest Amounts Across Harvest Season") +
  facet_wrap(~site, scale = "free")


##Plot harvest in order of increasing harvest date for each community
total_simulated_harvest <- total_simulated_harvest %>%
  group_by(site) %>%
  mutate(harvest_day = rank(harvest_total, ties.method = "min"))

##Calculate proportion of year that harvest is below threshold (total harvest/365)
total_annual_harvest <- total_simulated_harvest %>%
  group_by(site) %>%
  summarise_at(vars(harvest_total), list(harvest_total_annual = sum)) %>%
  mutate(daily_harvest_threshold = harvest_total_annual/365)


harvest_threshold <- left_join(total_simulated_harvest, total_annual_harvest, by = "site") %>%
  group_by(site) %>%
  mutate(below_harvest_threshold = ifelse(harvest_total < daily_harvest_threshold, "Y", "N")) %>%
  filter(below_harvest_threshold == "Y") %>%
  count() %>%
  mutate(prop_days_below_threshold = (n/365)*100) 

##Plot thresholds
ggplot(total_simulated_harvest, aes(x = harvest_day, y = harvest_total, group = site)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(data = total_annual_harvest, aes(yintercept = daily_harvest_threshold), linetype = "dashed", color = "red") +
  # scale_x_discrete(labels = function(x) gsub("date", "", x)) +
  facet_wrap(~site, scale = "free")

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
comm_div <- read.csv("data/intermediate_data/community_harvest_diversity_metrics.csv") %>%
  rename(site = "Site")
#rm(list = ls()[!ls() %in% c("comm_div_2", "h_df_all")])

comm_dv_cv <- left_join(sim_harv_cv, comm_div, by = "site") %>%
  left_join(harvest_threshold, by = "site")

##CV vs. harvest diversity metrics ---------------
##testing if relationships hold w/o klukwan
#comm_dv_cv <- comm_dv_cv %>%
#  filter(site != "Klukwan")

ggplot(comm_dv_cv, aes(x = richness, y = harvest_total_cv)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() 

richness_cv_lm <- lm(harvest_total_cv ~ richness, comm_dv_cv)
summary(richness_cv_lm)

ggplot(comm_dv_cv, aes(x = sw_diversity, y = harvest_total_cv)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()
sw_cv_lm <- lm(harvest_total_cv ~ sw_diversity, comm_dv_cv)
summary(sw_cv_lm)


ggplot(comm_dv_cv, aes(x = evenness, y = harvest_total_cv)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()
even_cv_lm <- lm(harvest_total_cv ~ evenness, comm_dv_cv)
summary(even_cv_lm)

ggplot(comm_dv_cv, aes(x = log(sd), y = harvest_total_cv)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

sd_h_cv_lm <- lm(harvest_total_cv ~ sd, comm_dv_cv)
summary(sd_h_cv_lm)

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

##

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
total_simulated_harvest_prop_habitat <- simulated_harvest_prop_df %>%
  filter(!is.na(harvest_amount)) %>%
  group_by(site, date, habitat) %>%
  summarise_at(vars(harvest_amount), list(harvest_total = sum))

ggplot(total_simulated_harvest_prop_habitat, aes(x = date, y = harvest_total, group = site, color = habitat)) +
  geom_line() +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Date", y = "Harvest Amount (%)", title = "Total Harvest Amounts Across Harvest Season") +
  theme_classic() +
  facet_wrap(~site)


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




