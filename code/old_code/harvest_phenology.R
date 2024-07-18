##Harvest Phenology
##Code to create harvest distributions for species based on harvest window 

library(tidyverse)
library(readxl)
library(lubridate)
library(ggridges)

df <- read.csv("data/intermediate_data/comparable_harvest_df.csv") %>%
  filter(!Site_Year_Code %in% c("Hoonah_2016") )



hp <- read_excel("data/harvest_taxa_phenology_99percent_harvest.xlsx", sheet = 1)

df_hp <- left_join(df, hp, by = "Lowest_Common_Taxon_Name") %>%
  select(Site_Year_Code, Lowest_Common_Taxon_Name, Habitat, Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum, Harvest_Start, Harvest_End, Avg_Peak_Harvest)


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
  weights <- dnorm(1:duration, mean = peak_date, sd = 0.2 * duration) # Adjust the standard deviation as needed
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
  amount <- df_hp$Estimated_Total_Pounds_Harvested_sum[i]
  duration <- df_hp$harvest_duration[i]
  peak_date <- as.numeric(df_hp$Avg_Peak_Harvest[i] - df_hp$Harvest_Start[i])
  Site_Year_Code <- df_hp$Site_Year_Code[i]
  Species <- df_hp$Lowest_Common_Taxon_Name[i]
  habitat <- df_hp$Habitat[i]
  # Distribute the harvest amount
  harvest_amount <- distribute_harvest(amount, duration, peak_date)
  # Create a dataframe with date and harvest amount
  data.frame(date = seq(df_hp$Harvest_Start[i], by = "day", length.out = duration),
             harvest_amount = harvest_amount, site_year_code = Site_Year_Code, species = Species, habitat = habitat)
})

# Combine the results into a single dataframe
simulated_harvest_df <- do.call(rbind, simulated_harvest) %>%
  mutate(month = lubridate::month(date, label = TRUE))

ggplot(simulated_harvest_df, aes(x = date, y = harvest_amount, color = habitat)) +
  geom_line() +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Date", y = "Harvest Amount (lbs)", title = "Simulated Harvest Distribution Across Harvest Season") +
  facet_wrap(~species)



ggplot(simulated_harvest_df, aes(x = date, y = harvest_amount, color = species)) +
  geom_line() +
  labs(x = "Date", y = "Harvest Amount", title = "Simulated Harvest Amounts Across Harvest Season") +
  facet_wrap(~habitat, scale = "free")

ggplot(simulated_harvest_df, aes(x = date, y = harvest_amount, group = species, color = species)) +
  geom_line() +
  labs(x = "Date", y = "Harvest Amount", title = "Simulated Harvest Amounts Across Harvest Season") 

ggplot(simulated_harvest_df, aes(x = date, y = harvest_amount, group = species, color = species)) +
  geom_line() +
  labs(x = "Date", y = "Harvest Amount", title = "Simulated Harvest Amounts Across Harvest Season") +
  facet_wrap(~site_year_code, scale = "free")

ggplot(simulated_harvest_df, aes(x = date, y = harvest_amount, group = habitat, color = habitat)) +
  geom_line() +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Date", y = "Harvest Amount", title = "Simulated Harvest Amounts Across Harvest Season") +
  facet_wrap(~site_year_code, scale = "free")

ggplot(simulated_harvest_df, aes(x = date, y = harvest_amount, group = habitat, color = habitat)) +
  geom_line() +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Date", y = "Harvest Amount", title = "Simulated Harvest Amounts Across Harvest Season") 
  



##Calculating total harvest per date
total_simulated_harvest <- simulated_harvest_df %>%
  filter(!is.na(harvest_amount)) %>%
  group_by(site_year_code, date) %>%
  summarise_at(vars(harvest_amount), list(harvest_total = sum))


ggplot(total_simulated_harvest, aes(x = date, y = harvest_total, group = site_year_code)) +
  geom_line() +
  facet_wrap(~site_year_code, scale = "free")

ggplot() +
  geom_line(data = simulated_harvest_df, aes(x = date, y = harvest_amount, group = habitat, color = habitat)) +
  geom_line(data = total_simulated_harvest, aes(x = date, y = harvest_total)) +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Date", y = "Harvest Amount", title = "Simulated Harvest Amounts Across Harvest Season") +
  facet_wrap(~site_year_code, scale = "free")

ggplot() +
  geom_line(data = simulated_harvest_df, aes(x = date, y = harvest_amount, group = habitat, color = habitat)) +
  geom_line(data = total_simulated_harvest, aes(x = date, y = harvest_total)) +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Date", y = "Harvest Amount", title = "Simulated Harvest Amounts Across Harvest Season") +
  facet_wrap(~site_year_code)

##example communities
##hydaburg_2012
hydaburg_2012_sim <- simulated_harvest_df %>%
  filter(site_year_code == "Hydaburg_2012")
hydaburg_2012_total <- total_simulated_harvest %>%
  filter(site_year_code == "Hydaburg_2012")

ggplot() +
  geom_line(data = hydaburg_2012_sim, aes(x = date, y = harvest_amount, group = habitat, color = habitat)) +
  geom_line(data = hydaburg_2012_total, aes(x = date, y = harvest_total)) +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Date", y = "Harvest Amount (lbs)", title = "Simulated Harvest Distribution Across Harvest Season") +
  theme_classic()

##hoonah_2012
hoonah_2012_sim <- simulated_harvest_df %>%
  filter(site_year_code == "Hoonah_2012")
hoonah_2012_total <- total_simulated_harvest %>%
  filter(site_year_code == "Hoonah_2012")

ggplot() +
  geom_line(data = hoonah_2012_sim, aes(x = date, y = harvest_amount, group = habitat, color = habitat)) +
  geom_line(data = hoonah_2012_total, aes(x = date, y = harvest_total)) +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Date", y = "Harvest Amount (lbs)", title = "Simulated Harvest Distribution Across Harvest Season") +
  theme_classic()
##calculating sd and CV
sim_harv_cv <- total_simulated_harvest %>%
  group_by(site_year_code) %>%
  summarise(harvest_total_mean = mean(harvest_total), harvest_total_sd = sd(harvest_total), harvest_total_cv = harvest_total_sd/harvest_total_mean)

ggplot(sim_harv_cv, aes(x = reorder(site_year_code, -harvest_total_cv), y = harvest_total_cv)) +
  geom_col() +
  theme(legend.position = "none", axis.text.x = element_text(size = 8, angle = 45, hjust = 1), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15))

ggplot(sim_harv_cv, aes(x = reorder(site_year_code, -harvest_total_sd), y = harvest_total_sd)) +
  geom_col() +
  theme(legend.position = "none", axis.text.x = element_text(size = 8, angle = 45, hjust = 1), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15))





###Simulated harvest percapita
# Apply the function to each row in the dataframe
simulated_harvest_percap <- lapply(1:nrow(df_hp), function(i) {
  amount <- df_hp$Percapita_Pounds_Harvested_sum[i]
  duration <- df_hp$harvest_duration[i]
  peak_date <- as.numeric(df_hp$Peak_Harvest[i] - df_hp$Harvest_Start[i])
  Site_Year_Code <- df_hp$Site_Year_Code[i]
  Species <- df_hp$Lowest_Common_Taxon_Name[i]
  habitat <- df_hp$Habitat[i]
  # Distribute the harvest amount
  harvest_amount <- distribute_harvest(amount, duration, peak_date)
  # Create a dataframe with date and harvest amount
  data.frame(date = seq(df_hp$Harvest_Start[i], by = "day", length.out = duration),
             harvest_amount = harvest_amount, site_year_code = Site_Year_Code, species = Species, habitat = habitat)
})

# Combine the results into a single dataframe
simulated_harvest_df_percapita <- do.call(rbind, simulated_harvest_percap)

ggplot(simulated_harvest_df_percapita, aes(x = date, y = harvest_amount, color = habitat)) +
  geom_line() +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Date", y = "Harvest Amount", title = "Simulated Harvest Amounts Across Harvest Season") +
  facet_wrap(~species)

ggplot(simulated_harvest_df_percapita, aes(x = date, y = harvest_amount, color = species)) +
  geom_line() +
  labs(x = "Date", y = "Harvest Amount", title = "Simulated Harvest Amounts Across Harvest Season") +
  facet_wrap(~habitat, scale = "free")

ggplot(simulated_harvest_df_percapita, aes(x = date, y = harvest_amount, group = species, color = species)) +
  geom_line() +
  labs(x = "Date", y = "Harvest Amount", title = "Simulated Harvest Amounts Across Harvest Season") 

ggplot(simulated_harvest_df_percapita, aes(x = date, y = harvest_amount, group = species, color = species)) +
  geom_line() +
  labs(x = "Date", y = "Harvest Amount", title = "Simulated Harvest Amounts Across Harvest Season") +
  facet_wrap(~site_year_code, scale = "free")

ggplot(simulated_harvest_df_percapita, aes(x = date, y = harvest_amount, group = habitat, color = habitat)) +
  geom_line() +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Date", y = "Harvest Amount", title = "Simulated Harvest Amounts Across Harvest Season - Percapita") +
  facet_wrap(~site_year_code, scale = "free")

ggplot(simulated_harvest_df_percapita, aes(x = date, y = harvest_amount, group = habitat, color = habitat)) +
  geom_line() +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Date", y = "Harvest Amount", title = "Simulated Harvest Amounts Across Harvest Season") 

##Calculating total harvest per date
total_simulated_harvest_percapita <- simulated_harvest_df_percapita %>%
  filter(!is.na(harvest_amount)) %>%
  group_by(site_year_code, date) %>%
  summarise_at(vars(harvest_amount), list(harvest_total = sum))


ggplot(total_simulated_harvest_percapita, aes(x = date, y = harvest_total, group = site_year_code)) +
  geom_line() +
  facet_wrap(~site_year_code, scale = "free")

ggplot() +
  geom_line(data = simulated_harvest_df_percapita, aes(x = date, y = harvest_amount, group = habitat, color = habitat)) +
  geom_line(data = total_simulated_harvest_percapita, aes(x = date, y = harvest_total)) +
  scale_colour_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
  labs(x = "Date", y = "Harvest Amount", title = "Simulated Harvest Amounts Across Harvest Season - Percapita") +
  facet_wrap(~site_year_code, scale = "free")
