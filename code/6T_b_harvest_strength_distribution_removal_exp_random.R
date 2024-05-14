##Harvest removal experiment -- randomly removing species 
library(tidyverse)
library(ggplot2)
library(ggrepel)

##source code that calculates average harvest across time for each community
source("code/3T_calculate_avg_community_harvest.R")
rm(list = ls()[!ls() %in% c("df_comm_avg")])

df_comm_avg <- df_comm_avg %>%
  filter(Total_Harvest_prop != 0)

##Harvest removal experiment -- random species loss ---------------
# ##For each community/year, create rank # based on proportion of total harvest
# ##first for total harvest
# total_harvest_rank <- df_comm_avg %>%
#   group_by(Site) %>%
#   mutate(harvest_rank = rank(-Total_Harvest_prop, ties.method = "min")) 
# 
# ##Plot harvest distributions by community
# ggplot(total_harvest_rank, aes(x = harvest_rank, y= Total_Harvest_prop, fill = Habitat)) +
#   geom_col() +
#   scale_fill_manual(values = c("salmon", "darkblue", "brown", "darkgreen"))+
#   facet_wrap(~Site, scale = "free")
# 
# ##Harvest Removal Experiment -- remove species randomly 
# results_1000 <- data.frame(
#   iteration = integer(),
#   community = character(),
#   species_removed = integer(),
#   total_harvest = numeric(),
#   stringsAsFactors = FALSE
# )
# # Function to calculate total harvest after removing a particular species
# 
# calculate_total_harvest <- function(df, species_to_remove) {
#   total_harvest <- sum(df$Total_Harvest_prop)
#   removed_harvest <- df$Total_Harvest_prop[df$Lowest_Common_Taxon_Name %in% species_to_remove]
#   remaining_harvest <- total_harvest - sum(removed_harvest)
#   return(remaining_harvest)
# }
# 
# 
# ##Iterate the experiment 1000 times
# for(iteration in 1:1000) {
#   print(paste("Iteration:", iteration))

# # Iterate over each community
# communities <- unique(total_harvest_rank$Site)
# 
# for (community in communities) {
#   cat("Community:", community, "\n")
#   
#   # Extract harvest data for the current community
#   community_data <- subset(total_harvest_rank, Site == community)
#   
#   # Shuffle the list of species within the community
#   shuffled_species <- sample(unique(community_data$Lowest_Common_Taxon_Name))
#   
#   # Initialize variables to store results for the current community
#   species_removed <- 0
#   remaining_harvest <- sum(community_data$Total_Harvest_prop)
#   
#   # Sequential removals of species and calculation of total harvest
#   for (species in shuffled_species) {
#     remaining_harvest <- calculate_total_harvest(community_data, species)
#     species_removed <- species_removed + 1
#     # Store results for the current community
#     results_1000 <- rbind(results_1000, data.frame(iteration = iteration, community = community, species_removed = species_removed, total_harvest = remaining_harvest))
#     community_data <- community_data[community_data$Lowest_Common_Taxon_Name != species, ]
#     }
#   }
# }


##add row for 0 species removed and 100% harvest
#total_harvest_prop <- results_1000 %>%
#  select(community, iteration) %>%
#  distinct() %>%
#  mutate(species_removed = 0) %>%
#  mutate(total_harvest = 100)

#results_2 <- rbind(results_1000, total_harvest_prop)

#write.csv(results_2, "data/intermediate_data/random_harvest_loss_1000.csv")

##Start here to avoid re-running simulations ----------------
results_2 <- read.csv("data/intermediate_data/random_harvest_loss_1000.csv")
##Calculate mean and SD of random harvest removal experiment
random_summary <- results_2 %>%
  group_by(community, species_removed) %>%
  summarise_at(vars(total_harvest), list(mean = mean, sd = sd))

##Plot Species Removal experiment
ggplot(random_summary, aes(x = species_removed, y = mean, group = community, color = community)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width = .2, position = position_dodge(0.05)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  theme(legend.position = "none") +
  facet_wrap(~community, scale = "free")

##extract slope and relate to species richness --------LINEAR
# List to store regression results
result_list <- split(random_summary, random_summary$community)
regression_results <- list()

# Perform linear regression for each community
for (community in names(result_list)) {
  community_df <- result_list[[community]]
  regression <- lm(mean ~ species_removed, data = community_df)
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



##Readin harvest diversity metrics
comm_div <- read.csv("data/intermediate_data/community_harvest_diversity_metrics.csv")

comm_div <- left_join(slopes, comm_div, by = "Site") 

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



ggplot(comm_div, aes(x = evenness_h, y = slope)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic() +
  ylab("Rate of Harvest Decline") +
  xlab("Harvest Habitat Evenness (Pielou)")

even_h_lm <- lm(slope ~ evenness_h, data = comm_div)
summary(even_h_lm)


ggplot(comm_div, aes(x = log(sd), y = slope)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_classic() +
  ylab("Rate of Harvest Decline") +
  xlab("Harvest Habitat Evenness (sd)")

sd_h_lm <- lm(slope ~ log(sd), data = comm_div)
summary(sd_h_lm)










###Trying when removing the same species randomly from each community  ----------------
## Harvest Removal Experiment -- remove same species randomly from each community in each iteration
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
# Iterate over each iteration
num_iterations <- 50  # Change this to the desired number of iterations
for (iteration in 1:num_iterations) {
  cat("Iteration:", iteration, "\n")
  
  # Randomly select a species to remove across all communities
  all_species <- unique(total_harvest_rank$Lowest_Common_Taxon_Name)
  species_to_remove <- sample(all_species, 1)
  
  # Iterate over each community
  communities <- unique(total_harvest_rank$Site)
  for (community in communities) {
    cat("Community:", community, "\n")
    
    # Extract harvest data for the current community
    community_data <- subset(total_harvest_rank, Site == community)
    
    # Calculate total harvest before removal
    total_harvest_before <- sum(community_data$Total_Harvest_prop)
    
    # Remove the selected species from the current community
    community_data <- community_data[community_data$Lowest_Common_Taxon_Name != species_to_remove, ]
    
    # Calculate total harvest after removal
    total_harvest_after <- sum(community_data$Total_Harvest_prop)
    
    # Store results for the current community
    results <- rbind(results, data.frame(community = community, species_removed = species_to_remove, total_harvest = total_harvest_after))
  }
}

##add row for 0 species removed and 100% harvest
total_harvest_prop <- results %>%
  select(community) %>%
  distinct() %>%
  mutate(species_removed = 0) %>%
  mutate(total_harvest = 100)

results_3 <- rbind(results, total_harvest_prop)


##Plot Species Removal experiment
ggplot(results_3, aes(x = species_removed, y = total_harvest, group = community, color = community)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
 # facet_wrap(~community, scale = "free") +

## Harvest Removal Experiment -- sequentially remove species with a random starting point
results <- data.frame(
  community = character(),
  species_removed = character(),
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
  
  # Randomly shuffle the order of species within the community
  shuffled_species <- sample(unique(community_data$Lowest_Common_Taxon_Name))
  
  # Initialize variables to store results for the current community
  species_removed <- character()
  remaining_harvest <- sum(community_data$Total_Harvest_prop)
  
  # Sequential removals of species with random starting point
  for (i in 1:length(shuffled_species)) {
    species_to_remove <- shuffled_species[i]
    remaining_harvest <- calculate_total_harvest(community_data, species_to_remove)
    species_removed <- c(species_removed, species_to_remove)
    
    # Store results for the current community
    results <- rbind(results, data.frame(community = community, species_removed = species_to_remove, total_harvest = remaining_harvest))
    
    # Remove the species from the community data
    community_data <- community_data[community_data$Lowest_Common_Taxon_Name != species_to_remove, ]
  }
}

##add row for 0 species removed and 100% harvest
total_harvest_prop <- results %>%
  select(community) %>%
  distinct() %>%
  mutate(species_removed = 0) %>%
  mutate(total_harvest = 100)

results_4 <- rbind(results, total_harvest_prop)


##Plot Species Removal experiment
ggplot(results_4, aes(x = species_removed, y = total_harvest, group = community, color = community)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

##i think want to change to assign a random rank to all possible species (i.e., total species list) and then remove them randomly, but do the same order across all communities