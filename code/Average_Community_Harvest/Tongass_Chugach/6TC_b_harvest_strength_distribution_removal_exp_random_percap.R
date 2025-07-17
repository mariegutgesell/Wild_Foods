##Harvest removal experiment -- randomly removing species -- PERCAPITA HARVEST
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(purrr)
library(broom)


library(minpack.lm)

##source code that calculates average harvest across time for each community
source("code/Average_Community_Harvest/Tongass_Chugach/3CT_calculate_avg_community_harvest.R")
rm(list = ls()[!ls() %in% c("df_comm_avg")])

df_comm_avg <- df_comm_avg %>%
  select(Site:Lowest_Common_Taxon_Name, Percapita_Pounds_Harvested_sum_avg, Percapita_Pounds_Harvested_sum_avg_total) %>%
  filter(Percapita_Pounds_Harvested_sum_avg != 0)

##Harvest removal experiment -- random species loss ---------------
# ##For each community/year, create rank # based on proportion of total harvest
# ##first for total harvest
 total_harvest_rank <- df_comm_avg %>%
   group_by(Site) %>%
   mutate(harvest_rank = rank(-Percapita_Pounds_Harvested_sum_avg, ties.method = "min")) 
# 
total_harvest_rank <- total_harvest_rank %>%
  group_by(Site) %>%
  arrange(harvest_rank) %>%
  mutate(harvest_rank_2 = row_number())

# ##Plot harvest distributions by community
 ggplot(total_harvest_rank, aes(x = harvest_rank_2, y= Percapita_Pounds_Harvested_sum_avg, fill = Habitat)) +
   geom_col() +
   scale_fill_manual(values = c("#FF9999","#003366","#CC9966", "#339933"))+
   facet_wrap(~Site, scale = "free")
# 
# ##Harvest Removal Experiment -- remove species randomly 
results_1000 <- data.frame(
   iteration = integer(),
   community = character(),
   species_removed = integer(),
   total_harvest = numeric(),
   stringsAsFactors = FALSE
 )
# # Function to calculate total harvest after removing a particular species
# 
 calculate_total_harvest <- function(df, species_to_remove) {
   total_harvest <- sum(df$Percapita_Pounds_Harvested_sum_avg)
   removed_harvest <- df$Percapita_Pounds_Harvested_sum_avg[df$Lowest_Common_Taxon_Name %in% species_to_remove]
   remaining_harvest <- total_harvest - sum(removed_harvest)
   return(remaining_harvest)
 }
# 
# 
# ##Iterate the experiment 1000 times
 for(iteration in 1:1000) {
   print(paste("Iteration:", iteration))

# # Iterate over each community
 communities <- unique(total_harvest_rank$Site)
#
 for (community in communities) {
   cat("Community:", community, "\n")
  
#   # Extract harvest data for the current community
   community_data <- subset(total_harvest_rank, Site == community)
   
#   # Shuffle the list of species within the community
   shuffled_species <- sample(unique(community_data$Lowest_Common_Taxon_Name))
   
   # Initialize variables to store results for the current community
   species_removed <- 0
   remaining_harvest <- sum(community_data$Percapita_Pounds_Harvested_sum_avg)
   
#   # Sequential removals of species and calculation of total harvest
   for (species in shuffled_species) {
     remaining_harvest <- calculate_total_harvest(community_data, species)
     species_removed <- species_removed + 1
     # Store results for the current community
     results_1000 <- rbind(results_1000, data.frame(iteration = iteration, community = community, species_removed = species_removed, total_harvest = remaining_harvest))
     community_data <- community_data[community_data$Lowest_Common_Taxon_Name != species, ]
     }
   }
 }


 #write.csv(results_1000, "data/intermediate_data/random_harvest_loss_1000_percap_v1.csv")
 
 results_1000 <- read.csv("data/intermediate_data/random_harvest_loss_1000_percap_v1.csv") %>%
   select(iteration:total_harvest)
 ##For some reason...crashed out after 854 iterations.. look into and try again? 
 ##add row for 0 species removed and 100% harvest
total_harvest_percap <- df_comm_avg %>%
  ungroup() %>%
  select(Site, Percapita_Pounds_Harvested_sum_avg_total) %>%
  unique() %>%
  rename(total_harvest = "Percapita_Pounds_Harvested_sum_avg_total", community = "Site") %>%
  mutate(species_removed = 0)


##repeat so have 0 harvest for every iteration
total_harvest_repeated <- total_harvest_percap %>%
  slice(rep(1:n(), each = 854)) %>%
  mutate(iteration = rep(1:854, times = nrow(total_harvest_percap))) %>%
  select(iteration, community, species_removed, total_harvest)



results_2 <- rbind(results_1000, total_harvest_repeated)

write.csv(results_2, "data/intermediate_data/random_harvest_loss_1000_percap.csv")

##Start here to avoid re-running simulations ----------------
results_2 <- read.csv("data/intermediate_data/random_harvest_loss_1000_percap.csv") %>%
  filter(iteration <= 854)

##For each iteration, want to conduct regression and then calculate mean and SD of the slope 
test <- results_2 %>%
  filter(community %in% c("Angoon", "Cordova"))


#ggplot(test, aes(x = species_removed, y = total_harvest)) +
#  geom_point() +
#  geom_smooth(method = "loess")+
#  facet_wrap(~iteration)

##with the random species loss, the best fit for equation will be different for each one.. 

##For now, starting with just linear, see if there is a way to do best fit model for each...but will be trickier to interpret as will ahve different rate metrics 
##extract slope and relate to species richness --------LINEAR ------------
# List to store regression results
community_iteration_list <- split(results_2, list(results_2$community, results_2$iteration))##want to split by both community and iteration 

regression_results <- list()

# Perform linear regression iteration for each community
for (community in names(community_iteration_list)) {
  community_df <- community_iteration_list[[community]]
  regression <- lm(total_harvest ~ species_removed, data = community_df)
  regression_results[[community]] <- summary(regression)
}


str(regression_results)
# Initialize an empty dataframe to store results
results_df <- data.frame(Community.iteration = character(),
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
                                   Value = coefficients[i, "Estimate"], stringsAsFactors = FALSE))
  }
}




##Calculate mean and SD of slope
lm_mean_slopes <- results_df %>%
  separate(Community, into = c("Community", "Iteration"), sep = "\\.", remove = FALSE) %>%
  filter(Coefficients == "species_removed") %>%
  group_by(Community) %>%
  summarise_at(vars(Value), list(slope_mean = mean, slope_sd = sd))


write.csv(lm_mean_slopes, "data/intermediate_data/1000_random_removal_slope_mean_sd.csv")

slopes <- read.csv("data/intermediate_data/1000_random_removal_slope_mean_sd.csv")


test2 <- results_2 %>%
  filter(community == "Beecher Pass") %>%
  filter(iteration <=30)

ggplot(test2, aes(x = species_removed, y = total_harvest)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(~iteration)

##comparing slopes 
slopes <- results_df %>%
  filter(Coefficients == "species_removed") %>%
  select(Community, Value) %>%
  rename(slope = "Value", Site = "Community")




##Trying to identify best model fit ---------------


# Define model functions
linear_model <- function(x, y) lm(y ~ x)
#exp_decay_model <- function(x, y) nls(y ~ a * exp(b * x), start = list(a = 1, b = -0.1), control = nls.control(maxiter = 100))
exp_decay_model <- function(x, y) nls(y ~ SSasymp(x, Asym, R0, log_alpha), control = nls.control(minFactor = 1e-6, maxiter = 50))
#log_model <- function(x, y) lm(y ~ log(x)) ##similar to exp decay, but we do decline to 0 and plus this data has 0's -- for now just use 3 general model types

#power_law_model <- function(x, y) nls(y ~ a * x^b, start = list(a = max(y), b = -0.5))
#inverse_decay_model <- function(x, y) nls(y ~ a + b / x, start = list(a = max(y), b = 0.1))
#gompertz_model <- function(x, y) nls(y ~ a * exp(-b * exp(-c * x)), start = list(a = max(y), b = 1, c = 0.1))
#logistic_model <- function(x, y) nls(y ~ a / (1 + exp(-b * (x - c))), start = list(a = max(y), b = 1, c = mean(x)))
logistic_model <- function(x, y) nls(y ~ SSlogis(x, Asym, xmid, scal), control = nls.control(minFactor = 1e-6, maxiter = 50))

# Create a function to fit and compare models
fit_models <- function(data, community, iteration) {
  x <- data$species_removed
  y <- data$total_harvest
  
  # Try fitting each model, capturing errors for models that fail to converge
  models <- list()
  
  # Linear Model
  models[["linear"]] <- tryCatch({
    model <- linear_model(x, y)
    tibble(model_type = "linear", r_squared = summary(model)$r.squared, model = list(model))
  }, error = function(e) NULL)
  
  # Exponential Decay Model
  models[["exp_decay"]] <- tryCatch({
    model <- exp_decay_model(x, y)
    r_squared <- cor(y, predict(model))^2
    tibble(model_type = "exp_decay", r_squared = r_squared, model = list(model))
  }, error = function(e) NULL)
  
  
  ###ADD this back in after error = if wnat to see error messages: function(e) {
  #message("Error in exp_decay model for community ", community, 
  #        ", iteration ", iteration, ": ", e$message)
  #NULL}
  # Logarithmic Model
#  models[["log"]] <- tryCatch({
#    model <- log_model(x+1, y)
#    tibble(model_type = "log", r_squared = summary(model)$r.squared, model = list(model))
#  }, error = function(e) NULL)
  
  # Power Law Model
 # models[["power_law"]] <- tryCatch({
 #   model <- power_law_model(x, y)
#    r_squared <- cor(y, predict(model))^2
#    tibble(model_type = "power_law", r_squared = r_squared, model = list(model))
#  }, error = function(e) NULL)
  
  # Inverse Decay Model
 # models[["inverse_decay"]] <- tryCatch({
  #  model <- inverse_decay_model(x, y)
  #  r_squared <- cor(y, predict(model))^2
  #  tibble(model_type = "inverse_decay", r_squared = r_squared, model = list(model))
#  }, error = function(e) NULL)
  
  # Gompertz Model
 # models[["gompertz"]] <- tryCatch({
#    model <- gompertz_model(x, y)
#    r_squared <- cor(y, predict(model))^2
#    tibble(model_type = "gompertz", r_squared = r_squared, model = list(model))
#  }, error = function(e) NULL)
  
  # Logistic Model
  models[["logistic"]] <- tryCatch({
    model <- logistic_model(x, y)
    r_squared <- cor(y, predict(model))^2
    tibble(model_type = "logistic", r_squared = r_squared, model = list(model))
  }, error = function(e) 
    NULL)
  
  # Filter out any NULL values (models that failed) and select the best model by R²
  models <- bind_rows(models)
  #return(models)
  best_model <- models %>% filter(r_squared == max(r_squared, na.rm = TRUE))
  return(best_model)
}

best_models <- test %>%
  group_by(community, iteration) %>%
  nest() %>%
  mutate(best_fit = pmap(list(data, community, iteration), fit_models)) %>%
  unnest(best_fit) %>%
  select(-data)

best_models_all <- results_2 %>%
  group_by(community, iteration) %>%
  nest() %>%
  mutate(best_fit = pmap(list(data, community, iteration), fit_models)) %>%
  unnest(best_fit) %>%
  select(-data)
saveRDS(best_models_all, "data/intermediate_data/random_removal_exp_model_outputs.rds")



best_models_all <- readRDS("data/intermediate_data/random_removal_exp_model_outputs.rds")
linear <- best_models %>%
  filter(model_type == "linear")
log <- best_models %>%
  filter(model_type == "log")
##all linear and log are there

logistic <- best_models %>%
  filter(model_type == "logistic")
exp_decay<- best_models %>%
  filter(model_type == "exp_decay")
##ones that previously were log not best fit is exp_decay, this makes sense to me as essentially the same, and has a rate



log_data <- test %>%
  filter(paste(community, iteration) %in% paste(log$community, log$iteration))
ggplot(log_data, aes(x = species_removed, y = total_harvest)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~paste(community, iteration))


exp_decay_data <- test %>%
  filter(paste(community, iteration) %in% paste(exp_decay$community, exp_decay$iteration)) %>%
  filter(iteration <= 30)
ggplot(exp_decay_data, aes(x = species_removed, y = total_harvest)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~paste(community, iteration))


logistic_data <- test %>%
  filter(paste(community, iteration) %in% paste(logistic$community, logistic$iteration)) %>%
  filter(iteration <= 30)

ggplot(logistic_data, aes(x = species_removed, y = total_harvest)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~paste(community, iteration))

test2 <- models %>%
  ungroup() %>%
  select(model_type) %>%
  unique()

##Errors generally occur because of singular gradient, or step factor is <minFactor -- essentially both indicating that the model fit is terrible, and likely better described by other models (e.g., linear or logistic)
#In the context of non-linear least squares fitting with nls(), a singular gradient suggests that the optimization algorithm is struggling to find a direction to improve the fit. This usually happens when:
#The parameter space is nearly flat in the direction of one or more parameters, meaning the data does not contain enough information to estimate those parameters effectively.
#The data does not exhibit the expected curvature or decay pattern that the model assumes




# Function to extract parameters based on model type
extract_parameters <- function(community, iteration, model) {
  tryCatch({
    if (inherits(model, "lm")) {
      # Linear model: use tidy() to get coefficients
      tidy(model) %>%
        select(term, estimate) %>%
        mutate(model_type = "linear")
      
    } else if (inherits(model, "nls")) {
      # Non-linear model (e.g., SSasymp): use tidy() to extract parameters
      tidy_params <- tidy(model)
      
      # If using SSasymp, calculate alpha from log_alpha
      if (any(tidy_params$term == "log_alpha")) {
        tidy_params <- tidy_params %>%
          mutate(alpha = ifelse(term == "log_alpha", exp(estimate), NA)) #%>%
          #spread(term, estimate)
      }
      
      # Add model type
      tidy_params <- tidy_params %>%
        mutate(model_type = ifelse(grepl("asym", deparse(formula(model))), "exp_decay", "logistic"))
      
      tidy_params
      
    } else {
      # Unsupported model type, return an empty data frame
      data.frame(term = character(), estimate = numeric(), model_type = character())
    }
  }, error = function(e) NULL #{
  #   If there's an error, return an empty data frame
  #  data.frame(term = character(), estimate = numeric(), model_type = character())
  #}
 ) %>%
    # Add community and iteration columns
    mutate(community = community, iteration = iteration)
}

# Apply extraction function to each model and combine results
best_models <- best_models %>%
  select(community, iteration, model)
model_parameters <- best_models %>%
  pmap_dfr(~extract_parameters(..1, ..2, ..3))


model_parameters_all <- best_models_all  %>%
  select(community, iteration, model) %>%
  pmap_dfr(~extract_parameters(..1, ..2, ..3))
write.csv(model_parameters_all, "data/intermediate_data/random_removal_model_params_percap.csv")


##Seeing if I can make comparable rate metrics -- i.e., % unit change
calculate_comparable_rate <- function(community, iteration, model) {
  if (inherits(model, "lm")) {
    # Linear model: slope as rate per unit
    slope <- coef(model)["x"]
    rate_per_unit <- slope * 100  # Convert to percent if needed
    
  } else if (inherits(model, "nls")) {
    # Exponential or logistic
    params <- coef(model)
    if ("log_alpha" %in% names(params)) {
      # Exponential decay model: percent decrease per unit
      b <- params["log_alpha"]
      rate_per_unit <- (1 - exp(b)) * 100  # Percentage decay rate per unit
      
    } else {
      # Logistic model: rate at inflection (midpoint)
      a <- params["xmid"]
      b <- params["scal"]
      rate_per_unit <- (a * b / 4) * 100  # Growth rate at midpoint as percent
    }
  } else {
    rate_per_unit <- NA  # Unsupported model
  } %>%
   mutate(community = community, iteration = iteration)
}

comparable_rates_all <- best_models_all  %>%
  select(community, iteration, model) %>%
  pmap_dfr(~calculate_comparable_rate(..1, ..2, ..3))


##trying my own here
model_parameters_all <- read.csv("data/intermediate_data/random_removal_model_params_percap.csv")
##1) linear 
linear_rpu <- model_parameters_all %>%
  filter(model_type == "linear") %>%
  filter(term == "x") %>%
  mutate(rate_per_unit = estimate * 100) ##converts slope to a %.. ? i think? 

exp_decay_rpu <- model_parameters_all %>%
  filter(model_type == "exp_decay") %>%
  filter(term == "log_alpha") %>%
  mutate(rate_per_unit = (1- exp(estimate))*100)

logistic_rpu <- model_parameters_all %>%
  filter(model_type == "logistic") %>%
  filter(term %in% c("xmid", "scal")) %>%
  mutate(rate_per_unit = (estimate$xmid * estimate$scal /4)*100)
  
  
  
         