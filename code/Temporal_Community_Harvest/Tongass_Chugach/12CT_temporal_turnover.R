##temporal beta diversity / turnover 

library(tidyverse)
library(vegan)
library(ggplot2)
library(BAT)

source("code/Temporal_Community_Harvest/Tongass_Chugach/3CT_calculate_prop_harvest.R")


df_temp_avg <- df_temp_avg %>%
  group_by(Forest, Community) %>%
  filter(n_distinct(Year) >= 3) %>%
  filter(Site_Year_Code != "Hoonah_2016")

##Convert to community-species matrix
df_comm_avg_wide <- df_temp_avg %>%
  dplyr:: select(Forest, Community, Year, Site_Year_Code, Lowest_Common_Taxon_Name, Total_Harvest_prop) %>% ##harvest proportion or total harvest?? 
  spread(key = Lowest_Common_Taxon_Name, value = Total_Harvest_prop) %>%
  ungroup()
df_comm_avg_wide[is.na(df_comm_avg_wide)] <- 0


#df_comm_avg_wide <- df_comm_avg_wide %>%
#  remove_rownames %>% 
#  column_to_rownames(var="Site_Year_Code")
##select only numerical rows
#df_comm_avg_wide <- df_comm_avg_wide %>%
#  dplyr::select(`Abalone`:Wolffish)

##standardise proportionally
#df_comm_avg_wide<- decostand(df_comm_avg_wide, "rank", na.rm = TRUE)
##Now, calculate pairwise bray-curtis harvest dissimilarities between each community



# Function to calculate beta diversity for a given site's data
betafun <- function(df) {
  # Select only species columns
  species_data <- df %>% select(-Forest, -Community, -Year, -Site_Year_Code)
  
  # Calculate distance matrix (e.g., Bray-Curtis dissimilarity)
  dist_matrix <- vegdist(species_data, method = "bray")
  
  # Calculate beta diversity (e.g., average distance)
  beta_diversity <- mean(dist_matrix)
  
  return(beta_diversity)
}



beta_div_all <- split(df_comm_avg_wide, df_comm_avg_wide$Community) %>%
  map(betafun) %>%
  bind_rows()

df_long <- pivot_longer(beta_div_all, cols = everything(), names_to = "Community", values_to = "Bray-Curtis")

testmat <- df_comm_avg_wide[,5:105]
bc_dist <- betadiver(testmat, method = "bray")


bc_dist <- as.data.frame(bc_dist)


##Conduct beta-diversity analysis on each site - returns dataframe for all site with temporal total beta diversity and its components 
##Beta_Total = 1 --> same individuals found at both time points
##Beta_Total = 0 --> totally different communities 
betafun_2 <- function(x){
  beta_matrix <- matrix(nrow=10, ncol = 3)
  colnames(beta_matrix) <- c("Beta_Total", "Beta_Replacement", "Beta_Richness")
  y<- nrow(x) - 1
  for(row_i in 1:y){
    beta1 <- x[row_i, 5:105]
    beta2 <- x[row_i+1, 5:105]
    beta_pair <- rbind(beta1,beta2)
    beta_total <- BAT::beta(beta_pair, func = "jaccard")
    beta_matrix[row_i,1] <- beta_total$Btotal
    beta_matrix[row_i,2] <- beta_total$Brepl
    beta_matrix[row_i,3] <- beta_total$Brich

  }
  beta_df <- as.data.frame(beta_matrix)
  beta_df$Forest <- x$Forest[1]
  beta_df$Community <- x$Community[2]
  
  return(beta_df)
} 

beta_div_all_2 <- split(df_comm_avg_wide, df_comm_avg_wide$Community) %>%
  map(betafun_2) %>%
  bind_rows() %>%
  filter(!is.na(Beta_Total)) 

test <- df_comm_avg_wide %>%
  group_by(Community) %>%
  mutate(year1 = lag(Year)) %>%
  select(Community, Year, year1) %>%
  filter(!is.na(year1)) %>%
  rename(year2 = "Year") %>%
  arrange(Community, year2) %>%
  ungroup()%>%
  select(year1, year2)


beta_div_all_2 <- cbind(beta_div_all_2, test) 


ggplot(beta_div_all_2, aes(x = year2, y = Beta_Total)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  facet_wrap(~Community)

###Looking at changes in species composition over time 

df_temp_avg %>%
  filter(Community == "Angoon") %>%
  ggplot(aes(x = Year, y = Percapita_Pounds_Harvested_sum, group = Lowest_Common_Taxon_Name)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  facet_wrap(~Lowest_Common_Taxon_Name)

test<- df_temp_avg %>%
  filter(Community == "Angoon" & Year == 2012)

