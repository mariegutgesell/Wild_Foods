##Looking at similarity/difference in community harvest composition by distance 

library(graph4lg)
library(tidyverse)
library(vegan)
library(spaa)
##Calculate pairwise distances between each community
sites_gps <- read_excel("data/CSIS_SurveyData_Demographics.xlsx", sheet = 2) %>%
  unite(Site_Year_Code, c(Community,Year), sep = "_", remove = FALSE) %>%
  dplyr::select(Community, Latitude, Longitude) %>%
  distinct()

sites_gps <- as.data.frame(sites_gps)

spatial_dist <- mat_geo_dist(data = sites_gps, ID = "Community", x = "Latitude", y = "Longitude", crds_type  = "polar")
spatial_dist <- as.data.frame(spatial_dist)
##distance in meters 
spatial_dist_km <- spatial_dist[,1:ncol(spatial_dist)]/1000

#When a projected coordinate reference system is used, it calculates classical Euclidean geographic distance between two points using Pythagora's theorem. When a polar coordinate reference system is used, it calculates the Great circle distance between points using different methods.

str(sites_gps)

##Calculate average harvest across years to get 1 harvest estimate for each community 
##import cleaned harvest data and trophic info that is comparable across all years
df <- read.csv("data/intermediate_data/comparable_harvest_df.csv") %>%
  filter(!Site_Year_Code %in% c("Hoonah_2016"))

total_harvest <- df %>%
  dplyr::select(Site_Year_Code, Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum) %>%
  filter(Percapita_Pounds_Harvested_sum != 0) %>%
  group_by(Site_Year_Code) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum), list(total = sum))

df_2 <- df %>%
  left_join(total_harvest, by = "Site_Year_Code") %>%
  filter(!is.na(Estimated_Total_Pounds_Harvested_sum)) %>%
  filter(!is.na(Percapita_Pounds_Harvested_sum)) %>%
  mutate(Total_Harvest_prop = (Estimated_Total_Pounds_Harvested_sum/Estimated_Total_Pounds_Harvested_sum_total)*100) %>%
  mutate(Percapita_Harvest_prop = (Percapita_Pounds_Harvested_sum/Percapita_Pounds_Harvested_sum_total)*100) %>%
  mutate(Percapita_Harvest_prop_log = log(Percapita_Harvest_prop)) %>%
  mutate(Percapita_Harvest_prop_sqrt = sqrt(Percapita_Harvest_prop)) %>%
  mutate(Total_Harvest_prop_log = log(Total_Harvest_prop)) %>%
  mutate(Total_Harvest_prop_sqrt = sqrt(Total_Harvest_prop)) %>%
  group_by(Site_Year_Code) %>%
  mutate(Percapita_Harvest_prop_scale = scale(Percapita_Harvest_prop)) %>%
  mutate(Total_Harvest_prop_scale = scale(Total_Harvest_prop)) 

df_comm_avg <- df_2 %>%
  dplyr::select(Site_Year_Code, Lowest_Common_Taxon_Name, Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum, Total_Harvest_prop) %>% ##total harvest proportion and percapita harvest proportion are the same
  separate(Site_Year_Code, c("Site", "Year"), sep = "_") %>%
  group_by(Site, Lowest_Common_Taxon_Name) %>%
  summarise_at(vars(Estimated_Total_Pounds_Harvested_sum, Percapita_Pounds_Harvested_sum, Total_Harvest_prop), list(avg = mean)) %>%
  dplyr::rename(Community = "Site")


##Convert to community-species matrix
df_comm_avg_wide <- df_comm_avg %>%
  dplyr:: select(Community, Lowest_Common_Taxon_Name, Percapita_Pounds_Harvested_sum_avg) %>%
   filter(Percapita_Pounds_Harvested_sum_avg != 0) %>% ##keep only values that are not 0 for percapita harvest, 
  spread(key = Lowest_Common_Taxon_Name, value = Percapita_Pounds_Harvested_sum_avg)
df_comm_avg_wide[is.na(df_comm_avg_wide)] <- 0
df_comm_avg_wide <- df_comm_avg_wide %>%
  remove_rownames %>% 
  column_to_rownames(var="Community")
##select only numerical rows
df_comm_avg_wide <- df_comm_avg_wide %>%
  dplyr::select(`Abalone`:`Wilson's Snipe`)

##standardise proportionally
df_comm_avg_wide<- decostand(df_comm_avg_wide, "rank", na.rm = TRUE)
##Now, calculate pairwise bray-curtis harvest dissimilarities between each community



bc_dist <- as.matrix(vegdist(df_comm_avg_wide, method = "bray"))
bc_dist <- as.data.frame(bc_dist)

##Convert the BC distnaces and Spatial distances into list form so can combine the dataframes

bc_dist <- as.dist(bc_dist)
bc_dist<- dist2list(bc_dist) %>%
  unite(Pair, col, row)

spatial_dist_km <- as.dist(spatial_dist_km)
spatial_dist_km <- dist2list(spatial_dist_km) %>%
  unite(Pair, col, row)


dist_df <- left_join(bc_dist, spatial_dist_km, by = "Pair") %>%
  dplyr::rename(Community_Pair = "Pair", BC_Sim = "value.x", Spatial_Distance_km = "value.y") %>%
  filter(Spatial_Distance_km != "0")


bc_sd_plot <- ggplot(dist_df, aes(x=Spatial_Distance_km, y = BC_Sim, width = 1000)) +
  geom_point() +
  geom_smooth() +
  theme_classic() +
  xlab("Spatial Distance (km)") +
  ylab("Bray-Curtis Similarity Index")
bc_sd_plot


##trying out binning 
df_bin <- dist_df %>%
  mutate(Spatial_Distance_Bin_km = case_when(Spatial_Distance_km <= 100 ~ '100',
                                             Spatial_Distance_km >= 101 & Spatial_Distance_km <= 200 ~ '200',
                                             Spatial_Distance_km >= 201 & Spatial_Distance_km <= 300 ~ '300',
                                             Spatial_Distance_km >= 301 & Spatial_Distance_km <= 400 ~ '400',
                                             Spatial_Distance_km >= 401 & Spatial_Distance_km <= 500 ~ '500',
                                             Spatial_Distance_km >= 501 & Spatial_Distance_km <= 600 ~ '600',
                                             Spatial_Distance_km >= 601 & Spatial_Distance_km <= 700 ~ '700',
                                             Spatial_Distance_km >= 701 & Spatial_Distance_km <= 800 ~ '800',
                                             Spatial_Distance_km >= 801 & Spatial_Distance_km <= 900 ~ '900',
                                             Spatial_Distance_km >= 901 & Spatial_Distance_km <= 1000 ~ '1000',
                                             Spatial_Distance_km >= 1001 & Spatial_Distance_km <= 1100 ~ '1100',
                                             Spatial_Distance_km >= 1101 & Spatial_Distance_km <= 1200 ~ '1200',)) 


df_bin_summary <- df_bin %>%
  group_by(Spatial_Distance_Bin_km) %>%
  summarise(mean_bc = mean(BC_Sim), sd_bc = sd(BC_Sim))


df_bin_summary$Spatial_Distance_Bin_km <- as.numeric(df_bin_summary$Spatial_Distance_Bin_km)
bc_sd_plot_bin <- ggplot(df_bin_summary, aes(x=Spatial_Distance_Bin_km, y = mean_bc)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  xlab("Spatial Distance (km)") +
  ylab("Average Bray-Curtis Similarity Index")
bc_sd_plot_bin

