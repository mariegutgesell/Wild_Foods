##Calculating average distance of communities to urban centers

library(tidyverse)
library(readxl)
library(sf) # functions start with st_*
# read + manipulate raster files
library(raster)
# Visualize in a web browser
library(mapview)

library(leafem)
library(htmltools)
library(leaflet)

library(geosphere)

sites_gps <- read_excel("data/CSIS_SurveyData_Demographics.xlsx", sheet = 2) %>%
  filter(!is.na(Longitude)) %>%
  dplyr::select(Community, Longitude, Latitude) %>%
  distinct() %>%
  mutate(community_type = "Rural")

##add coordinates for juneau, anchorage, ketchikan
sites_gps[nrow(sites_gps) + 1,] = list("Juneau",-134.428965,58.332135, "Urban")
sites_gps[nrow(sites_gps) + 1,] = list("Ketchikan",-131.672469,55.349982, "Urban")
sites_gps[nrow(sites_gps) + 1,] = list("Anchorage",-149.870128,61.171329, "Urban")

rural <- sites_gps%>%
  filter(community_type == "Rural")
urban <- sites_gps %>%
  filter(community_type == "Urban")

dist_matrix <- distm(
  x = rural[, c("Longitude", "Latitude")],
  y = urban[, c("Longitude", "Latitude")],
  fun = distHaversine
)

dist_matrix <- dist_matrix/1000

dist_df <- as.data.frame(dist_matrix) %>%
  rename(Juneau = "V1", Ketchikan = "V2", Anchorage = "V3") 

community_name <- rural$Community

dist_df <- cbind(community_name, dist_df)

##find nearest urban
nearest <- apply(dist_matrix, 1, function(x) {
  min_idx <- which.min(x)
  list(
    nearest_urban_id = urban$id[min_idx],
    distance_km = x[min_idx]
  )
})

results <- cbind(rural, do.call(rbind, lapply(nearest,function(x) {as.data.frame(t(unlist(x)))} )))

mean_nearest_distance <- results %>%
  summarise_at(vars(distance_km), list(mean, sd))
