##Site Map Code 


#### Packages ####
# read manipulate vector files tidyverse friendly
# introduce sf sfg and sfc objects
# rgeos
library(sf) # functions start with st_*
# read + manipulate raster files
library(raster)
# Visualize in a web browser
library(mapview)
# we already have a couple of nice tuto about it!
library(tidyverse)

library(leafem)
library(htmltools)
library(leaflet)
library(readxl)


sites_gps <- read_excel("data/CSIS_SurveyData_Demographics.xlsx", sheet = 2) %>%
  filter(!is.na(Longitude)) %>%
  dplyr::select(Community, Longitude, Latitude) %>%
  distinct()

sites_coord <- st_as_sf(sites_gps, coords = c("Longitude", "Latitude"), crs = 4326)
site_map <- mapview(sites_coord, map.types = "Esri.NatGeoWorldMap", legend = FALSE, col.regions = "black", alpha = 1, alpha.regions = 1, cex = 3)

mapview(map.types = "CartoDB.Positron")
site_map
##could make point size relative to the amount harvested, there are many options for how to make these interactive maps

leaflet(sites_gps) %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  addCircleMarkers(~Longitude, ~Latitude, label = ~as.character(Community), radius = 3, color = "black", stroke = FALSE, fillOpacity = 1.0) %>%
  addScaleBar() %>%
  addLabelOnlyMarkers(~Longitude, ~Latitude, label = ~htmlEscape(Community), labelOptions = labelOptions(noHide =TRUE, textOnly = TRUE, textsize = "10px", direction = "right"))


library(webshot)

mapshot(site_map, file =  "figures/site_map_SEAK.png")


##Map of 3 sites for NIFA proposal
nifa_sites <- sites_gps %>%
  filter(Community %in% c("Metlakatla", "Hoonah", "Cordova"))

leaflet(nifa_sites) %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  addCircleMarkers(~Longitude, ~Latitude, label = ~as.character(Community), radius = 3, color = "black", stroke = FALSE, fillOpacity = 1.0) %>%
  addScaleBar() %>%
  addLabelOnlyMarkers(~Longitude, ~Latitude, label = ~htmlEscape(Community), labelOptions = labelOptions(noHide =TRUE, textOnly = TRUE, textsize = "10px", direction = "right"))

nifa_sites_coord <- st_as_sf(nifa_sites, coords = c("Longitude", "Latitude"), crs = 4326)
site_map <- mapview(nifa_sites_coord, map.types = "Esri.NatGeoWorldMap" , legend = FALSE, col.regions = "red", alpha = 1, alpha.regions = 1, cex = 10)
site_map


