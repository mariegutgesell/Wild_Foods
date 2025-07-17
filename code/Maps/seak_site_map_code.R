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
library(leaflet)


sites_gps <- read_excel("data/CSIS_SurveyData_Demographics.xlsx", sheet = 2) %>%
  filter(!is.na(Longitude)) %>%
  dplyr::select(Community, Longitude, Latitude) %>%
  distinct()

sites_gps <- sites_gps %>%
  mutate(focal_site = ifelse(Community %in% c("Metlakatla", "Hoonah", "Cordova"), "yes", "no"))

write.csv(sites_gps, "data/site_coordinates.csv")
sites_coord <- st_as_sf(sites_gps, coords = c("Longitude", "Latitude"), crs = 4326)
site_map <- mapview(sites_coord, map.types = "Esri.NatGeoWorldMap", legend = FALSE, col.regions = "black", alpha = 1, alpha.regions = 1, cex = 4)
site_map

#"USGS.USTopo"
##"Esri.NatGeoWorldMap"
mapview(map.types = "Esri.NatGeoWorldMap")


site_map

site_map2 <- mapview(sites_coord, map.types = "Esri.NatGeoWorldMap", legend = FALSE)
site_map2

##Map w/ National Forests
site_map <- leaflet() %>%
  addProviderTiles("https://basemap.nationalmap.gov/arcgis/rest/services/USGSTopo/MapServer/tile/{z}/{y}/{x}") %>%
  addMarkers(data = sites_gps, ~Longitude, ~Latitude)
site_map

##could make point size relative to the amount harvested, there are many options for how to make these interactive maps
# Create an icon list based on the condition
sites_gps$icon <- ifelse(sites_gps$focal_site == "yes", "star", "circle")

# Create custom icons with leaflet
icons <- awesomeIcons(
  icon = sites_gps$icon,
  iconColor = 'black',
  markerColor = ifelse(sites_gps$focal_site == "yes", 'red', 'black'),
  library = 'glyphicon'
)

my_icons2 <- iconList(
  circle = makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/black-circle-icon-23.png",
                    iconWidth = 18, iconHeight = 18),
  square = makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/black-square-frame-23.png",
                    iconWidth = 18, iconHeight = 18),
  triangle = makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/triangle-png-28.png",
                      iconWidth = 18, iconHeight = 18)
)

# Generate the leaflet map with custom icons
site_map <- leaflet(data = sites_gps) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addAwesomeMarkers(
    ~Longitude, ~Latitude, 
    icon = icons,
    popup = ~paste("Site: ", Community)  # Adjust this for your specific columns
  )

site_map


leaflet(sites_gps) %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  addCircleMarkers(~Longitude, ~Latitude, label = ~as.character(Community), radius = 3, color = "black", stroke = FALSE, fillOpacity = 1.0) %>%
  addScaleBar() %>%
  
 # addLabelOnlyMarkers(~Longitude, ~Latitude, label = ~htmlEscape(Community), labelOptions = labelOptions(noHide =TRUE, textOnly = TRUE, textsize = "10px", direction = "right"))


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


