##Interactive Map of Harvest Structure


##Adapted from R code by K. Cazelles 
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

sites_gps <- read_excel("data/CSIS_SurveyData_Demographics.xlsx", sheet = 2) 


##for now, just simplifying to one point per community, but in future want to have way of having/showing data across years for a community
sites_gps <- sites_gps %>%
  dplyr::select(Community, Latitude, Longitude) %>%
  distinct()

##stream maps -- all work, just look slightly different
sites_coord <- st_as_sf(sites_gps, coords = c("Longitude", "Latitude"), crs = 4326)
mapview(sites_coord, zcol = "Community") %>%
  addStaticLabels(label = sites_coord$Community,
                  noHide = TRUE,
                  direction = 'right',
                  textOnly = TRUE,
                  textsize = "15px")

leaflet(sites_gps) %>%
  addTiles %>%
  addMarkers(~Longitude, ~Latitude, label = ~htmlEscape(Community), labelOptions = labelOptions(noHide = TRUE))

leaflet(sites_gps) %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  addLabelOnlyMarkers(~Longitude, ~Latitude, label = ~as.character(Community), 
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T, textsize = "10px" )) %>%
  addScaleBar()

leaflet(sites_gps) %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  addCircleMarkers(~Longitude, ~Latitude, label = ~as.character(Community), radius = 3, color = "black", stroke = FALSE, fillOpacity = 1.0) %>%
  addScaleBar() %>%
  addLabelOnlyMarkers(~Longitude, ~Latitude, label = ~htmlEscape(Community), labelOptions = labelOptions(noHide =TRUE, textOnly = TRUE, textsize = "10px", direction = "right"))



##so we can add pop-up tables, figures, or images, as well as interactive links using mapview, can even do videos, wow so cool
##https://r-spatial.github.io/mapview/articles/mapview_04-popups.html

##trying to see if can do a popup image -- lets try this for now for the two site where we have that food web visualization, just to give an example
##have food web images so far for skagway and edna bay 1987
test_coord <- sites_gps %>%
  filter(Community %in% c("Edna Bay", "Skagway"))

eb_img <- "data/foodweb_images/eb_1987.jpg"
sg_img <- "data/foodweb_images/skag_1987.jpg"

img <- rbind(eb_img, sg_img)

test_coord <- cbind(test_coord, img)


library(leafpop)
test_coord <- st_as_sf(test_coord, coords = c("Longitude", "Latitude"), crs = 4326)
mapview(test_coord, zcol = "Community",
        popup = popupImage(img, src = "local"))
