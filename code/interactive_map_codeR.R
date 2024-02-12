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

setwd("~/Desktop/Wild Foods Repo/")
source("code/1_dataframe_formation.R")
rm(list = ls()[!ls() %in% c("df_final")])

sites_gps <- read_excel("data/CSIS_SurveyData_Demographics.xlsx", sheet = 2) %>%
  unite(Site_Year_Code, c(Community,Year), sep = "_", remove = FALSE)

##
harvest_total_df <- df_final %>%
  dplyr::select(Site_Year_Code, Taxa_lvl1, Taxa_lvl2, Taxa_lvl3, Taxa_lvl4, Taxa_lvl5, Estimated_Total_Pounds_Harvested) %>%
  filter(Estimated_Total_Pounds_Harvested > 0) %>%
  ungroup() %>%
  group_by(Site_Year_Code) %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(est_total_mt = (Estimated_Total_Pounds_Harvested*0.45359237)/1000) ##metric tons conversion

sites_gps <- left_join(sites_gps, harvest_total_df, by = "Site_Year_Code") 


##for now, just simplifying to one point per community, but in future want to have way of having/showing data across years for a community
sites_gps <- sites_gps %>%
  dplyr::select(Site_Year_Code, Latitude, Longitude, est_total_mt) %>%
  distinct()

##stream maps -- all work, just look slightly different
sites_coord <- st_as_sf(sites_gps, coords = c("Longitude", "Latitude"), crs = 4326)
mapview(sites_coord, zcol = "Site_Year_Code", cex = "est_total_mt")
##could make point size relative to the amount harvested, there are many options for how to make these interactive maps

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
  filter(Site_Year_Code %in% c("Edna Bay_1987", "Skagway_1987"))

eb_img <- "data/popup_graphs/eb_1987.jpg"
sg_img <- "data/popup_graphs/skag_1987.jpg"

img <- rbind(eb_img, sg_img)
getwd()

img_eb_local <- paste0(getwd(), "/data/popup_graphs/eb_1987.jpg")
img_sg_local <- paste0(getwd(), "/data/popup_graphs/skag_1987.jpg")

img_2 <- c(paste0(getwd(),"/data/popup_graphs/eb_1987.jpg"),  paste0(getwd(),"/data/popup_graphs/skag_1987.jpg"))


library(leafpop)
test_coord <- st_as_sf(test_coord, coords = c("Longitude", "Latitude"), crs = 4326)
sample_map <- mapview(test_coord, alpha = 1, alpha.region = 1, map.type = "Esri.WorldImagery",
        popup = popupImage(img, src = "local"))
sample_map

sample_map_2 <- mapview(list(test_coord, sites_coord), alpha = 1, alpha.region = 1,
                      popup = popupImage(img, src = "local"))
sample_map_2

library(webshot)

mapshot(sample_map, url =  "data/popup_graphs/sample_interactive_map.html")

sample_map_2 <- mapview(test_coord, popup = FALSE)@map %>%
 addPopupImages(img_2, group = "test_coord")
sample_map_2

mapshot(sample_map_2, url =  paste0(getwd(), "/data/popup_graphs/sample_interactive_map_2.html"))
#library(remotes)
#remotes::install_github("r-spatial/mapview")

##image not working in saved url... 

grep"png" sample_interactive_map.html

