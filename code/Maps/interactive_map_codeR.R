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


df_final <- read.csv("CT_2020_harvest_estimates_cost_servings_community_sum.csv") 

sites_gps <- read_excel("data/CSIS_SurveyData_Demographics.xlsx", sheet = 2) %>%
  unite(Site_Year_Code, c(Community,Year), sep = "_", remove = FALSE)

####Determine the surveys from the most representative year, make sure have all surveys/communities want to use for this analysis, remove double years
latest_surveys <- sites_gps %>%
  filter(Most_Rep_Year == "Yes") %>%
  filter(Site_Year_Code != "Beecher Pass_1987") %>% ##don't have a population estimate for 
  filter(Site_Year_Code != "Hoonah_2016") %>%
  filter(Site_Year_Code != "Yakutat_2000") %>%
  filter(Site_Year_Code != "Klukwan_1996") %>%
  rename(Harvest_Survey_Year = "Year", Forest = "National_Forest") #%>%
#  left_join(pop_df, by = c("Community")) #%>%
#  select(Site_Year_Code, Community, Year, Community_Population)


sites_gps <- left_join(latest_surveys, df_final, by = c("Forest", "Community", "Harvest_Survey_Year"))  %>%
  filter(!is.na(Longitude))


##for now, just simplifying to one point per community, but in future want to have way of having/showing data across years for a community
sites_gps <- sites_gps %>%
  dplyr::select(!X) %>%
  dplyr::select(!`Sampling Notes`)

sites_gps[] <- lapply(sites_gps, function(x) {
  if (is.numeric(x)) round(x, 2) else x
})
##stream maps -- all work, just look slightly different
sites_coord <- st_as_sf(sites_gps, coords = c("Longitude", "Latitude"), crs = 4326)
mapview(sites_coord, zcol = "Site_Year_Code", cex = "total_percapita_kg", legend = FALSE)
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

