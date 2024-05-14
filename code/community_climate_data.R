##Climate data for communities

remotes::install_github("mikejohnson51/AOI") # suggested!
remotes::install_github("mikejohnson51/climateR")

library(AOI)
library(climateR)
library(sf)
library(raster)
library(rasterVis)
library(dplyr)

#set working directory 
landuse <- read_excel("data/CSIS_SurveyData_Demographics.xlsx", sheet = 2) %>%
  dplyr::select(Community, Longitude, Latitude) %>%
  distinct() %>%
  filter(!is.na(Longitude))

# Example
pts <- st_as_sf(landuse,
                coords = c("Longitude", "Latitude"),
                crs = 4326) # %>% st_transform(crs = 3161)


##Trying to extract temperature data
tc_prcp = getTerraClim(pts, startDate = "1983-01-01")

tc_