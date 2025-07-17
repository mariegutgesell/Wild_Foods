## Radial Landuse of Communities in SE Alaska 
##Adapted from Kevin C's code on Feb 19, 2024 by Marie Gutgesell
##packages needed: raster, sf 

#' @param file_lu file pf LU file
#' @param sf sf object
extract_sf <- function(file_lu, sf) {
  ra <- raster(file_lu)
  extract(ra, st_transform(sf, crs = projection(ra)))
}

#' @param file_lu file pf LU file
#' @param sf sf object
#' @param buf buffer distance
extract_sf_buf <- function(file_lu, sf, buf) {
  ra <- raster(file_lu)
  extract(ra, st_buffer(st_transform(sf, crs = projection(ra)), buf))
}


#' @param xy coordinates of points to be used to do the extraction
#' @param buf buffer
#' @param crs coordinates reference system
extract_from_coords <- function(file_lu, xy, crs, buf) {
  ra <- raster(file_lu)
  st_as_sf(xy, coords = 1:2)
  extract(ra, st_transform(sf, crs = projection(ra)))
}



#' @param vc_lu vector of Land Use value
#' @param perc logical. Should the results be expressed as a percentage? Note that given that some categories are discarded this may nit sum up to 1.
simplify_lu <- function(vc_lu, perc = TRUE) {
  # NB 11 => unclassified (see guide)
  # 31 => water
  # 91 zones rocheuse, plages ...
  out <- data.frame(
    # Forest/Trees
    forest = sum(vc_lu %in% c(41, 42, 43, 44, 49)),
    #all natural (forest, wetlands, unmanaged grasslands, water)
    natural = sum(vc_lu %in% c(31, 41, 42, 43, 44, 49, 62, 71)),
    # settlement and roads
    urban = sum(vc_lu %in% c(21, 25)),
    # crops and grassland managed
    agricultural = sum(vc_lu %in% c(51, 52, 61))
  )
  if (perc) out/length(vc_lu) else out
}

library(raster)
library(sf)
library(readxl)
library(tidyverse)

#set working directory 
landuse <- read_excel("data/CSIS_SurveyData_Demographics.xlsx", sheet = 2) %>%
  dplyr::select(Community, Longitude, Latitude) %>%
  distinct() %>%
  filter(!is.na(Longitude))

# Example
pts <- st_as_sf(landuse,
                coords = c("Longitude", "Latitude"),
                crs = 4326) # %>% st_transform(crs = 3161)

# this file was dowloaded
fl <- "data/landcover_data/NLCD_2011_Land_Cover_AK_20200724/NLCD_2011_Land_Cover_AK_20200724.ige"

extract_sf(fl, pts)


bufs <- c(1000, 5000, 10000, 20000, 50000) ##for now starting w/ 1 - 50km buffers
res <- list()
for (i in seq_along(bufs)) {
  res[[i]] <- extract_sf_buf(fl, pts, bufs[i])
}

saveRDS(res, "land_use_raw.rds")

res_simplified <- lapply(
  res,
  function(x) cbind(
    sitecode = pts$sitecode,
    do.call(rbind, lapply(x, simplify_lu))
  )
)

names(res_simplified) <- paste0("bdist_", bufs)
saveRDS(res_simplified, "land_use_simplified.rds")

landuse_250 <- res_simplified[["bdist_250"]]
write.csv(landuse_250, "data/Land-Use Data/landuse_250m_radius_allsites.csv")

landuse_250_m3 <- landuse_250 %>%
  filter(sitecode == "AT" | sitecode == "HC" |sitecode=="P4")
write.csv(landuse_250_m3, "~/Desktop/Seasonal Stream Food Webs/Data/Land-use Data/landuse_250_m3.csv")