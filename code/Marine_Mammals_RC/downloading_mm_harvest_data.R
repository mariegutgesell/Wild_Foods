##testing reading in marine mammal data for raven

library(foreign)

sealions <- read.dbf("~/Downloads/Aerial Sealions Observations Prince William Sound_2008_2021/Sealions_2008_2021_WGS84.dbf", as.is = F)
