library(sf)
library(tidyverse)
library(rgdal)
library(stringr)
library(lwgeom)

load('la_census.Rdata')
census <- read.csv('census_codebook.csv')
council <- st_read("Council Districts/geo_export_60b76ffd-e8a1-4e69-99a8-32e57169a1c5.shp")
dtla <- st_read("DTLA.kml")
neigh <- st_read('Neighborhood Councils/geo_export_398e0866-faf4-4573-a4a5-5709996b9bb6.shp')
#council <- council%>%filter(district%in%c(1,14,9))
la_census$census <- str_split_fixed(la_census$NAME, ",", 2)[,1]
la_census$census <- str_replace(la_census$census,'Census Tract','CT')
la_census$Nothing <- 1
la_census <- la_census%>%st_set_crs(st_crs(council))
la_census <- st_join(la_census, council,left = FALSE, largest = TRUE)
#dtla <- dtla%>%st_set_crs(st_crs(council))
#dt_census <- st_join(la_census,dtla,left = FALSE, largest = TRUE)
#exp_census <- la_census%>%filter(!NAME%in%dt_census$NAME)


