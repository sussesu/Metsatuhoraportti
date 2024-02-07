#################################################################################'
##
## Process MK data for 
##
#################################################################################'


library(tidyverse)
library(sf)

# flnameOut_stormPolygons <- "./data/stormdamage/stormPolygons_finland_2024.gpkg"
flnameOut_stormCentroids <- "./data/stormdamage/stormCentroids_finland_2024.gpkg"

data_dir <- "./data/MK/"
fls <- list.files(data_dir, pattern="*.gpkg")

## Remove output-files if they already exist (otherwise updated data will just append to the old file)
# if(file.exists(flnameOut_stormPolygons)) {
#   file.remove(flnameOut_stormPolygons)
# }
if(file.exists(flnameOut_stormCentroids)) {
  file.remove(flnameOut_stormCentroids)
} 

## Compute
for(i in 1:length(fls)) {
  print(paste("start:", i, fl_i))
  fl_i <- fls[i]
  mk_i <- st_read(paste0(data_dir, fl_i)) 
  mk_i <- mk_i%>%
    filter(forestdamagequalifier == 1504 & cuttingrealizationpractice%in%c(20, 21))
  print(paste("Polygons ready, total storm damage polygons:", nrow(mk_i)))
  mk_centroids_i <- st_centroid(mk_i)
  
  # st_write(mk_i, dsn = flnameOut_stormPolygons, append = TRUE)
  st_write(mk_centroids_i, dsn = flnameOut_stormCentroids, append = TRUE)
  
  print("Centroids calculated and added to file")
  
  rm(mk_centroids_i, mk_i)
}