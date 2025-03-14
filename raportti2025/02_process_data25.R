#################################################################################'
##
## Process MK data for the Metsatuhoraportti 2025 (tuhot 2024)
##
#################################################################################'

library(tidyverse)
library(sf)

flnameOut_stormCentroids <- "./data/stormdamage/stormCentroids_finland_2025.gpkg"
flnameOut_snowCentroids <- "./data/stormdamage/snowCentroids_finland_2025.gpkg"


data_dir <- "./data/MK2025/"
fls <- list.files(data_dir, pattern="*.gpkg")

## Remove output-files if they already exist (otherwise updated data will just append to the old file)
# if(file.exists(flnameOut_stormPolygons)) {
#   file.remove(flnameOut_stormPolygons)
# }
if(file.exists(flnameOut_stormCentroids)) {
  warning("THIS FILE EXISTS, ARE YOU SURE YOU WANT TO DELETE IT?????? PROCEED WITH CAUTION!!!")
} 

if(file.exists(flnameOut_stormCentroids)) {
  file.remove(flnameOut_stormCentroids)
} 

## Compute for storm damage
for(i in 1:length(fls)) {
  fl_i <- fls[i]
  print(paste("start:", i, fl_i))
  mk_i <- st_read(paste0(data_dir, fl_i)) 
  mk_i <- mk_i%>%
    filter(forestdamagequalifier == 1504 & cuttingrealizationpractice%in%c(20, 21))
  print(paste("Polygons ready, total storm damage polygons:", nrow(mk_i)))
  mk_centroids_i <- st_centroid(mk_i)
  
  st_write(mk_centroids_i, dsn = flnameOut_stormCentroids, append = TRUE)
  
  print("Centroids calculated and added to file")
  
  rm(mk_centroids_i, mk_i)
}

## Compute for snow damage
for(i in 1:length(fls)) {
  fl_i <- fls[i]
  print(paste("start:", i, fl_i))
  mk_i <- st_read(paste0(data_dir, fl_i)) 
  mk_i <- mk_i%>%
    filter(forestdamagequalifier == 1501)
  print(paste("Polygons ready, total snow damage polygons:", nrow(mk_i)))
  mk_centroids_i <- st_centroid(mk_i)
  
  st_write(mk_centroids_i, dsn = flnameOut_snowCentroids, append = TRUE)
  
  print("Centroids calculated and added to file")
  
  rm(mk_centroids_i, mk_i)
}

