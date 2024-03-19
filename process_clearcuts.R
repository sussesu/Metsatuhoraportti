## Process clear cuts between 2018-2020
## (demonstration related to CB work, not Metsätuhoraportti!)

library(tidyverse)
library(sf)

data_dir <- "./data/MK/"

fls <- list.files(data_dir, pattern="*.gpkg$")
flnameOut_clearcuts_post2018 <- "./data/stormdamage/clearCuts_finland_post2018.gpkg"

## Remove output-files if they already exist (otherwise updated data will just append to the old file)
if(file.exists(flnameOut_clearcuts_post2018)) {
  file.remove(flnameOut_clearcuts_post2018)
} 

## Compute
for(i in 1:length(fls)) {
  fl_i <- fls[i]
  print(paste("start:", i, fl_i))
  mk_i <- st_read(paste0(data_dir, fl_i)) 
  mk_i <- mk_i%>%
    filter(cuttingrealizationpractice == 5) %>%
    filter(declarationarrivaldate >= as.Date("2018-01-01")) %>%
    filter(declarationarrivaldate < as.Date("2021-01-01"))
  print(paste("Polygons ready, total polygons:", nrow(mk_i)))
  mk_centroids_i <- st_centroid(mk_i)
  
  # st_write(mk_i, dsn = flnameOut_stormPolygons, append = TRUE)
  st_write(mk_centroids_i, dsn = flnameOut_clearcuts_post2018, append = TRUE)
  
  print("Centroids calculated and added to file")
  
  rm(mk_centroids_i, mk_i)
}


# check file

ccuts <- st_read(flnameOut_clearcuts_post2018)
dim(ccuts)
summary(ccuts$declarationarrivaldate)
