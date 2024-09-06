## Process clear cuts between 2018-2020
## (demonstration related to CB work, not Metsätuhoraportti!)

library(tidyverse)
library(sf)

data_dir <- "./data/MK/"

fls <- list.files(data_dir, pattern="*.gpkg$")
flnameOut <- "./data/stormdamage/harvests_mantsakirja.gpkg"

##########################################################################'
## RUN THINGS 
##########################################################################'

## Remove output-files if they already exist (otherwise updated data will just append to the old file)
if(file.exists(flnameOut)) {
  file.remove(flnameOut)
} 

## Compute
for(i in 1:length(fls)) {
  fl_i <- fls[i]
  print(paste("start:", i, fl_i))
  mk_i <- st_read(paste0(data_dir, fl_i)) 
  mk_i <- mk_i%>%
    # filter(cuttingrealizationpractice == 5) %>%
    filter(declarationarrivaldate >= as.Date("2010-01-01")) %>%
    filter(declarationarrivaldate < as.Date("2020-01-01"))
  print(paste("Polygons ready, total polygons:", nrow(mk_i)))
  mk_centroids_i <- st_centroid(mk_i)
  
  # st_write(mk_i, dsn = flnameOut_stormPolygons, append = TRUE)
  st_write(mk_centroids_i, dsn = flnameOut, append = TRUE)
  
  print("Centroids calculated and added to file")
  
  rm(mk_centroids_i, mk_i)
}


##########################################################################'
## CHECK OUTPUTS
##########################################################################'

ccuts <- st_read(flnameOut)
dim(ccuts)
summary(ccuts$declarationarrivaldate)

suomi <- st_read("../data_and_bits/suomi_karttapohja/suomi_aland_etrstmfin.shp")

png(file="./outputs/mantsakirja_suomikartta2019.png", width=5, height = 6, unit = "in", res = 300)
ggplot(suomi) + geom_sf() +
  geom_sf(data=ccuts %>% 
            filter(declarationarrivaldate >= as.Date("2019-01-01")) %>%
            filter(declarationarrivaldate < as.Date("2020-01-01")), # %>% slice_sample(n=1e4),
          size=0.01) +
  theme_minimal()
dev.off()

sum(ccuts$area)/1e6
