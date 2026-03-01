#EEP PROJECT - 01/03/2026 
#This code is me trying to make a map of all the data

#am using this tutorial: https://docs.ropensci.org/osmplotr/articles/basic-maps.html#osmdata


#load libraries
library(tidyverse)
library(readxl)
library(osmdata) #using this 
library(osmplotr) #using this 
library(sf)



#create area for map 
bbox<-get_bbox(c(-3.307356, 55.886465, -2.95, 56.00))

#get all the layers 

#landuse
landq <- opq(bbox = bbox) |>
  add_osm_feature(key = "landuse", value = c("grass", "meadow", "forest", "farmland", "recreation_ground"))
datalanduse <- osmdata_sf(landq)$osm_polygons

#water
watq<- opq(bbox = bbox) |>
  add_osm_feature(key = "natural", value =c("water", "coastline"))
datwat<- osmdata_sf(watq)$osm_polygons

#woods
datwood<- extract_osm_objects (key = "natural", value="wood", bbox = bbox)

#roads
roadq<-opq(bbox=bbox) |>
  add_osm_feature(key = "highway", value = c("motorway", "primary", "secondary", "tertiary", "residential"))
datroads <- osmdata_sf(roadq)$osm_polygons

#buildings 
datbuildings <- extract_osm_objects(key = "building", bbox = bbox)


#add the layers 

#basemap
map<- osm_basemap(bbox = bbox, bg = "blue")

#water
map<- add_osm_objects (map, datwat, col="#59B7D9")

#landuse
map<- add_osm_objects (map, datalanduse, col = "gray40")

#woods
map <- add_osm_objects(map, datwood, col = "#238B45")

#buildings 
map <- add_osm_objects(map, datbuildings, col = "grey40")

#roads
map <- add_osm_objects(map, datroads, col = "white", lwd = 0.6)

#printmap
map
