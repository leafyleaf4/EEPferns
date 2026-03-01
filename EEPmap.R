#EEP PROJECT - 01/03/2026 
#This code is me trying to make a map of all the data

#am using this tutorial: https://docs.ropensci.org/osmplotr/articles/basic-maps.html#osmdata


#load libraries
library(tidyverse)
library(readxl)
library(maps) #not this? 
library(osmdata) #using this 
library(ggmap)#not this? 
library(osmplotr) #using this 



#oklay that didnt work will try again 


bbox<-get_bbox(c(-3.307356, 55.886465, -3.116, 56.00))
datalanduse<- extract_osm_objects(key="landuse", value=, bbox=bbox)
datwat<- extract_osm_objects (key = "natural", value="water", bbox = bbox)
#datwat2<- extract_osm_objects (key = "natural", value="coastline", bbox = bbox)
datwood<- extract_osm_objects (key = "natural", value="wood", bbox = bbox)

map<- osm_basemap(bbox = bbox, bg = "white")
map<- add_osm_objects (map, datalanduse, col = "gray40")
map<- add_osm_objects (map, datwat, col="#59B7D9")
#map<- add_osm_objects (map, datwat2, col="#59B7D9")
map<- add_osm_objects (map, datwood, col="green")


map
