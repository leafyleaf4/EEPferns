#EEP PROJECT - 01/03/2026 
#This code is me trying to make a map of all the data

#am using this tutorial: https://docs.ropensci.org/osmplotr/articles/basic-maps.html#osmdata


#load libraries
library(tidyverse)
library(readxl)
library(osmdata) #using this 
library(osmplotr) #using this 
library(sf)
library(ggspatial)



#create area for map 
bbox<-get_bbox(c(-3.284696, 55.914, -3.144, 55.961653))





#load in data
ferndata<-read_excel("ferndata.xlsx")


data_sf<-st_as_sf(ferndata, coords = c("Longitude", "Latitude"), crs = 4326)
data_sf <- st_transform(data_sf, crs = st_crs(datalanduse))

#get all the layers 
#gotta run this one bit by bit all slowly or it will break 

#landuse
landq <- opq(bbox = bbox) |>
  add_osm_feature(key = "landuse", value = c("grass", "meadow", "forest", "farmland", "recreation_ground"))
datalanduse <- osmdata_sf(landq)$osm_polygons

#water
watq<- opq(bbox = bbox) |>
  add_osm_feature(key = "natural", value =c("water"))
datwat<- osmdata_sf(watq)$osm_polygons

#woods
datwood<- extract_osm_objects (key = "natural", value="wood", bbox = bbox)

#roads
roadq<-opq(bbox=bbox) |>
  add_osm_feature(key = "highway", value = c("motorway", "primary", "secondary", "tertiary", "residential"))
datroads <- osmdata_sf(roadq)$osm_lines

#buildigs 
datbuildings <- extract_osm_objects(key = "building", bbox = bbox)

#protected area
parkq <- opq(bbox = bbox) %>% 
  add_osm_feature(key = "leisure", value = "parks")
dataparks <- osmdata_sf(parkq)$osm_polygons


#protected area
protq <- opq(bbox = bbox) %>% 
  add_osm_feature(key = "leisure", value = c("nature_reserve", "protected_area"))
dataprot <- osmdata_sf(parkq)$osm_polygons
dataprot<- st_transform(dataprot, crs_use)

#all nature
allnatureq <- opq(bbox = bbox) %>%
  add_osm_feature(key = "Natural")
datallnature <- osmdata_sf(scrubq)$osm_polygons
datallnature<- st_transform(datallnature, crs_use)

#amenities
amenq<- opq(bbox=bbox) %>% 
  add_osm_feature(key="Amenity")
datamenity<-osmdata_sf(amenq)$osm_polygons
datamenity<- st_transform(datamenity, crs_use)


#change to national grid references 
crs_use <- 27700  

datalanduse  <- st_transform(datalanduse, crs_use)
datwat       <- st_transform(datwat, crs_use)
datwood      <- st_transform(datwood, crs_use)
datroads     <- st_transform(datroads, crs_use)
datbuildings <- st_transform(datbuildings, crs_use)
dataparks <- st_transform(dataparks, crs_use)


#make map 

map2 <- ggplot() +
  theme_void() +
  theme( panel.background = element_rect(fill = "grey40")) +
  geom_sf(data=datallnature, fill="#249E3C")+
  geom_sf(data = dataparks, fill = "#9BCB8B")+
  geom_sf(data=dataprot, fill = "#10591D")+
  geom_sf(data = datalanduse,fill = "#238B45") +
  geom_sf(data = datwood, fill = "darkgreen") +
  geom_sf(data = datbuildings, fill = "black" ) +
  geom_sf(data=datamenity, fill = "black")+
  geom_sf(data = datroads, aes(linewidth = highway), colour = "grey90") +
  geom_sf(data = datwat, fill = "#59B7D9") +
  scale_linewidth_manual(values = c(motorway = 0.9, primary = 0.6, secondary = 0.4, tertiary = 0.2,residential = 0.1))+
  annotation_scale(location = "bl") +
  annotation_north_arrow( location = "tl", which_north = "true", style = north_arrow_orienteering())+
  theme(legend.position = "none")+
  geom_sf(data=data_sf, colour="yellow", size=1)

map2





# checking thingies


#okay i am officially going insane but i downloaded data from os digimaps hopefully thatll work 




#not using the following bit anymore - using ggplot instead 


#add the layers 

#basemap
map<- osm_basemap(bbox = bbox, bg = "grey20")

#water
map<- add_osm_objects (map, datwat, col="#59B7D9")

#landuse
map<- add_osm_objects (map, datalanduse, col = "#238B45")

#woods
map <- add_osm_objects(map, datwood, col = "darkgreen")

#buildings 
map <- add_osm_objects(map, datbuildings, col = "grey40")

#roads
map <- add_osm_objects(map, datroads, col = "grey90", size = 0.5)

#printmap
map




