#==========================================#
# Elaborado por: Eduard F Martinez-Gonzalez
# Ultima modificacion: 24-08-2021
# R version 4.1.1 (2021-08-10)
#==========================================#

# initial configuration
if (!require("pacman")) install.packages("pacman") # Isntalar pacman (sino está instalada)
require(pacman) # llamar pacman
p_load(tidyverse,viridis,sf,leaflet,osmdata,ggsn,skimr) # llamar y/o instalar librerias
if(sessionInfo()$loadedOnly$Rcpp$Version!="1.0.7") update.packages("Rcpp")

#=========================#
# [1.] OpenStreetMap Data #
#=========================#

# view keys
browseURL("https://wiki.openstreetmap.org/wiki/Map_features")

# get avaliables values for amenity
available_tags("amenity")

# get bbox
getbb("Quito Ecuador")

# get amenity data
ameni_osm = opq(bbox = getbb("Quito Ecuador")) %>%
              add_osm_feature(key = "amenity", value = "bar") %>%
              add_osm_feature(key = "amenity", value = "pub") %>%
              add_osm_feature(key = "building", value = "hotel") %>%
              osmdata_sf()
ameni_osm %>% class()
ameni_osm

# get sf
ameni_point = ameni_osm$osm_points
ameni_polygon = ameni_osm$osm_polygons

# plot data
leaflet() %>% addTiles() %>% 
addCircleMarkers(data=ameni_point , weight=1 , col="green") %>%
addPolylines(data=ameni_polygon,color="red",opacity=0.7,weight=4) 

# rbind data
ameni_point$osm_id %in% ameni_polygon$osm_id %>% table()
ameni_polygon = ameni_polygon %>% rename(name_pol = name)
join_amenity = st_intersection(ameni_point,ameni_polygon)
amenitys = rbind(ameni_point %>% select(osm_id,amenity) ,
                 ameni_polygon  %>% st_centroid() %>% select(osm_id,amenity)) %>%
           subset(is.na(amenity)==F)
amenitys %>% class()

# get street
street = opq(bbox = getbb("Quito Ecuador")) %>%
         add_osm_feature(key = "highway") %>%
         osmdata_sf()
street = street$osm_lines

# get boundary
quito = opq(bbox = getbb("Quito Ecuador")) %>%
        add_osm_feature(key = "boundary", value = "administrative") %>% osmdata_sf()
quito = quito$osm_multipolygons %>% subset(admin_level==9)

# plot basic map
p = ggplot() + geom_sf(data=quito, col="orange" , fill=NA , size=0.3) + 
geom_sf(data=street, col="black" , size=0.05)  + 
geom_sf(data=amenitys,size=0.8,shape=5,aes(col=amenity)) + theme_bw()
p 

# add scalebar and north symbol
p = p + north(data=quito , location="topleft") + 
scalebar(data=quito , dist=5 , dist_unit="km" , transform=T , model="WGS84")
p

# make zoom

p = p + coord_sf(xlim=c(-78.615,-78.44),ylim=c(-0.36,-0.05))
p

# remove axis-labels
p = p + labs(x="",y="","Amenitys")
p

# save plot
ggsave(plot=p , filename="clase_3/output/quito_map.pdf" , width=6.5 , height=8)



