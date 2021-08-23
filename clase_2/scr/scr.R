#==========================================#
# Elaborado por: Eduard F Martinez-Gonzalez
# Ultima modificacion: 23-08-2021
# R version 4.0.3 (2020-10-10)
#==========================================#

# initial configuration
if (!require("pacman")) install.packages("pacman") # Isntalar pacman (sino está instalada)
require(pacman) # llamar pacman
p_load(tidyverse,viridis,sf,leaflet,raster) # llamar y/o instalar librerias

#==============================#
# [2.] Introduccion a GIs en R #
#==============================#

# source
browseURL("https://geoportal.dane.gov.co/servicios/descarga-y-metadatos/descarga-mgn-marco-geoestadistico-nacional/")

# read shape
points = st_read("clase_2/input/points_barranquilla.shp")
points

# plot points
leaflet() %>% addTiles() %>% addCircleMarkers(data = points[1:5,])

# geometry
points %>% crs() # get CRS

points %>% st_bbox() # get bbox

points %>% st_geometry() # get vertices

# attributes
points %>% class() # get class

points %>% colnames() # get column names

points$MPIO_CCDGO %>% table() # frequency table

# add columns 
data = data.frame(MPIO_CCDGO= unlist(points$MPIO_CCDGO) %>% unique(),
                  rate = rnorm(n=23 , mean=1000 , sd=10) %>% round())

points = left_join(points,data,"MPIO_CCDGO") # add variable

points %>% head() # view

#====================================#
# [3.] Modelo de equilibrio espacial #
#====================================#

# load packages with census data
p_load(tidycensus) 

# get access to the Census API
browseURL("http://api.census.gov/data/key_signup.html")

# interface with the US Census Bureau data
census_api_key("YOUR CENSUS API HERE") # 

# get Median Housing Values Cook county
chicago = get_acs(geography="block group",variables="B25077_001E",state ="IL",county="Cook County",year=2016,geometry=T)

# get Median Housing Values Suffolk County
boston = get_acs(geography="block group",variables="B25077_001E",state ="MA",county="Suffolk County",year=2016,geometry = T)

# plot layers
ggplot() + geom_sf(data=boston , col="black" , fill=NA) + theme_bw()

# another plot
leaflet(boston) %>% addTiles() %>% addPolygons(color="green",fill=NA,weight=2)

# create city centers  
chicago_cbd = st_as_sf(x = read.table(text="-87.627800  41.881998"),
                       coords = c(1,2), crs = "+proj=longlat +datum=WGS84")

boston_cbd = st_as_sf(x = read.table(text="-71.057083  42.361145"),
                      coords = c(1,2),
                      crs = "+proj=longlat +datum=WGS84")

# put everything in the same projection
chicago_cbd = chicago_cbd %>% st_transform(st_crs(chicago))

boston_cbd = boston_cbd %>% st_transform(st_crs(boston))

# view data
leaflet(boston) %>% addTiles() %>% 
addPolygons(color="green",fill=NA,weight=2) %>% addCircleMarkers(data=boston_cbd,col="red",weight=3)

# create distances
chicago$dist_CBD = st_distance(chicago,chicago_cbd) 
boston$dist_CBD = st_distance(boston,boston_cbd) 

# change units to miles
chicago$dist_CBD = as.numeric(chicago$dist_CBD)*0.000621371 
boston$dist_CBD = as.numeric(boston$dist_CBD)*0.000621371

# plot map
ggplot() + geom_sf(data=boston , col="black" , aes(fill=dist_CBD)) + 
scale_fill_viridis(option="A" , alpha=0.9 , direction=-1 , name="Dist. CBD (miles)") +
geom_sf(data=boston_cbd , col = "green" , size = 5) + theme_bw()
ggsave("clase_2/output/map_distance_boston_cbd.pdf")

# prepare data
boston$City="Boston" # create name city
chicago$City="Chicago" # create name city
chicago=chicago %>% filter(dist_CBD<=10) # keep block groups in Cook County that are within 10 miles of the city center
st_geometry(chicago)=NULL
st_geometry(boston)=NULL
dta=rbind(chicago,boston) # stack data

# scaterplot
ggplot(dta, aes(x=dist_CBD, y=estimate, color=City)) +
geom_point(shape=1) + geom_smooth(method=lm) + xlab("Distance to CBD (miles)") +
ylab("Median Housing Prices ($)") + theme_bw()
ggsave("clase_2/output/figure_2.1.pdf")

