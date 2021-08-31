#==========================================#
# Elaborado por: Eduard F Martinez-Gonzalez
# Ultima modificacion: 31-08-2021
# R version 4.1.1 (2021-08-10)
#==========================================#

# initial configuration
rm(list=ls())
require(pacman) # llamar pacman
p_load(tidyverse,sf,osmdata,sp,ncdf4,raster,stars,getSpatialData,viridis,leaflet,png,grid) # llamar y/o instalar librerias

# raster: raster(), stack() 
# stars: read_stars()
# png, grid: leer graficos

#=================#
# [1.] Motivacion #
#=================#

# Night lights and economic growth
dev.off()
grid.raster(readPNG("clase_5/pics/Indonesia.png")) # Tomado de: Measuring Economic Growth from Outer Space
grid.raster(readPNG("clase_5/pics/Rwanda.png")) # Tomado de: Measuring Economic Growth from Outer Space
browseURL("https://www.aeaweb.org/articles?id=10.1257/aer.102.2.994") # Ir a: Measuring Economic Growth from Outer Space

# Night lights and Covid-19
dev.off()
grid.raster(readPNG("clase_5/pics/covid.png")) 
browseURL("https://economia.uniandes.edu.co/sites/default/files/observatorio/Resultados-luminosidad.pdf") # Ir a: COVID19 y actividad económica: demanda de energía y luminosidad ante la emergencia

#==================================#
# [2.] Introduccion a datos raster #
#==================================#

## qué es un raster?
dev.off()
grid.raster(readPNG("clase_5/pics/raster.png")) 

## resolucion
dev.off()
grid.raster(readPNG("clase_5/pics/rasterize.png")) 

## bandas de un raster
dev.off()
grid.raster(readPNG("clase_5/pics/rgb_raster.png")) # Imagen tomada de https://www.neonscience.org

## importar raster de luces
luces_r = raster('clase_5/input/colombia_202003.tif')
luces_r
luces_s = read_stars("clase_5/input/colombia_202003.tif")
luces_s
0.00416667*111000 # resolucion

## geometria
st_bbox(luces_s)
st_crs(luces_s)
st_dimensions(luces_s)

## atributos
names(luces_s)
names(luces_s) = "date_202003"

## valores del raster
luces_s[[1]] %>% max(na.rm = T)
luces_s[[1]] %>% min(na.rm = T)
luces_s[[1]] %>% as.vector() %>% summary() 
luces_s[[1]][is.na(luces_s[[1]])==T] # Reemplazar NA's
luces_s[[1]][2000:2010,2000:2010] %>% table() # Sustraer una parte de la matriz

## puedo reproyectar un raster?
st_crs(luces_s)
luces_new_crs = st_transform(luces_s,crs=4126)
luces_s[[1]][2000:2010,2000:2010] # no se alteran las geometrias
luces_new_crs[[1]][2000:2010,2000:2010] # no se alteran las geometrias

## hacer clip a un raster
medellin = opq(bbox = getbb("Medellín Colombia")) %>%
           add_osm_feature(key = "boundary", value = "administrative") %>% osmdata_sf()
medellin = medellin$osm_multipolygons %>% subset(admin_level==7) %>% subset(name=="Zona Urbana Medellín")
ggplot() + geom_sf(data=medellin , col="red") + theme_bw() 

luces_medellin_1 = st_crop(luces_s,medellin) # crop luces de Colombia con polygono de Medellin

ggplot() + geom_stars(data=luces_medellin_1 , aes(y=y,x=x,fill=date_202003)) + # plot raster
scale_fill_viridis(option="A" , na.value='white') +
geom_sf(data=medellin , fill=NA , col="green") + theme_bw() 

#==========================#
# [3.] Extraer informacion #
#==========================#

## load data
luces_medellin_0 = read_stars("clase_5/input/colombia_202002.tif") %>% 
                   st_crop(.,medellin)
names(luces_medellin_0) = "date_202002"

ggplot() + geom_stars(data=luces_medellin_0 , aes(y=y,x=x,fill=date_202002)) + # plot raster
scale_fill_viridis(option="A" , na.value='white') +
geom_sf(data=medellin , fill=NA , col="green") + theme_bw() 

## apilar raster
luces_medellin = c(luces_medellin_0,luces_medellin_1)
luces_medellin

## get building data
house = opq(bbox = getbb("Medellín Colombia")) %>% add_osm_feature(key = "building", value = "house") %>% 
        osmdata_sf() %>% .$osm_polygons %>% .[,c("osm_id")] %>% mutate(building="house")

commercial = opq(bbox = getbb("Medellín Colombia")) %>% add_osm_feature(key = "building", value = "commercial") %>% 
             osmdata_sf() %>%  .$osm_polygons %>% .[,c("osm_id")]  %>% mutate(building="commercial")

hotel = opq(bbox = getbb("Medellín Colombia")) %>% add_osm_feature(key = "building", value = "hotel") %>% 
        osmdata_sf() %>%  .$osm_polygons %>% .[,c("osm_id")]  %>% mutate(building="hotel")

construction = opq(bbox = getbb("Medellín Colombia")) %>% add_osm_feature(key = "building", value = "construction") %>% 
               osmdata_sf() %>%  .$osm_polygons %>% .[,c("osm_id")]  %>% mutate(building="construction")

building = rbind(house,commercial) %>% rbind(.,hotel) %>% rbind(.,construction) %>% .[medellin,]
saveRDS(building,"clase_5/output/building.rds")

building = readRDS("clase_5/output/building.rds")

building %>% head()

ggplot() + geom_sf(data = building , aes(fill=building) , col=NA)

## extraer informacion de un raster (opcion 2)
luces_building = st_extract(x = luces_medellin, at = building) %>% st_as_sf()

luces_building %>% head()

## extraer informacion de un raster (opcion 2)
luces_medellin_sf = st_as_sf(x = luces_medellin, as_points = T, na.rm = T) # raster to sf (points)

luces_medellin_sf2 = st_as_sf(x = luces_medellin, as_points = F, na.rm = T) # raster to sf (polygons)

luces_building_2 = st_join(x=building , y=luces_medellin_sf2 , largest=F) # join layers

## variacion promedio 
df = luces_building_2

st_geometry(df) = NULL

df %>% group_by(building) %>% 
summarise(pre=mean(date_202002,na.rm=T) , post=mean(date_202003,na.rm=T))

#====================#
# [4.] Download Data #
#====================#

## Datos de Luces Nocturnas
browseURL("https://www.ngdc.noaa.gov/eog/dmsp/downloadV4composites.html") # 1992-2013
browseURL("https://payneinstitute.mines.edu/eog/") # Datos VIIRS mensuales 201204-2021...
cat("Ver códigos")

browseURL("https://figshare.com/articles/dataset/Harmonization_of_DMSP_and_VIIRS_nighttime_light_data_from_1992-2018_at_the_global_scale/9828827/2") # 1992-2018
cat("Ver paper")

## Datos de la NASA
browseURL("https://disc.gsfc.nasa.gov")
cat("Ver código")

## getSpatialData
browseURL("https://github.com/16EAGLE/getSpatialData")
if(!require(devtools)) install.packages("devtools",dependencies=T)
if(!require(getSpatialData)) devtools::install_github("16EAGLE/getSpatialData")

## log in: EARTHDATA
browseURL("https://urs.earthdata.nasa.gov")    
login_earthdata(username = "ef.martinezg")

## log in: Copernicus Open Access Hub
browseURL("https://scihub.copernicus.eu/dhus/#/self-registration")    
login_CopHub(username = "ef.martinezg")

# Print the status of all services:
services()

## ver datos disponibles
get_products()

#
medellin_sp = as_Spatial(medellin)
set_aoi(medellin_sp)

##
records = get_records(time_range = c("2021-05-15", "2021-05-20"),
                       products = c("sentinel-2"))

##
records = check_availability(records, verbose = TRUE)
records = subset(records,download_available==T & level=="Level-2A")


##  
view_records(records)
records = get_previews(records,dir_out = "clase_5/output") 
sentinel = stack("clase_5/output/sentinel-2/S2A_MSIL2A_20210516T153621_N0300_R068_T18NVM_20210516T194809_preview.tif")
plot(sentinel)
plotRGB(sentinel,r = 1, g = 2, b = 3) 

##
records = calc_cloudcov(records,dir_out = "clase_5/output") 

## Descargar los raster
records <- get_data(records,dir_out = "clase_5/output")

#====================#
# [5.] Interpolacion #
#====================#









