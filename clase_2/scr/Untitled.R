


install.packages("pacman") # Instalar librería
require(pacman) # Llamar la librería
p_load(tidycensus,sf,dplyr,ggplot2) # Instala y llama las librerías

# Conectarse API
census_api_key("2318bd0a022364803267f00e8bfb6e97b96df613")

# Obtener el valor medio de la vivienda en el condado de Cook
chicago = get_acs(geography="block group",variables="B25077_001E",state ="IL",county="Cook County",year=2016,geometry=T)

# Obtener el valor medio de la vivienda en el condado de Suffolk County
boston = get_acs(geography="block group",variables="B25077_001E",state ="MA",county="Suffolk County",year=2016,geometry = T)

# pintar mapas
ggplot() + geom_sf(data = boston , col = "black" , fill = NA) + 
           geom_sf(data = boston_cbd , col = "red" , size = 5)
  
# crear 
chicago_cbd=st_as_sf(x = read.table(text="-87.627800  41.881998"),
                     coords = c(1,2), crs = "+proj=longlat +datum=WGS84")

boston_cbd=st_as_sf(x = read.table(text="-71.057083  42.361145"),
                    coords = c(1,2),
                    crs = "+proj=longlat +datum=WGS84")

chicago_cbd = chicago_cbd %>% st_transform(st_crs(chicago))

boston_cbd = boston_cbd %>% st_transform(st_crs(boston))

# Calcular la distancia (metros)
chicago$dist_CBD = st_distance(chicago,chicago_cbd) 
boston$dist_CBD = st_distance(boston,boston_cbd) 

# Convertir distancia de metros a millas
chicago$dist_CBD = as.numeric(chicago$dist_CBD)*0.000621371 
boston$dist_CBD = as.numeric(boston$dist_CBD)*0.000621371

chicago %>% head() # veamos que tenemos


# pintar mapas
ggplot() + geom_sf(data = boston , col = "black" , aes(fill = dist_CBD)) + 
geom_sf(data = boston_cbd , col = "red" , size = 5)

# variable con el nombre la ciudad
boston$City="Boston"
chicago$City="Chicago"

# Dejar observaciones a 10 millas del centro de chicago
chicago=chicago %>% filter(dist_CBD<=10)

# Apilar datos
st_geometry(chicago)=NULL
st_geometry(boston)=NULL
dta=rbind(chicago,boston)
dta %>% head()

# Plot maps
ggplot(dta, aes(x=dist_CBD, y=estimate, color=City)) +
  geom_point(shape=1) + geom_smooth(method=lm) + xlab("Distance to CBD (miles)") +
  ylab("Median Housing Prices ($)") + theme_bw()







