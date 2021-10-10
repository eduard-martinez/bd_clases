#==========================================#
# Elaborado por: Eduard F Martinez-Gonzalez
# Ultima modificacion: 24-08-2021
# R version 4.1.1 (2021-08-10)
#==========================================#

# initial configuration
rm(list=ls())
require(pacman) # llamar pacman
p_load(tidyverse,sf,sp,osmdata,leaflet,spdep,png,grid,gstat,ggsn,gtools,spatialreg) # llamar y/o instalar librerias

# spdep: poly2nb, nb2listw
# gstat: variogram
# png, grid: leer graficos
# gtools: quantcut
# spatialreg: lagsarlm 

# Material de Ignacio
browseURL("https://ignaciomsarmiento.github.io/2017/02/07/An-Introduction-to-Spatial-Econometrics-in-R.html")

#=================#
# [1.] Motivacion #
#=================#

# ramdon vs correlation
dev.off()
grid.raster(readPNG("clase_4/pics/motivation.png"))

# semivariogram
dev.off()
grid.raster(readPNG("clase_4/pics/veriograma.png"))

# moran plot
dev.off()
grid.raster(readPNG("clase_4/pics/moran_plot.png"))

#=======================#
# [2.] Pesos Espaciales #
#=======================#

# (Precios de la vivienda en Boston y Chicago (ver clase 2)
load("clase_4/input/data.Rdata")

# generar variable aleatoria
set.seed(21042021)
boston$normal = rnorm(nrow(boston),500,110)
boston$estimate = boston$estimate/1000

# sf to sp 
class(boston)
boston_sp = boston %>% 
            mutate(estimate=ifelse(is.na(estimate),mean(estimate,na.rm=T),estimate)) %>% # 501,543
            as_Spatial()
class(boston_sp)

# vecinos espaciales
dev.off()
grid.raster( readPNG("clase_4/pics/contiguity_matrixes.png"))

# definir vecinos
nb_boston = poly2nb(pl=boston_sp , queen=T) # opcion reina
nb_boston
class(nb_boston)
plot(boston_sp, border="grey60")
plot(nb_boston, coordinates(boston_sp) ,  pch=19, cex=0.5 , add=T)

#  queen vs rook
nb_q = poly2nb(pl=boston_sp, queen=T) # Vecinos reina
nb_r = poly2nb(pl=boston_sp, queen=F) # Vecinos torre
isTRUE(all.equal(nb_q, nb_r, check.attributes=F)) # verificar que no son iguales los objetos
plot(boston_sp, border="grey60")
plot(diffnb(nb_q, nb_r, verbose=FALSE), coordinates(boston_sp), add=TRUE, pch=".", cex=0.6, lwd=2,col="red")
plot(nb_r, coordinates(boston_sp), add=TRUE, pch=19, cex=0.6)

# construir matriz de pesos
cat("El estilo B asigna a todos los vecino un peso de 1
     El estilo W le asigna a cada vecino el peso de 1/sum(n_vecinos)
     El estilo C le asigna a todos los elementos el peso de sum(Number of regions)/sum(Number of nonzero links)
     El estilo U le asigna a todos los elementos el peso de 1/sum(Number of nonzero links)")
w_boston = nb2listw(nb_boston, style = "W", zero.policy = T)
w_boston

# Veamos los vecinos y los pesos del polygon 1
w_boston$weights[[1]]
w_boston$neighbours[[1]]
dev.off()
grid.raster( readPNG("clase_4/pics/contiguity_example.png"))
plot(boston_sp,boreder="black")
plot(boston_sp[1,],col="red",add=T)
plot(boston_sp[w_boston$neighbours[[1]],],col="yellow",add=T)

#==================#
# [3.] Variogramas #
#==================#

# plot map
boston$estimate_q = quantcut(boston$estimate,na.rm=T)
boston$normal_q = quantcut(boston$normal,na.rm=T)

ggplot() + geom_sf(data=boston , col="black" , aes(fill=estimate_q)) + 
scale_fill_manual(values=c("yellow","#FF9900","#FF6600","#CC0000","#990000"),
                  na.value="white", name="Median Housing \nValues") + 
theme_bw() + north(data = boston) + ggtitle("Map A")
ggsave("clase_4/output/map_A.pdf")

ggplot() + geom_sf(data=boston , col="black" , aes(fill=normal_q)) + 
scale_fill_manual(values=c("yellow","#FF9900","#FF6600","#CC0000","#990000"),
                  na.value="white", name="Ramdon var.") + 
theme_bw() + north(data = boston) + ggtitle("Map B")
ggsave("clase_4/output/map_b.pdf")

# variograms
dev.off()
grid.raster(readPNG("clase_4/pics/vario_equation.png")) # Aplied Spatial Data in R
# h = distancia
# Z(s) = Variable aleatorio para la ubicacion s
variogram(estimate ~ 1, boston_sp, cloud = F , cressie=T) %>% plot()

cat("En el caso de observaciones atípicas, los valores extremos de nubes de variogramas se identifican 
    fácilmente para encontrar los valores atípicos. Es posible que sea necesario eliminarlos o, 
    de lo contrario, se pueden calcular medidas sólidas para el variograma de muestra pasando el 
    argumento lógico cressie = TRUE a la llamada a la función del variograma (Cressie, 1993).")
db_plot = left_join(x = variogram(estimate ~ 1, boston_sp, cloud = F , cressie=T) %>% mutate(estimate=gamma) %>% .[,c("dist","estimate")],
                    y = variogram(normal ~ 1, boston_sp, cloud = F , cressie=T) %>% mutate(normal=gamma) %>% .[,c("dist","normal")],"dist") 
db_plot
ggplot(db_plot) + 
geom_point(aes(x=dist, y=normal , fill="Datos aleatorios (Dist. Normal)"), shape=21, alpha=0.5, size=5 ) +
geom_point(aes(x=dist, y=estimate , fill="Median Housing Value"), shape=21, alpha=0.5, size=5 ) +
labs(caption = "Fuente: Tidycensus", y = "Semivariograma", x = "Distancia de separación entre bloques", fill = "") + theme_bw()
ggsave("clase_4/output/veriograma.png", height=5 , width=8)

#===============================#
# [4.] Autocorrelacion espacial #
#===============================#

# Indice de Moran Global
dev.off()
grid.raster(readPNG("clase_4/pics/Imoran_exp.png"))
# n : numero de regiones
# Wij : medida de proximidad espacial entre la región i y j

# moran.plot
plot_moran = moran.plot(x = boston_sp$estimate , listw=w_boston) %>%
             cbind(.,boston_sp@data) %>% mutate(labels=GEOID)

# Plot
mp = moran.plot(x = boston_sp$estimate , listw=w_boston)
ggplot(data = mp , aes(x=x, y=wx)) + geom_smooth(method="lm",col="black",size=0.3) +
geom_point(shape=1 , size=3 ) + 
geom_hline(yintercept=mean(mp$wx), lty=2 , col="red") + geom_vline(xintercept=mean(mp$x), lty=2 , col="red") +
geom_point(data=subset(mp,is_inf==T),aes(x=x, y=wx), shape=9 , col="blue") +
geom_text(data=subset(mp,is_inf==T) , aes(x=x, y=wx , label=labels , vjust=1.5) , size=2.5) + 
labs(y = "Median Housing Values (vecinos)", x = "Median Housing Values", col = "") +
theme_bw() + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +  coord_equal()
ggsave("clase_4/output/moran_plot.png")

# test-moran
cat("La hipotesis nula es que los valores se distribuyen aleatoriamente atravez de los polygonos")
moran.test(boston_sp$estimate, listw = w_boston)

#====================================#
# [5.] Amenitis y precio de vivienda #
#====================================#

# get amenity data
bar = opq(bbox = getbb("Boston USA")) %>% add_osm_feature(key = "amenity", value = "bar") %>% 
      osmdata_sf() %>% .$osm_points %>% select(osm_id) %>% mutate(amenity="bar")
restaurant = opq(bbox = getbb("Boston USA")) %>% add_osm_feature(key = "amenity", value = "restaurant") %>% 
             osmdata_sf() %>%  .$osm_points %>% select(osm_id)  %>% mutate(amenity="restaurant")
pub = opq(bbox = getbb("Boston USA")) %>% add_osm_feature(key = "amenity", value = "pub") %>% 
      osmdata_sf() %>%  .$osm_points %>% select(osm_id)  %>% mutate(amenity="pub")
cafe = opq(bbox = getbb("Boston USA")) %>% add_osm_feature(key = "amenity", value = "cafe") %>% 
       osmdata_sf() %>%  .$osm_points %>% select(osm_id)  %>% mutate(amenity="cafe")
school = opq(bbox = getbb("Boston USA")) %>% add_osm_feature(key = "amenity", value = "school") %>% 
         osmdata_sf() %>%  .$osm_points %>% select(osm_id)  %>% mutate(amenity="school")
amenity = rbind(bar,pub) %>% rbind(.,restaurant) %>% rbind(.,cafe) %>% rbind(.,school)

# plot
ggplot() + geom_sf(data = boston , fill=NA , col="black") + geom_sf(data=amenity,col="red")

# intersect shapes
amenity = st_transform(amenity,crs = st_crs(boston))
amenity2 = st_intersection(amenity,boston)
amenity = amenity[boston,]

# plot
ggplot() + geom_sf(data = boston , fill=NA , col="black") + geom_sf(data=amenity,col="red")

# joint layers
amenity_boston = st_join(x = amenity , y = boston)
st_geometry(amenity_boston) = NULL
df = amenity_boston %>% mutate(conteo = 1) %>% group_by(GEOID,amenity) %>% 
     summarize(total = sum(conteo)) 
df = df %>% pivot_wider(names_from = amenity, values_from = total) 
df = lapply(df, function(x) x = ifelse(is.na(x),0,x)) %>% data.frame()

# estimaciones
boston = left_join(boston,df,"GEOID")
lm(estimate ~ bar + cafe + pub + restaurant + school , data = boston) %>% summary()

#===================#
# [6.] MORAN y LISA #
#===================#

# estimar la correlacion espacial entre dos variables
source("clase_4/scr/moran.R")
boston_sp@data = left_join(boston_sp@data,df,"GEOID")

# Preparar sp
nb_boston = poly2nb(pl=boston_sp , queen=T) # Definir vecinos
w_boston = nb2listw(nb_boston, style = "B", zero.policy = T) # Matrix de pesos
W  = as(w_boston, "symmetricMatrix")
W  = as.matrix(W/rowSums(W))
W[which(is.na(W))] = 0

# variables a usar en correlacion
x = boston_sp@data[,"bar"] # outcome
y = boston_sp@data[,"restaurant"] # outcome lag

# reemplazar los NA de las variables
x[is.na(x)] = 0 
y[is.na(y)] = 0 

# Estimar el indice de moran global, local y las simulaciones del indice
moran = moran_I(x, y, W)
moran_global = moran[[1]]
moran_local = moran[[2]]  # local values
simulaciones_locales = simula_moran(x, y, W)$local_sims

# Construir significacia de las estimaciones locales
probabilidades = c(0.025, 1-0.025) # 5% a dos colas
intervalos = t( apply(simulaciones_locales, 1, function(x) quantile(x, probs=probabilidades)))
significancia = (moran_local < intervalos[,1] )  | ( moran_local > intervalos[,2] )

# Identificar clusters LISA
xp = (x-mean(x))/sd(x)
yp = (y-mean(y))/sd(y)
patterns = as.character(interaction(xp > 0, W%*%yp > 0) ) 
patterns = patterns %>% gsub("TRUE","Alto",.) %>%  gsub("FALSE","Bajo",.)

# Agregar informacion al mapa
boston_sp$significancia = significancia
boston_sp$lisa = patterns

# Agregar estimaciones al mapa 
boston_sf = boston_sp %>% st_as_sf() %>%
            mutate(lisa = gsub("\\.","-",lisa) , lisa = ifelse(significancia==T,lisa,"Sin relación") %>% as.character())  

# plot map
ggplot() + geom_sf(data=boston_sf , aes(fill=lisa) , col="black" , alpha=1 , size=0.05) + 
scale_fill_manual(values = c("#FF0033", "#FF0099", "#33CCFF", "#0033FF", "white")) +
scale_y_continuous(expand=c(0,0)) + scale_x_continuous(expand=c(0,0)) +
labs(y = "", x = "", fill = "Patrón") + theme_light() + north(data=boston_sf , location="topleft") + 
scalebar(data=boston_sf, dist=5 , transform=T , dist_unit="km" , location="bottomleft")
ggsave("clase_4/output/LISA_.pdf")

