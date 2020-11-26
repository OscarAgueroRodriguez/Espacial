##### Script trabajo distancias
library(sf)
library(geosphere)
library(tidyverse)
library(raster)
library(sp)
library(spdep)
library(tmap)

# Carga de datos
setwd("~/Estadística/Estadística Espacial/Trabajo Final/Base")
base <- read_sf("prueba.shp")

#selección de la provincia = Harare

Harare <- base %>% filter(Province=="Harare")

# Identificación del distrito con mayor población

harare %>%  group_by(wardpcod_1) %>% 
  summarise(
    empleados = sum(pd_empl__1)
  )


# Calculo de la distancia

Harare$Distance <- as.numeric(st_distance(st_centroid(Harare[41,]) , st_centroid(Harare)))

# Análisis grafico de las principales variables

map1 <- tm_shape(Harare) + tm_fill(col="pd_empl__1", title = "Población Empleada") + tm_borders() + 
  tm_compass(type = "8star", position = c("left", "top"))
map1 + tm_layout(title = "Harare") #+ tm_text("wardpcod_1")

map_2 <- tm_shape(Harare) + tm_fill(col="pop", title = "Población total") + tm_borders() + 
  tm_compass(type = "8star", position = c("left", "top"))
map_2 + tm_layout(title = "Harare")

map_3 <- tm_shape(Harare) + tm_fill(col="years_scho", title = "Educación") + tm_borders() + 
  tm_compass(type = "8star", position = c("left", "top"))
map_3 + tm_layout(title = "Harare")

map_4 <- tm_shape(Harare) + tm_fill(col="Distance", title = "Distancia") + tm_borders() + 
  tm_compass(type = "8star", position = c("left", "top"))
map_4 + tm_layout(title = "Harare")

# Proyección de los datos espaciales

#Proyectar
#"+proj=utm +zone=36 +south +a=6378249.145 +b=6356514.966398753 +units=m +no_defs"

Harare %>% st_transform("+proj=utm +zone=36 +south +a=6378249.145 +b=6356514.966398753 +units=m +no_defs") -> Harare.20936 
Harare.20936_map <- Harare.20936

#Extrac Lat and log from a Multipolygon

#Centrar la proxy de Latitud y Longitud

Harare.20936$geom2 <- st_centroid(st_geometry(Harare.20936))
str(Harare.20936$geom2)

#Crear las variables de Latitud y Longitud

Harare.20936 <- Harare.20936 %>% tidyr::extract(geom2, c('lat', 'log'), '\\((.*), (.*)\\)', convert = TRUE)

#Revisión
print(Harare.20936, n = 2)
st_geometry(Harare.20936) <- "geom2"
plot(st_geometry(Harare.20936))

# Analisis de los modelos
# Modelo OLS
mod <- lm(pd_empl__1 ~ pop + years_scho + Distance + areaKM2
          , data=Harare.20936)
summary(mod)
plot(mod$residuals)

AIC(mod)

#Analisis de correlación Espacial

#Criterio dela Reina
list.reina<-poly2nb(Harare.20936_map, queen=TRUE)
Wr<-nb2listw(list.reina, style="B", zero.policy=T)
moran.lm_wr<-lm.morantest(mod,Wr, alternative="two.sided",zero.policy=T)
print(moran.lm_wr)

#Criterio de la Torre
list.torre<-poly2nb(Harare.20936_map, queen=FALSE)
Wt<-nb2listw(list.torre, style="B", zero.policy=T)
moran.lm_wt<-lm.morantest(mod,Wt, alternative="two.sided",zero.policy=T)
print(moran.lm_wt)

# Se utiliza la torre para los resultados del trabajo


# Modelo SAR
mod_sar <- spautolm(mod$call$formula, data = Harare.20936, listw = Wr)
summary(mod_sar)

# Modelo CAR

mod_car <- spautolm(mod$call$formula, data = Harare.20936, family = "CAR", listw = Wr)
summary(mod_car)

# Modelo GWR
library(spgwr)
GWRbandwidth <- gwr.sel(mod$call$formula, data = Harare.20936,
                        coords=cbind(na.omit(Harare.20936$log),na.omit(Harare.20936$lat)),adapt=T)

mod_gwr <- gwr(mod$call$formula, data = Harare.20936,
               coords=cbind(na.omit(Harare.20936$log),na.omit(Harare.20936$lat)),
               adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE)
mod_gwr$results

results <- as.data.frame(mod_gwr$SDF)
results$gwr.e


#Mapa

Harare.20936_map$educa <- results$years_scho
Harare.20936_map$pop_gwr <- results$pop
Harare.20936_map$distance_gwr <- results$Distance
Harare.20936_map$predich <- results$pred

# Revisión de los resultados con modelo GWR

map_5 <- tm_shape(Harare.20936_map) + tm_fill(col="educa", title = "Resultado Educ.") + tm_borders() + 
  tm_compass(type = "8star", position = c("left", "top"))
map_5 + tm_layout(title = "Harare con GWR")

map_6 <- tm_shape(Harare.20936_map) + tm_fill(col="pop_gwr", title = "Resultado Pobla") + tm_borders() + 
  tm_compass(type = "8star", position = c("left", "top"))
map_6 + tm_layout(title = "Harare con GWR")

map_7 <- tm_shape(Harare.20936_map) + tm_fill(col="distance_gwr", title = "Resultado Dist") + tm_borders() + 
  tm_compass(type = "8star", position = c("left", "top"))
map_7 + tm_layout(title = "Harare con GWR")

map_8 <- tm_shape(Harare.20936_map) + tm_fill(col="predich", title = "Empleo Estimado") + tm_borders() + 
  tm_compass(type = "8star", position = c("left", "top"))
map_8 + tm_layout(title = "Harare con GWR")

# Solo se utiliza el mapa 8 para el trabajo final





