
library(sf)       
library(units)
library(sp)       
library(rJava)    
library(spcosa)    
library(ggplot2)   
library(tmap)

#install.packages('rJavaEnv')
#rJavaEnv::java_quick_install(version = 21)



# Cargar el polígono (AOI) y transformarlo a EPSG:32718
Poligono <- st_read("SHP/Poligono.shp")
Poligono_st <- st_transform(Poligono, 32718)

# Calcular el área del polígono
area_Poligono <- st_area(Poligono_st)
area_Poligono/10000

#devtools::install_github("statnmap/HatchedPolygons")

library(ggpattern)
library(HatchedPolygons)
#hatch <- hatched.SpatialPolygons(Poligono_st, density = 4, angle = 45)
#hatch <- hatch %>% sf::st_set_crs(st_crs(germany))

library(ggspatial)
library(scales)
Mapa = ggplot()+
  geom_sf(data = Poligono_st,
          fill = "#38A700",  
          color = "black",  
          size = 0.8, 
          alpha = 0.7,     
          linetype = "solid")+
  theme(legend.position = "bottom",
        
        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=8),
        axis.title = element_text(face="bold", color="black"),
        
        plot.title = element_text(size = 16, hjust = 0.5, family="serif", face = "italic"),
        plot.subtitle = element_text(size = 11,  face = "italic", family="serif"),
        plot.caption = element_text(size = 9, family="serif", face = "italic"),
        
        plot.background = element_rect(fill = "white", color = NA),
        
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=7, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.3,"cm"), #ancho de cuadrados de referencia
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  labs(x = 'Longitud', y = 'Latitud',
       title="Generacion de puntos de muestreo de suelos",
       subtitle="Concesion con fines no maderables en el departamento  \nde Madre de Dios - Peru, ",
       caption="Gorky Florez  'Datos: Gerencia regional de flora y fauna silvestre ")+
  scale_x_continuous(labels = number_format(accuracy = 0.01)) +  # Dos decimales en el eje x
  scale_y_continuous(labels = number_format(accuracy = 0.01))    # Dos decimales en el eje y

ggsave(plot=Mapa ,"Mapa1.png",units = "cm",width = 15, #alto
       height = 15, #ancho
       dpi=1200)

# Convierte el polígono a sp para utilizarlo con spcosa
poly_ws_sp <- as(Poligono_st, "Spatial")


# Dividir el polígono en estratos
ws_strata <- stratify(poly_ws_sp, nStrata = 25, nTry = 10)


# Plot the strata
plot(ws_strata) +
  scale_x_continuous(name = "Easting (km)") +
  scale_y_continuous(name = "Northing (km)") +
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  labs(title = "25 estratos compactos")+
  theme(legend.position = "bottom",
        
        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=8),
        axis.title = element_text(face="bold", color="black"),
        
        plot.title = element_text(size = 16, hjust = 0.5, family="serif", face = "italic"),
        plot.subtitle = element_text(size = 11,  face = "italic", family="serif"),
        plot.caption = element_text(size = 9, family="serif", face = "italic"),
        
        plot.background = element_rect(fill = "white", color = NA),
        
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=7, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.3,"cm"), #ancho de cuadrados de referencia
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))

set.seed(123)

# La funcion spsample nos ayuda a generar los puntos centricos
ws_sample1 <- spsample(ws_strata)

plot(ws_strata, ws_sample1) +
  scale_x_continuous(name = "Easting (km)") +
  scale_y_continuous(name = "Northing (km)") +
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  labs(title = "25 estratos compactos, muestra en los centroides")+
  theme(legend.position = "bottom",
        
        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=8),
        axis.title = element_text(face="bold", color="black"),
        
        plot.title = element_text(size = 16, hjust = 0.5, family="serif", face = "italic"),
        plot.subtitle = element_text(size = 11,  face = "italic", family="serif"),
        plot.caption = element_text(size = 9, family="serif", face = "italic"),
        
        plot.background = element_rect(fill = "white", color = NA),
        
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=7, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.3,"cm"), #ancho de cuadrados de referencia
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))


## Control de la discretización

ws_strata_fine <- stratify(poly_ws_sp, nStrata = 25, nTry = 10, nGridCells = 19000)

ws_sample_fine <- spsample(ws_strata_fine)

plot(ws_strata_fine, ws_sample_fine) +
  scale_x_continuous(name = "Easting (km)") +
  scale_y_continuous(name = "Northing (km)") +
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  labs(title = "25 estratos compactos, discretización fina")+
  theme(legend.position = "bottom",
        
        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=8),
        axis.title = element_text(face="bold", color="black"),
        
        plot.title = element_text(size = 16, hjust = 0.5, family="serif", face = "italic"),
        plot.subtitle = element_text(size = 11,  face = "italic", family="serif"),
        plot.caption = element_text(size = 9, family="serif", face = "italic"),
        
        plot.background = element_rect(fill = "white", color = NA),
        
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=7, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.3,"cm"), #ancho de cuadrados de referencia
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))


## Muestreo aleatorio estratificado

ws_sample_stratified <- spsample(ws_strata_fine, n = 3)

plot(ws_strata_fine, ws_sample_stratified) +
  scale_x_continuous(name = "Easting (km)") +
  scale_y_continuous(name = "Northing (km)") +
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  labs(title = "25 estratos compactos, 3 puntos aleatorios por estrato")+
  theme(legend.position = "bottom",
        
        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=8),
        axis.title = element_text(face="bold", color="black"),
        
        plot.title = element_text(size = 16, hjust = 0.5, family="serif", face = "italic"),
        plot.subtitle = element_text(size = 11,  face = "italic", family="serif"),
        plot.caption = element_text(size = 9, family="serif", face = "italic"),
        
        plot.background = element_rect(fill = "white", color = NA),
        
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=7, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.3,"cm"), #ancho de cuadrados de referencia
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))


new_points <- spsample(ws_strata_fine)
## Exportación de los puntos muestreados

infill.points.df <- as(new_points, "data.frame")

# convertir a sf para cambiar CRS
infill.pts.sf <- st_as_sf(infill.points.df, coords = c("x1", "x2"))

st_crs(infill.pts.sf) <- st_crs(poly_ws_sp)

infill.pts.sf <- st_transform(infill.pts.sf, 4326)

# volver a convertir a data.frame para exportar
infill.pts.df <- as.data.frame(st_coordinates(infill.pts.sf))
names(infill.pts.df) <- c("Long", "Lat")
infill.pts.df
library(openxlsx) 
write.xlsx(infill.pts.df, "Puntos de muestreo.xlsx")



# Load required packages
library(terra)
library(gdistance)
library(elevatr)
library(sf)

# Defina su área de interés (AOI)
pol_sf  <- st_read("SHP/Poligono.shp")
pol_sf  <- st_transform(pol_sf ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
# Descargar los datos del DEM
dem <- get_elev_raster(pol_sf, z = 14, src = "aws", clip = "locations")

writeRaster(dem, "Raster/dem_Poli.tif", overwrite = TRUE)

# Crear un objeto raster con el paquete terra
dem <- rast("Raster/dem_Poli.tif")
plot(dem)
# Calcular la pendiente
slope <- terrain(dem, v = "slope", unit = "radians") 
plot(slope)
# Calcular el aspecto
aspect <- terrain(dem, v = "aspect", unit = "radians") 
plot(aspect)
# Calcular el hill
elev = get_elev_raster(pol_sf, z=14)
Poligo_alt    <- crop(elev, pol_sf)                           #   
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, pol_sf)
slopee    = terrain(Poligo_alt  , opt = "slope") 
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)
plot(hille)
hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)
ggplot()+
  geom_sf(data = pol_sf, fill=NA, color="white", size=0.01)+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  theme_void()

# Calcular el TPI (Índice de Posición Topográfica)
tpi <- terrain(dem, v = "TPI")
plot(tpi)
# Calculate TRI (Terrain Ruggeedness Index)
tri <- terrain(dem, v = "TRI")
plot(tri)
# Save the derived layers
writeRaster(slope, filename = "Raster/slope_Poli.tif", overwrite = TRUE)
writeRaster(aspect, filename = "Raster/aspect_Poli.tif", overwrite = TRUE)
writeRaster(tpi, filename = "Raster/tpi_Poli.tif", overwrite = TRUE)
writeRaster(tri, filename = "Raster/tri_Poli.tif", overwrite = TRUE)

# Calcular el coste acumulado capa superficial

library(raster)

dem_r <- raster("Raster/dem_Poli.tif")
# Transformar a coordenadas en grados
dem_deg <- projectRaster(dem_r, crs = CRS("+proj=longlat +datum=WGS84 +no_defs"))

# Create a transition layer based on the cost raster
cost_transition <- transition(dem_deg, transitionFunction = mean, directions = 8)

# Corrige la capa de transición para calcular las distancias con precisión
cost_transition <- geoCorrection(cost_transition)

# Definir las coordenadas del punto inicial
start_point <- c(x = -69.17391451956608, y = -12.251650788227854)

# Superficie de coste acumulado
accumulated_cost <- accCost(cost_transition, start_point)
plot(accumulated_cost)
# Export raster
writeRaster(accumulated_cost, "Raster/cost_Poli.tif", overwrite = TRUE)


library(terra)
library(sf)
library(sp)
library(clhs)
library(raster)
library(tidyverse)
library(tmap)

# Cargar cada raster individualmente
raster1 <- raster("Raster/aspect_Poli.tif")
raster2 <- raster("Raster/cost_Poli.tif")
raster3 <- raster("Raster/dem_Poli.tif")
raster4 <- raster("Raster/slope_Poli.tif")
raster5 <- raster("Raster/tpi_Poli.tif")
raster6 <- raster("Raster/tri_Poli.tif")

# Ajustar resolución y extensión a raster1
raster2 <- resample(raster2, raster1)
raster3 <- resample(raster3, raster1)
raster4 <- resample(raster4, raster1)
raster5 <- resample(raster5, raster1)
raster6 <- resample(raster6, raster1)

# Apilar los rásters
r.stack <- stack(raster1, raster2, raster3, raster4, raster5, raster6)


# Crear una cuadrícula regular de puntos en la parte superior de nuestra pila de trama
library(terra)

# Convertir r.stack a SpatRaster
r.stack_terra <- rast(r.stack)

set.seed(5)
s <- spatSample(r.stack_terra, size= 1000, method = "regular", xy = TRUE)

# Filtrar los puntos con valores ausentes en el atributo "coste".
s <- s[!is.na(s$cost), ]

# Crear un marco de datos de puntos espaciales
s_spdf <- SpatialPointsDataFrame(coords = s[, 1:2], data = s[, 3:ncol(s)])

# La superficie de coste se activa mediante el argumento "coste
s.clhs <- clhs(s_spdf, size = 25, progress = FALSE, iter = 2000, cost = 'cost_Poli', simple = FALSE)

# Extraer los índices
subset.idx <- s.clhs$index_samples

# Visualice las capas DEM y de costes, con los puntos seleccionados marcados
terra::plot(r.stack$dem_Poli, axes=TRUE, legend=TRUE)
terra::contour(r.stack$cost_Poli, nlevels=10, col='#464343', add=TRUE)
points(s[subset.idx, ], col = '#FF00C8', pch=16, cex = 1.5)


# Convierte el raster en un data frame
dem_df <- as.data.frame(r.stack$dem_Poli, xy = TRUE, na.rm = TRUE)
cost_df <- as.data.frame(r.stack$cost_Poli, xy = TRUE, na.rm = TRUE)
points_df <- as.data.frame(s[subset.idx, ])

library(ggnewscale)
# Grafica con ggplot2
colores<- c("#015B26", "#3F8433", "#8BAB56", "#CFCF7D", "#C49450", "#AA5B32", "#872921")

Mapa=ggplot() +
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_tile(data = dem_df, aes(x = x, y = y, fill = dem_Poli), alpha=0.6) +
  scale_fill_gradientn(colours = colores ,
                       
                       na.value = 'white',
                       name='Elevacion \n(msnm)')+
  # Añade las curvas de nivel del segundo raster
  geom_contour(data = cost_df, aes(x = x, y = y, z = cost_Poli),
               color = 'black', bins = 10) +
  
  # Añade los puntos
  geom_point(data = points_df, aes(x = x, y = y),
             color = 'black', size = 2) +
  geom_sf(data = pol_sf, fill=NA, color="white", size=0.01)+
  # Personaliza el tema
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, hjust = 0.5, family="serif", face = "italic"),
        plot.subtitle = element_text(size = 11,  face = "italic", family="serif"),
        plot.caption = element_text(size = 9, family="serif", face = "italic"))+
  labs(x = "Longitud", y = "Latitud") +
  coord_equal()+
  guides(fill = guide_legend( 
    direction = 'horizontal',
    keyheight = unit(5, units = "mm"),
    keywidth = unit(15, units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = .5,
    nrow = 1,
    byrow = T,
    reverse = F,
    label.position = "bottom"
  ))+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    height = unit(0.7, "cm"),
    width = unit(1.1, "cm"),
    pad_x = unit(0.1, "cm"), pad_y = unit(0.8, "cm"),
    style=north_arrow_fancy_orienteering())+
  ggspatial::annotation_scale(
    location = "bl",
    bar_cols = c("black", "white"),
    text_family = "ArcherPro Book",
    text_col = "black"
  ) +
  coord_sf() +  # Reemplaza `coord_equal()` con `coord_sf()`
  labs(x = 'Longitud', y = 'Latitud',
       title="Generacion de puntos de muestreo de suelos por condicional",
       subtitle="Muestreo de suelo por condiciones de DEM, Pendiente, Aspecto, Superficie de coste acumulado \nÍndice de Posición Topográfica y Índice de rugosidad del terreno  ",
       caption="Ing. Gorky Florez  'Datos: Gerencia regional de flora y fauna silvestre ")

ggsave(plot=Mapa ,"Mapa2.png",units = "cm",width = 19, #alto
       height = 15, #ancho
       dpi=1200)

# Guardar los puntos seleccionados como CSV
write.xlsx(s[subset.idx, ], "Puntos de muestreo2.xlsx")



### Similarity Buffer analysis ###

# Convert terra rasters to data.frame
df_raster <- as.data.frame(r.stack, xy = TRUE)

# Convert data frame to SpatialPointsDataFrame
coordinates(df_raster) <- c("x", "y")

# Load the raster layer
rast_layer <- raster("Raster/dem_poli.tif")

# Create a raster template
r <- raster(extent(rast_layer), res = 0.0002777778, crs = crs(rast_layer)) 

# Create a raster stack using a loop
r.stack <- stack() # Initialize an empty raster stack

# Recorrer cada variable del SpatialPointsDataFrame
for (i in 1:ncol(df_raster@data)) {
  # Rasterize each variable
  layer <- rasterize(df_raster, r, df_raster@data[, i], fun = mean)  # Use mean for aggregation
  names(layer) <- names(df_raster@data)[i]
  r.stack <- stack(r.stack, layer)
}

# Calcular el índice de similitud de Gower entre la pila de trama y los datos muestreados del análisis CLHS, con un búfer de 250.
gw_cuenca <- similarity_buffer(r.stack, s.clhs$sampled_data, buffer = 500)

# Guarda los resultados del buffer como un GeoTIFF llamado 'list_buffer.tif' en el directorio 'Raster'.
terra::writeRaster(gw_cuenca, "Raster/grids_buffer.tif")

plot(gw_cuenca)





