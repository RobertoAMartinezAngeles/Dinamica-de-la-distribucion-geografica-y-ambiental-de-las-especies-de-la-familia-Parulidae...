#SCRIPT DOS PROCESAMIENTO DE LOS DATOS BIOLÓGICOS.

## Se cargaron los paquetes a utilizar---------------------------
library(ntbox)
library(dplyr)
library(raster)
library(sf)
library(rio)
library(geodata)
library(terra)

#Se añadió un vector shp de América de hydrosheds
am <- sf::st_read("E:/Maestria/TRABAJO DE INVESTIGACION 1/CAPAS/AMERICAA.shp") |> 
  as("Spatial")
plot(am)
am <- vect(am)
plot(am)
#Se añadieron las variables bioclimáticas
bios30 <- "E:/Maestria/TRABAJO DE INVESTIGACION 1/CAPAS/wc2.1_30s_bio"  # Cambia esto por la ruta correcta
# Se obtiene una lista de todos los archivos .tif en la carpeta
archivos_tif <- list.files(bios30, pattern = "\\.tif$", full.names = TRUE)
# Todos los archivos .tif se cargaron en un objeto SpatRaster (stack de capas)
# Se puede plotera la capa 'bio1' 
raster_stack <- rast(archivos_tif)
wc <- rast(archivos_tif)
plot(wc[["bio1"]])
#Con la función crop y luego mask se cortaron las variables bioclimáticas a la extension de América. 
am30 <- terra::crop(raster_stack, am)   
am30 <- terra::mask(am30, am)

library(dplyr)
raster::plot(am30[["bio1"]])
especie <- read.csv("E:/Maestria/TRABAJO DE INVESTIGACION 3/Por especies/Basileuterus_belli.csv")

# Se verifican cuántas filas tiene el archivo
num_filas <- nrow(especie)
set.seed(1)
# Archivo CSV de los registros de ocurrencia de la especie
especie <- read.csv("E:/Maestria/TRABAJO DE INVESTIGACION 3/Por especies/Basileuterus_belli.csv")
#Para las especies que tenian mas de 100,000 datos de ocurrencia se seleccionaron aleatoriamente 100,000 datos y se inicio el proceso de la depuracion de datos.

if (num_filas >= 100000) {
  especie_muestra <- especie[sample(1:num_filas, 100000), ]
} else {
  cat("El archivo tiene menos de 100,000 filas. Se seleccionarán todas las filas.")
  especie_muestra <- especie
}

head(especie_muestra)
especie <-especie_muestra
#Si las especies tenian menos de 100,000 se inició el procedimiento de la depuración.
# Archivo CSV de los registros de ocurrencia de la especie
especie <- read.csv("E:/Maestria/TRABAJO DE INVESTIGACION 3/Por especies/Basileuterus_belli.csv")

## Eliminación de datos duplicados geográficos---------------------------------------------------------
sinduplicados <- ntbox::clean_dup(especie, longitude =  "Lon","Lat", threshold = 0)
cat("Numero total de datos de presencia despues de la limpieza de duplicados espaciales:",
    nrow(sinduplicados))
## Eliminación de los duplicados ambientales ---------------------------------------------------------
# Primero, se extrajo la informacion ambiental de los datos de presencia.

dAll_e30 <- raster::extract(am30,sinduplicados[,c("decimalLongitude", "decimalLatitude")])
dAll_ge30 <- data.frame(sinduplicados[,c("Scientific", "Lon",
                                         "Lat",
                                         "year","genus","mes","day")],
                        dAll_e30)

# Posteriormente, se eliminaron los datos duplicados.
dAll_ge30c <- ntbox::clean_dup(dAll_ge30,
                               longitude ="Lon",
                               latitude = "Lat",
                               threshold = res(am30)[1])

# Eliminación de NAs
dAll_ge30c <- unique(na.omit(dAll_ge30c))
#Con la siguiente tabla se puede visualizar cuantos datos se han eliminado.
tabla1 <- data.frame(Resolucion = c(30), 
                     Num_Registros_GBIF = c(nrow(especie)),
                     Sin_duplicadosGeo = c(nrow(dAll_ge30)),
                     Sin_duplicadosAmb = c(nrow(dAll_ge30c)))

# Asignación correcta de los nombres a las columnas
names(tabla1) <- c("Resolucion", "Registros GBIF", "Sin duplicados Geo", "Sin duplicados Amb")

# Se muestra la tabla
knitr::kable(tabla1)
#DEPURACION DE DATOS ATIPICOS CON UN CLUSTER DENDOGRAMA
# Se seleccionan solo las columnas bio_01 a bio_19 para el análisis
datos_clustering <- dAll_ge30c[, c("bio1", "bio2", "bio3", "bio4", "bio5", 
                                   "bio6", "bio7", "bio8", "bio9", "bio10",
                                   "bio11", "bio12", "bio13", "bio14", "bio15",
                                   "bio16", "bio17", "bio18", "bio19")]

# Se escalan los datos
datasd <- scale(datos_clustering)

# Se calcula la matriz de distancias euclideanas
d <- dist(datasd, method = "euclidean")

# Realización del clúster jerárquico
clust <- hclust(d, method = "average")
# Visualización el dendrograma
plot(clust, cex = 0.5)  
# Se cortó el dendrograma en el nivel 6, ya que se parte con la idea de que los datos restantes tienen una similitud superior al 75% (Height = 6)
num_clust <- cutree(clust, h = 6)  # Esto te da el número de clústeres para esa altura

# Se agrega una columna del numero de clústeres al dataframe
dAll_ge30c$cluster <- num_clust
# Ahora se filtra el dataframe para conservar solo los datos en los clústeres hasta Height = 6
# Se conservaran todos los datos hasta el número máximo de clústeres obtenido en esa altura.
clusters_seleccionados <- unique(num_clust)  # Esto da todos los números de clústeres que existen hasta Height = 6

# Filtración del dataframe
B_belli_filtrado <- subset(dAll_ge30c, cluster %in% clusters_seleccionados)

# Verificación el resultado
print(table(B_belli_filtrado$cluster))  # Esto te muestra cuántos datos hay en cada clúster
# Filtrar el dataframe para conservar solo los clústeres 1, 2, 3, 4, 5 y 6
clusters_seleccionados <- c(1, 2, 3, 5, 6)

# Filtrar el dataframe
B_belli_filtrado <- subset(dAll_ge30c, cluster %in% clusters_seleccionados)
# Se verifica el resultado para asegurarse que queden solo los clústeres seleccionados.
print(table(B_belli_filtrado$cluster))  
#Se guardó el dataframe filtrado en un nuevo archivo CSV:
#write.csv(B_belli_filtrado, "E:/Maestria/TRABAJO DE INVESTIGACION 3/Por especies_sin_atipicos_cluster/Basileuterus_belli_sin_atipicos_cluster.csv")

# Para obtener la visualización y verificar la reducción de atipicos se cargaron los datos anteriores y se repite el procedimiento anterior del cluster.
datos_clusterfiltrado <- B_belli_filtrado[, c("bio1", "bio2", "bio3", "bio4", "bio5", 
                                              "bio6", "bio7", "bio8", "bio9", "bio10",
                                              "bio11", "bio12", "bio13", "bio14", "bio15",
                                              "bio16", "bio17", "bio18", "bio19")]

datasd2 <- scale(datos_clusterfiltrado)


d2 <- dist(datasd2, method = "euclidean")


clust <- hclust(d2, method = "average")

# Visualizamos el dendrograma y notamos que ya no contiene a los datos atípicos y que se ha reducido el número de datos.
plot(clust, cex = 0.5) 
#Dvision por temporadas

colnames(B_belli_filtrado)

# Filtración de los datos para invierno (meses 1, 2, 3, 10, 11, 12)
invierno <- subset(B_belli_filtrado, mes %in% c(1, 2, 3, 10, 11, 12))

# Filtración de los datos para verano (meses 4, 5, 6, 7, 8, 9)
verano <- subset(B_belli_filtrado, mes %in% c(4, 5, 6, 7, 8, 9))

# Se guardaron los archivos filtrados
write.csv(invierno,"E:/Maestria/TRABAJO DE INVESTIGACION 3/Por especies_sin_atipicos_Cluster/POR TEMPORADAS/Basileuterus _belli_invierno.csv", row.names = FALSE)
write.csv(verano,"E:/Maestria/TRABAJO DE INVESTIGACION 3/Por especies_sin_atipicos_Cluster/POR TEMPORADAS/Basileuterus _belli_verano.csv", row.names = FALSE)
#El procedimiento de limpieza de los datos autocorrelacionados se relaizo por temporada.
library(spThin)
data <- read.csv("E:/Maestria/TRABAJO DE INVESTIGACION 3/Por especies_sin_atipicos_Cluster/POR TEMPORADAS/Basileuterus _belli_verano.csv")
head(data)

chipe1km <-
  thin( loc.data = data, 
        lat.col = "Lat", long.col = "Lon", 
        spec.col = "species",
        thin.par = 1, # numero en km para el buffer de nearest neighbor distance (1 km)
        reps = 1, # replica solo es una
        locs.thinned.list.return = TRUE, 
        write.files = TRUE, 
        max.files = 1, # numero de archivos filtrados
        out.dir = "E:/Maestria/TRABAJO DE INVESTIGACION 3/Por especies_con_spThin_Cluster", out.base = "Basileuterus _belli_verano", # directorio donde se guardó
        write.log.file = TRUE,
        log.file = "chipe1km.txt" )
# Se abrio el archivo sin autocorrelación espacial
filtered_data <- read.csv("E:/Maestria/TRABAJO DE INVESTIGACION 3/Por especies_con_spThin_Cluster/Basileuterus _belli_verano_thin1.csv")
#Con la siguiente tabla podemos ver el proceso de reducción de los datos a causa de la limpieza realizada.
filtered_data$Lat <- as.numeric(filtered_data$Lat)
filtered_data$Lon <- as.numeric(filtered_data$Lon)
tabla1 <- data.frame(Resolucion = c(30), 
                     Num_Registros_GBIF = c(nrow(especie)),
                     Sin_duplicadosGeo = c(nrow(dAll_ge30)),
                     Sin_duplicadosAmb = c(nrow(dAll_ge30c)),
                     Sin_atipicos = c(nrow(datos_clusterfiltrado)),
                     Invierno = c(nrow(invierno)),
                     Verano = c(nrow(verano)),
                     SpthinTemp = c(nrow(filtered_data)))

# Asignación correcta de los nombres a las columnas
names(tabla1) <- c("Resolucion", "Registros GBIF", "Sin duplicados Geo", "Sin duplicados Amb", "Sin atipicos", "Invierno", "Verano", "SpthinTemp")
# Se muestra la tabla
knitr::kable(tabla1)
#FIN DEL PROCESO DE LA DEPURACIÓN DE LOS DATOS BIOLÓGICOS
