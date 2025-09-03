#SCRIPT TRES ALGORITMO NTBOX PARA LOS MODELOS DE NICHO ECOLÓGICO----------------------------------------------------------
library(ntbox)
library(raster)
library(sp)
library(purrr)
library(rgl)
library(stringr)
library(tenm)
library(letsR)
library(lattice)
library(latticeExtra)
rm(list = ls())

## ----SE COLOCA LOS DATOS DE LA ESPECIE-------------------------------------------------------------------------------------
library(readr)
occ <- read_csv("E:/Maestria/TRABAJO DE INVESTIGACION 4/DEPURACIONFINAL/Geothlypis_speciosa_invierno.csv")
head(occ[, 1:3])

library(terra)
library(raster)
#Variables bioclimáticas
setwd("E:/Maestria/TRABAJO DE INVESTIGACION 1/CAPAS/wc2.1_30s_bio")
vars <- list.files(pattern = ".tif")
vars <- rast(vars)

occ_vars <- extract(vars, occ[, c("Lon", "Lat")], df = TRUE)
head(occ_vars)

# Para reordenar las columnas
column_order <- c("ID", "bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", 
                  "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")
occ_vars <- occ_vars[, column_order]

# Remover la columna ID y se eliminan NAs
occ_vars <- occ_vars[, -1]
data <- na.omit(occ_vars)
head(data)
#Visualización y definición de elipsoide
environ_dataI <- data
ellipsoid_metadata <- cov_center(environ_dataI, mve = TRUE,
                                 level = 0.99,
                                 vars = c("bio5", "bio6", "bio13"))

rgl::plot3d(rgl::ellipse3d(ellipsoid_metadata$covariance,
                           centre = ellipsoid_metadata$centroid,
                           level = 0.99),
            alpha = 0.4, col = "blue")
in_e <- ntbox::inEllipsoid(ellipsoid_metadata$centroid,
                           ellipsoid_metadata$covariance,
                           level = 0.975,
                           env_data = environ_dataI[, c("bio5", "bio6", "bio13")])
rgl::points3d(environ_dataI[in_e$in_Ellipsoid == 1, c("bio5", "bio6", "bio13")])
rgl::points3d(t(as.matrix(ellipsoid_metadata$centroid)),
              col = "red",
              size = 10)
rgl::rglwidget(width = 500, height = 500)
rgl::close3d()
#PARTICION ALEATORIA DE LOS DATOS PARA CALIBRACIÓN Y EVALUACIÓN Y SU POSTERIOR GUARDADO
library(readr)

# Se cargan los datos
occ <- read_csv("E:/Maestria/TRABAJO DE INVESTIGACION 4/DEPURACIONFINAL/Geothlypis_speciosa_invierno.csv")

# Insertamos una semilla para asegurar que la selección sea reproducible
set.seed(1)

# Se crea el índice para el conjunto de entrenamiento (70% de los datos)
trainGvariav <- sample(nrow(occ), size = ceiling(nrow(occ) * 0.7))

# Separamos en entrenamiento y prueba
dtrainGnv <- occ[trainGvariav, 1:3]
dtestGnv <- occ[-trainGvariav, 1:3]

# Se agrega la columna 'type' con el valor "train" para el conjunto de entrenamiento
dtrainGnv$type <- "train"

# Se agrega la columna 'type' con el valor "test" para el conjunto de prueba
dtestGnv$type <- "test"

# Se guardan los archivos por separados en CSV
write.csv(dtrainGnv, "E:/Maestria/TRABAJO DE INVESTIGACION 4/PARTICION_ALEATORIA/Geothlypis_speciosa_invierno_entrenamiento.csv", row.names = FALSE)
write.csv(dtestGnv, "E:/Maestria/TRABAJO DE INVESTIGACION 4/PARTICION_ALEATORIA/Geothlypis_speciosa_invierno_validacion.csv", row.names = FALSE)

# Si queremos los datos de entrenamiento y validación en un solo archivo 
final_data <- rbind(dtrainGnv, dtestGnv)

# Guardar el archivo final con la columna 'type'
write.csv(final_data, "E:/Maestria/TRABAJO DE INVESTIGACION 4/PARTICION_ALEATORIA/Geothlypis_speciosa_invierno_completo.csv", row.names = FALSE)

# Se muestran los primeros datos
head(final_data)

## ----echo=TRUE, eval=TRUE------------------------------------------------------------------------
pg <- final_data
if (!dir.exists("data_patagona")) dir.create("data_patagona")
#Con esto se cargan los datos de entrenamiento(train) y validación(test).
pgtrain <- pg |> dplyr::filter(type == "train") 
pgtest <- pg |> dplyr::filter(type == "test")
am <- sf::st_read("E:/Maestria/TRABAJO DE INVESTIGACION 4/POLIGONO/Geothlypis_speciosa.shp") |> 
  as("Spatial")
#plot(am) Si queremos observar el área de calibración de la especie.
asur_ext <- terra::ext(am)
#Se cargan las 19 variables para realizar el corte de ellas a la extensión del área de calibración.
bios30 <- "E:/Maestria/TRABAJO DE INVESTIGACION 1/CAPAS/wc2.1_30s_bio"  # Cambia esto por la ruta correcta
# Obtener una lista de todos los archivos .tif en la carpeta
archivos_tif <- list.files(bios30, pattern = "\\.tif$", full.names = TRUE)
# Se cargan todos los archivos .tif en un objeto SpatRaster (stack de capas)
# Se puede plotear cada capa por su nombre 'bio1'.
wc <- rast(archivos_tif)
# Plotear la capa 'bio1' por nombre
plot(wc[["bio1"]])
raster_stack <- rast(archivos_tif)
wc <- rast(archivos_tif)
am <- vect(am) 
# En lugar de usar buffer, utilizamos directamente el polígono `am` para hacer un 'mask' sobre las capas bioclimáticas.
asur_wc_masked <- terra::crop(raster_stack, asur_ext)   # Recortamos el raster a la extensión del polígono.
asur_wc_masked <- terra::mask(asur_wc_masked, am)  # Aplicamos el 'mask' para que solo queden los valores dentro del polígono.
terra::plot(asur_wc_masked[[1]])  # Mostramos la primera capa de las variables bioclimáticas recortadas
points(pg[, c("Lon", "Lat")], pch = 19, cex = 0.5, col = as.factor(pg$type))# Para ver los registros de nuestra especie sobre la capa recordata.
terra::add_legend("bottomright", legend = levels(as.factor(pg$type)), col = c(1, 2), pch = 19)

# Se extrae de valores de los puntos de ocurrencia.
edata <- terra::extract(asur_wc_masked, pg[, c("Lon", "Lat")])

pgenv <- data.frame(pg, edata)
pgenv<-na.omit(pgenv) #POR SI TENEMOS NAS EN LAS VARIABLES RECORTADAS
pgenvL <- base::split(pgenv, pgenv$type)

pg_train <- pgenvL$train
pg_test <- pgenvL$test

## -----------------------------------------------------------------------------------------------
#Con el paquete NTBOX SE PUEDEN OBTENER LAS VARIABLES QUE NO ESTEN CORRELACIONADAS
# Matriz de correlaciones
cor_mat <- cor(pgenv[, names(asur_wc_masked)], method = "spearman")
env_vars <- ntbox::correlation_finder(cor_mat,
                                      threshold = 0.8,
                                      verbose = FALSE)$descriptors
#Me muestra las variables que no estan correlacionadas, sin embargo aqui no se toma en cuenta las variables "prioritarias de nuestra elección y que no estan correlacionadas con las demas variables que ya determinamos anteriormente.
print(env_vars)

# Aquí definimos las variables que nosotros elegimos y que no estan correlacionadas:
env_vars <- c("bio2","bio5","bio6","bio7","bio13","bio14","bio18")

## -----------------------------------------------------------------------------------------------
nvarstest <- c(3, 4, 5, 6, 7)#Número de dimensiones con los que se calibrarán los modelos de elipsoides, ntbox genera modelos con todas las combinaciones lineales de todas las variables. 

## -----------------------------------------------------------------------------------------------
level <- 0.975 # Proporción de puntos dentro del elipsoide.

## -----------------------------------------------------------------------------------------------
omr_criteria <- 0.05#Criterio de omisión para seleccionar lo modelos.

## -----------------------------------------------------------------------------------------------
proc <- TRUE#Correr ROC parcial

## -----------------------------------------------------------------------------------------------
parallel1 <- TRUE # De tipo lógico. Si es TRUE, la selección se correrá en paralelo (Será más rápido el procedimiento de los modelos)

## -----------------------------------------------------------------------------------------------
comp_each <- 100 #Número de modelos que se correrán en cada trabajo (núcleo de la compu).
parallel::detectCores() # SOLO PARA SABER CUÁNTOS NÚCLEOS TIENE LA COMPU
# Crear puntos aleatorios dentro del polígono `am`
set.seed(123)  # Para reproducibilidad
num_puntos_bg <- 10000  # Número de puntos de fondo (background)
puntos_bg <- terra::spatSample(am, size = num_puntos_bg, method = "random")

# Extraer las variables bioclimáticas para esos puntos aleatorios
env_bg <- terra::extract(asur_wc_masked, puntos_bg)
env_bg <- data.frame(puntos_bg, env_bg)
env_bg<-na.omit(env_bg) # POR SI MARCA ERROR ABAJO
#Corriendo la función ellipsoid_selection`
## -----------------------------------------------------------------------------------------------
e_selct <- ntbox::ellipsoid_selection(env_train = pg_train,
                                      env_test = pg_test,
                                      env_vars = env_vars,
                                      level = level,
                                      nvarstest = nvarstest,
                                      env_bg = env_bg,
                                      omr_criteria = 0.06,
                                      parallel = parallel1,
                                      comp_each = comp_each,
                                      proc = parallel1)
#CRITERIOS DE SELECCIÓN, VEREMOS CUALES Y CUANTOS MODELOS PASARON LOS CRITERIOS DE SELECCION (VALOR DE OMISIÓN <0.05 Y ROC PARCIAL >1.0)

# Nos quedamos con los modelos que pasaron las pruebas de selección
e_selct_omr <- e_selct |> dplyr::filter(om_rate_train <= 0.06 & om_rate_test <= 0.06)
#Proyección de los mejores modelos (ver los que pasaron)
bestvarcomb <- stringr::str_split(e_selct_omr$fitted_vars, ",") 
# Convertir las capas de SpatRaster a RasterStack
asur_wc_masked_raster <- raster::stack(asur_wc_masked)
# Ahora, al llamar a ellipsoidfit2(), usa el RasterStack
modelos <- 1:length(bestvarcomb) |> purrr::map(function(x) {
  vars1 <- unlist(bestvarcomb[[x]])
  
  # Convertir las variables seleccionadas en un RasterStack para que sea compatible con ellipsoidfit2
  env_layers <- asur_wc_masked_raster[[vars1]]
  
  # Calcular el centro y la matriz de covarianza
  cov_centroid <- ntbox::cov_center(pg_train[, vars1],
                                    mve = T, level = level,
                                    vars = vars1)
  centroid <- cov_centroid$centroid
  mve_cov <- cov_centroid$covariance
  
  # Ajustar el elipsoide con las variables seleccionadas
  efit <- ntbox::ellipsoidfit2(env_layers,
                               centroid = centroid,
                               covar = mve_cov,
                               level = level, size = 3)
  return(efit)
})
#Con el paso anterior podemos ver las elipsoides de volumen mínimo de cada modelo.
typeof(modelos)

modelos <- raster::stack(modelos)
raster::plot(modelos)
#Binarización de los modelos
#Hay que estimar los umbrales de idoneidad que se utilizarán para binarizar los modelos de nicho. En este caso usara el ten percentil.
## -----------------------------------------------------------------------------------------------
suits_occ <- raster::extract(modelos,pgtrain[,c("Lon","Lat")]) #idoneidades de los puntos de entrenamiento
threshold <- 0.1 # Este es el ten percentil 
umbrales_bin <- lapply(1:ncol(suits_occ),function(x){
  suits_mod <- na.omit(sort(suits_occ[,x]))
  suit_id <- ceiling(length(suits_mod)*threshold)
  return(suits_mod[suit_id]) #suit_id tiene los 10 puntos
}) |> unlist()
#Ahora aplicamos los umbrales para binarizar nuestros mapas
umbrales_bin

## -----------------------------------------------------------------------------------------------

modelos_bin <- modelos > umbrales_bin #algebra de mapas.
#Para visualizar los modelos binarios
raster::plot(modelos_bin)
#Modelos consenso
#Si sumamos los modelos obtenemos el modelo consenso.
## -----------------------------------------------------------------------------------------------
suma_modelo_consenso <- sum(modelos_bin)
plot(suma_modelo_consenso, main="Suma consenso")
points(pg[,c("Lon","Lat")],pch=19,
       cex=0.25,col= as.factor(pg$type))
legend("bottomright",legend = levels(as.factor(pg$type)),
       col = c(1,2),pch=19)
## -----------------------------------------------------------------------------------------------
modelo_conseso <- prod(modelos_bin)
plot(modelo_conseso, main = "Interseccion consenso")
points(pg[,c("Lon","Lat")],pch=19,
       cex=0.25,col= as.factor(pg$type))
legend("bottomright",legend = levels(as.factor(pg$type)),
       col = c(1,2),pch=19)
#Para guardar nuestro modelo final o modelo consenso.
writeRaster(modelo_conseso, "E:/Maestria/TRABAJO DE INVESTIGACION 4/MODELOS CONSENSO/Geothlypis_speciosa_invierno_REP.asc")
#Para visualizar el modelo consenso que ya se guardó.
prueba<- rast("E:/Maestria/TRABAJO DE INVESTIGACION 4/MODELOS CONSENSO/Geothlypis_speciosa_invierno_REP.asc")
plot(prueba)
#Para guardar las diferentes metricas de los modelos seleccionados(roc parcial, valores de omisión)
write.csv(e_selct_omr,"E:/Maestria/TRABAJO DE INVESTIGACION 4/MODELOS CONSENSO/Geothlypis_speciosa_invierno_REP.csv")                                              
#EXTRA
#Para guardar la elipsoide de volumen minimo.
# Se abre la ventana gráfica para guardar la imagen como PNG
png("E:/Maestria/TRABAJO DE INVESTIGACION 4/ELIPSES/speciosainviernoNTBOX.png", width = 800, height = 600)

# Se asegura que el gráfico se dibuja correctamente antes de cerrar el dispositivo gráfico
dev.flush()  # Esta línea es importante para forzar el renderizado del gráfico

# Cerrar el dispositivo gráfico para guardar el archivo
dev.off()
#Para guardar en png
rgl::rgl.snapshot("E:/Maestria/TRABAJO DE INVESTIGACION 4/ELIPSES/speciosainviernoNTBOX.png")

