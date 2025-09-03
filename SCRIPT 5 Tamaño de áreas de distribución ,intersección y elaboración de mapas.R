#SCRIPT DE TAMAÑOS DE ÁREAS DE DISTRIBUCIÓN, INTERSECCION Y ELABORACIÓN DE MAPAS.
library(terra)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(ggspatial)
library(cowplot)
library(terra)
rm(list = ls())
# Se añaden los modelos de invierno y verano
rI <- rast("E:/Maestria/TRABAJO DE INVESTIGACION 4/MODELOS CONSENSO/Leiothlypis_virginiae_invierno_REP.asc")  # Ruta correcta
rV <- rast("E:/Maestria/TRABAJO DE INVESTIGACION 4/MODELOS CONSENSO/Leiothlypis_virginiae_verano_REP.asc")  # Ruta correcta

# Se verifican que ambos rasters tienen el mismo tamaño y resolución
if (!identical(res(rI), res(rV))) {
  stop("Los rasters no tienen la misma resolución.")
}

# Se crea una capa combinada con los valores especificados para cada tipo de coincidencia
r_combinado <- app(c(rI, rV), fun = function(x) {
  if (any(is.na(x))) {
    return(NA)  # No considerar píxeles con NA
  }
  
  invierno <- x[1]
  verano <- x[2]
  
  # Combinación de los resultados con los valores respectivos
  if (invierno == 1 && verano == 1) {
    return(1)  # Azul para la coincidencia en ambos
  } else if (invierno == 1 && verano == 0) {
    return(2)  # Color de invierno
  } else if (invierno == 0 && verano == 1) {
    return(3)  # Color de verano
  } else {
    return(4)  # Gris claro para ninguno
  }
})

# Definición de los colores 
color_invierno <- "#43AC8E"  # Color similar a la elipse de invierno
color_verano <- "#D66D1C"    # Color similar a la elipse de verano
color_union <- "#4682B4"     # Azul para la intersección

# Se le asignan los colores a los valores de la capa combinada
colores <- c(color_union, color_invierno, color_verano, "lightgray")
names(colores) <- c(1, 2, 3, 4)

# Visualización de la capa combinada
plot(r_combinado, col = colores, legend = FALSE)

# Cálculo del porcentaje de área compartida
cantidad_pixeles_1V <- sum(rV[] == 1, na.rm = TRUE)
cantidad_pixeles_1I <- sum(rI[] == 1, na.rm = TRUE)

# Para contar los píxeles donde ambos son 1
cantidad_pixeles_compartidos <- sum((rV[] == 1) & (rI[] == 1) & !is.na(rV[]) & !is.na(rI[]), na.rm = TRUE)

# Cálculo del porcentaje de área compartida
total_pixeles <- length(rV[])
porcentaje_compartido <- (cantidad_pixeles_compartidos / total_pixeles) * 100

# Porcentaje de área compartida
cat("Porcentaje de área compartida: ", round(porcentaje_compartido, 2), "%\n")
#ESTE PORCENTAJE ESTA CONSIDERANDO LOS NAS QUE TIENE EL MODELO.
# Área por píxel en km²
res_x <- 0.008333333  # Resolución en grados
res_y <- 0.008333333  # Resolución en grados
area_pixel_km2 <- (res_x * res_y) * (111^2)
#Solo tomaremos en cuenta los valores de uno y cero, los NAS no. 
# Para calcular la superficie total en km² para las áreas donde ambos modelos son 1
area_compartida_km2 <- cantidad_pixeles_compartidos * area_pixel_km2

# Área compartida en km² considerando solo los valores de 1
cat("Área compartida en km²: ", round(area_compartida_km2, 2), "km²\n")
sum(is.na(rV[]))  # Cantidad de píxeles con NA en verano
sum(is.na(rI[]))  # Cantidad de píxeles con NA en invierno
# Se van a contar solo los píxeles donde hay presencia en al menos una capa
total_presencia <- sum((rV[] == 1) | (rI[] == 1), na.rm = TRUE)

# Se calcula el porcentaje de coincidencia solo en esas áreas (1)
porcentaje_compartido <- (cantidad_pixeles_compartidos / total_presencia) * 100

# Porcentaje compartido tomando en cuenta los valores donde hay 1 en ambos modelos
cat("Porcentaje de área compartida considerando solo presencia: ", round(porcentaje_compartido, 2), "%\n")
# Se calcula el número de píxeles con valor 1 en cada modelo
pixeles_invierno <- sum(rI[] == 1, na.rm = TRUE)
pixeles_verano <- sum(rV[] == 1, na.rm = TRUE)

# Se calcula el área por píxel en km²
res_x <- 0.008333333  # Resolución en grados
res_y <- 0.008333333  # Resolución en grados
area_pixel_km2 <- (res_x * res_y) * (111^2)

# Calculo del área total en km² para invierno y verano
area_invierno_km2 <- pixeles_invierno * area_pixel_km2
area_verano_km2 <- pixeles_verano * area_pixel_km2

# Se muestran los resultados
cat("Área en km² en invierno: ", round(area_invierno_km2, 2), "km²\n")
cat("Área en km² en verano: ", round(area_verano_km2, 2), "km²\n")
# Número de píxeles con valor 1 en cada raster
pixeles_invierno <- sum(rI[] == 1, na.rm = TRUE)
pixeles_verano <- sum(rV[] == 1, na.rm = TRUE)

# Resultados
cat("Cantidad de píxeles con valor 1 en invierno:", pixeles_invierno, "\n")
cat("Cantidad de píxeles con valor 1 en verano:", pixeles_verano, "\n")
#PIXELES COMPARTIDOS
cantidad_pixeles_compartidos <- sum((rV[] == 1) & (rI[] == 1) & !is.na(rV[]) & !is.na(rI[]), na.rm = TRUE)
# Cantidad de píxeles compartidos con valor 1 en ambas temporadas
cat("Cantidad de píxeles compartidos con valor 1 en ambas temporadas:", cantidad_pixeles_compartidos, "\n")
# Calculo del área compartida en km² (solo donde ambos tienen valor 1)
area_compartida_exacta_km2 <- cantidad_pixeles_compartidos * area_pixel_km2

# Área compartida exacta en km²
cat("Área exacta compartida en km² (solo donde ambos tienen valor 1): ", round(area_compartida_exacta_km2, 2), "km²\n")
#MAPA-----------------------------------------------------------------------------------------------------------------------------
# Se convierte el raster a data frame para ggplot
r_df <- as.data.frame(r_combinado, xy = TRUE)
names(r_df)[3] <- "Distribucion"
r_df <- na.omit(r_df)

# Definición de colores
colores <- c("#4682B4", "#43AC8E", "#D66D1C", "lightgray")
labels <- c("Intersección", "Invierno", "Verano", "Área de calibración")

# Se carga el mapa de América
world <- ne_countries(scale = "medium", returnclass = "sf")
america <- world[world$continent %in% c("North America", "South America"), ]

# Esta función nos permite calcular automáticamente los límites por país
obtener_limites <- function(paises) {
  region <- world[world$admin %in% paises, ]
  return(st_bbox(region))
}

# Obtención de los límites para los paises deseados:
ubicacion_bbox <- obtener_limites(c("Mexico","Canada","Guatemala"))

# El mapa principal esta basado en la extensión del modelo
raster_extent <- ext(r_combinado)

# Mapa principal con ggplot
mapa_principal <- ggplot() +
  geom_raster(data = r_df, aes(x = x, y = y, fill = as.factor(Distribucion))) +
  scale_fill_manual(values = colores, labels = labels, name = "Distribución") +
  geom_sf(data = america, fill = NA, color = "black", size = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  coord_sf(xlim = c(raster_extent[1], raster_extent[2]), ylim = c(raster_extent[3], raster_extent[4])) +
  theme_minimal() +
  theme(axis.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1)) +
  ggtitle(expression(italic("Leiothlypis virginiae")))

# Mapa de ubicación con el modelo y las líneas de los países visibles
mapa_ubicacion <- ggplot() +
  geom_sf(data = world, fill = "white", color = "black", size = 0.3) +
  geom_raster(data = r_df, aes(x = x, y = y, fill = as.factor(Distribucion)), alpha = 0.6) +
  scale_fill_manual(values = colores, guide = "none") +
  geom_sf(data = america, fill = NA, color = "black", size = 0.3) +  # Volver a dibujar líneas de países
  coord_sf(xlim = c(ubicacion_bbox[1], ubicacion_bbox[3]), ylim = c(ubicacion_bbox[2], ubicacion_bbox[4])) +
  theme_void() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

# Se combinan ambos mapas
final_map <- plot_grid(mapa_principal, mapa_ubicacion, ncol = 2, rel_widths = c(3, 1))
#PLOTEO DEL MAPA
print(final_map)
#FIN DEL PROCEDIMIENTO---------------------------------------------------------------------------------------------------------