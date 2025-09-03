# SCRIP 1 Selección de las especies de estudio, con las columnas de interes, eliminación de datos duplicados y guardado de las especies por archivo.

#Instalacion del paquete necesario
install.packages("dplyr")
#Abrir el paquete o la libreria
library(dplyr)
# Para obtener la cita de la libreria
citation("dplyr")
# Se cargó el archivo de la base descargada de GBIF y se delimitó
parulidae_records <- read.delim("C:/Users/Roberto Mtz/Downloads/0038283-241126133413365/0038283-241126133413365.csv")
#Para ver las columnas
colnames(parulidae_records)
# Se seleccionan las columnas de interés y se renombraron algunas columnas

selected_datos <- parulidae_records %>%
  select(species, decimalLongitude, decimalLatitude, year, genus, month, day, locality) %>%  # Selección de columnas
  rename(Lon = decimalLongitude, Lat = decimalLatitude, mes = month) %>%  # Renombrar columnas
  filter(!is.na(Lon) & !is.na(Lat) & !is.na(species) & species != "") %>%  # Filtrar filas sin coordenadas y sin especie
  filter(!is.na(year) & !is.na(mes))  # Filtrar filas sin año o mes
#Se quitaron los datos duplicados
sin_duplicados <- distinct(selected_datos)
# Se guardaron los datos en un archivo CSV, esto se hizo para cada base
write.csv(sin_duplicados, "E:/Maestria/TRABAJO DE INVESTIGACION 3/Nueva carpeta (2)/Datos para trabajar/Datossinduplicados1970-1979.csv", row.names = FALSE)
#Para ver que especies hay en la base
parulidae1970_1979 <- read.csv("E:/Maestria/TRABAJO DE INVESTIGACION 3/Nueva carpeta (2)/Datos1970-1979.csv")
colnames(sin_duplicados)
# Muestra los nombres de las filas de la columna 'species'
print(sin_duplicados$species)
# Muestra los nombres únicos de las especies en la columna 'species'
unique_species <- unique(sin_duplicados$species)
print(unique_species)
# Crea una lista con los nombres únicos de las especies
unique_species_list <- as.list(unique(sin_duplicados$species))

# Imprimir la lista
print(unique_species_list)
# Solo nombres únicos de las especies
unique_species <- unique(sin_duplicados$species)

# data.frame con los nombres únicos
unique_species_df <- data.frame(Species = unique_species, stringsAsFactors = FALSE)
#Se guardo en un csv
output_file <- "E:/Maestria/TRABAJO DE INVESTIGACION 3/Nueva carpeta (2)/Datos para trabajar/LISTA DE species1970-1979.csv"
# Se guardó el data.frame en un archivo CSV
write.csv(unique_species_df, file = output_file, row.names = FALSE)
# Cargar el archivo CSV
#ESTE PASO ANTERIOR SE REALIZO CON LOS 7 BASES DESCARGADAS ORIGINALMENTE DE GBIF
#1970-1979
#1980-1991
#1992-2008
#2009-2014
#2015-2019
#2020-2022
#2023-2024 # total 7 BASES
#A CADA UNA DE LAS BASES GUARDADAS SE SELECCIONARON LAS ESPECIES QUE SE DISTRIBUYEN EN LA ZONA DE ESTUDIO,DE IGUAL MANERA SE REALIZÓ LA DEPURACIÓN/ VERIFICACION TAXONÓMICA 
parulidae1970_1979 <- read.csv("E:/Maestria/TRABAJO DE INVESTIGACION 3/Nueva carpeta (2)/Datos1970-1979.csv")

# Se definió en un vector las especies que se quieren conservar de la columna "species", 
species_to_keep <- c(
  "Basileuterus belli", "Basileuterus culicivorus", "Basileuterus lachrymosus", "Basileuterus rufifrons",
  "Cardellina pusilla", "Cardellina rubra", "Cardellina rubrifrons", "Cardellina versicolor",
  "Geothlypis formosa", "Geothlypis nelsoni", "Geothlypis philadelphia", "Geothlypis poliocephala",
  "Geothlypis speciosa", "Geothlypis tolmiei", "Geothlypis trichas",
  "Helmitheros vermivorum",
  "Leiothlypis celata", "Leiothlypis crissalis", "Leiothlypis luciae", "Leiothlypis peregrina",
  "Leiothlypis ruficapilla", "Leiothlypis virginiae",
  "Mniotilta varia",
  "Myioborus miniatus", "Myioborus pictus",
  "Oreothlypis superciliosa",
  "Parkesia motacilla", "Parkesia noveboracensis",
  "Setophaga americana", "Setophaga citrina", "Setophaga coronata", "Setophaga dominica",
  "Setophaga graciae", "Setophaga magnolia", "Setophaga nigrescens", "Setophaga occidentalis",
  "Setophaga pensylvanica", "Setophaga petechia", "Setophaga pitiayumi",
  "Setophaga ruticilla", "Setophaga townsendi", "Setophaga virens",
  "Setophaga aestiva" , "Setophaga auduboni"
)
#NOTA LAS ULTIMAS DOS, SON SUBESPECIES, SE AÑADIRAN A LA BASE DE S. PETECHIA Y S. CORONATA RESPECTIVAMENTE
# Se le realizó el filtrado al dataframe
filtrado_especies <- parulidae1970_1979[parulidae1970_1979$species %in% species_to_keep, ]

#SE GUARDO UN NUEVO ARCHIVO CSV CON LAS ESPECIES DE ESTUDIO
# Guardar el dataframe filtrado en un archivo CSV
write.csv(filtrado_especies, "E:/Maestria/TRABAJO DE INVESTIGACION 3/Nueva carpeta (2)/Datos para trabajar/Especiesdeestudio1970-1979.csv", row.names = FALSE)

#ESTO SE REALIZO CON LAS 7 BASES.
#POSTERIORMENTE SE CARGARON LAS BASES QUE CONTIENEN SOLO A LAS ESPECIES DE ESTUDIO

setwd("E:/Maestria/TRABAJO DE INVESTIGACION 3/Nueva carpeta (2)/Datos para trabajar")
list.files()
elprimero <- read.csv("Especiesdeestudio1980-1991.csv")
elsegundo <- read.csv("Especiesdeestudio1992-2008.csv")
eltercero <- read.csv("Especiesdeestudio2009-2014.csv")
elcuarto <- read.csv("Especiesdeestudio2015-2019.csv")
elquinto <- read.csv("Especiesdeestudio2020-2022.csv")
elsexto <- read.csv("Especiesdeestudio2023-2024.csv")
elseptimo <- read.csv("Especiesdeestudio1970-1979.csv")

# Se le asigno los nombres de las columnas del primero al segundo
colnames(elprimero) <- colnames(elsegundo)
#posteriormente los demas
# verificacion de que el número de columnas es el mismo
if (ncol(elprimero) != ncol(elsegundo) != ncol(eltercero) != ncol(elcuarto) != ncol(elquinto) != ncol(elsexto) != ncol(elseptimo)) {
  stop("El segundo archivo CSV tiene un número diferente de columnas.")
}
# Verificar que el número de columnas es el mismo
if (ncol(elprimero) != ncol(elsegundo) || 
    ncol(elprimero) != ncol(eltercero) || 
    ncol(elprimero) != ncol(elcuarto) || 
    ncol(elprimero) != ncol(elquinto) || 
    ncol(elprimero) != ncol(elsexto) || 
    ncol(elprimero) != ncol(elseptimo)) {
  stop("Los archivos CSV tienen un número diferente de columnas.")
}
# Se combinaron los dataframes
combined_df <- rbind(elprimero, elsegundo, eltercero, elcuarto, elquinto, elsexto, elseptimo)
colnames(combined_df)

# Se guardó el dataframe combinado
output_file <- "E:/Maestria/TRABAJO DE INVESTIGACION 3/Nueva carpeta (2)/ESPECIES_TOTALES.csv"

# Guardar el dataframe combinado en un archivo CSV
write.csv(combined_df, file = output_file, row.names = FALSE)
# Se contaron los datos por especie
species_count <- table(combined_df$species)

# Convertir el conteo a un dataframe
species_count_df <- as.data.frame(species_count)
colnames(species_count_df) <- c("species", "count")

# Guardar el dataframe en un archivo CSV
write.csv(species_count_df, "conteo_por_especie.csv", row.names = FALSE)
setwd("E:/Maestria/TRABAJO DE INVESTIGACION 3/Por especies")
# Se obtuvó la lista de especies únicas
species_list <- unique(combined_df$species)
#Se creó un archivo csv para cada especie
for (species in species_list) {
  # Filtrar los datos para la especie actual
  species_data <- combined_df[combined_df$species == species, ]
  
  # Limpiar el nombre de la especie para el nombre del archivo
  # Reemplazar caracteres no válidos por guiones bajos
  clean_species_name <- gsub("[^[:alnum:]_]", "_", species)
  
  # Crear el nombre del archivo para la especie
  filename <- paste0(clean_species_name, ".csv")
  
  # Se guardó un csv con los datos de cada especie
  write.csv(species_data, filename, row.names = FALSE)
}

RStudio.Version()
citation()



