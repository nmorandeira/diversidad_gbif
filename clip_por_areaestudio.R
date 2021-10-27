
library(sf)
library(readr)

#leer shapefile del area de estudio - se indica el shp
study_area <- st_read(dsn = "data/study_area/Delta_BajoParana_EPSG5347.shp", quiet=TRUE)

#chequear coordenadas
st_crs(study_area)

#luego, leer archivo CSV de gbif y generar un archivo espacial
aves_ceibas <- read_delim("data/dataset_ceibas_2021.csv", "\t", escape_double = FALSE, trim_ws = TRUE)
aves_ceibas_espacial <- st_as_sf(aves_ceibas, coords = c("decimalLongitude","decimalLatitude"), crs = 4326)

#chequear coordenadas
st_crs(aves_ceibas_espacial)

#las coordenadas deben ser coincidentes a las de study area, si no lo son avisame y vemos 

#clip - recorte por el area de estudio
aves_studyarea <- st_intersects(x=aves_ceibas_espacial, y=study_area)

#para grabarlo como tabla
write_csv(aves_studyarea, "output/aves_studyarea.csv")
