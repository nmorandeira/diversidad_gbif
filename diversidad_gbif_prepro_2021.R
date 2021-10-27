####### Comunidades de aves de Ceibas, provincia de Entre Rios #############
####### Análisis de diversidad a partir de registros de GBIF ##############

####### Código escrito por Natalia Morandeira, nmorandeira@unsam.edu.ar ####
###### Colaboradora: Sofía Larrandart, slarrandart@gmail.com 
####### Septiembre-Octubre de 2020, actualizado en mayo de 2021. Parte del código fue destinado a la asignatura Ecología, ECyT-3iA, Universidad Nacional de San Martín

# En el ejemplo usamos una base de datos de eBird obtenida a través de la web. El dataset de este ejercicio fue descargado por Sofía Larrandat y tiene una licencia CC 1.0: https://www.gbif.org/occurrence/download/0272796-200613084148143 
# Cita del dataset: GBIF.org (10 May 2021) GBIF Occurrence Download https://doi.org/10.15468/dl.txypjs


#### PRIMERA PARTE - Pre-procesamiento de la base de datos descargada de Gbif ##########

### Primero, cargamos los datos tal cual los bajamos de gbif. 
# Lo podemos hacer en RStudio desde "Import Dataset", indicando que el separador es tabulador, o lo podemos hacer así:

library(readr) #nombre de la libreria necesaria
aves_ceibas <- read_delim("data/dataset_ceibas_2021.csv", "\t", escape_double = FALSE, trim_ws = TRUE)
View(aves_ceibas) #abrir la tabla (o "dataframe"). Vemos un montón de columnas que no nos interesan.

############
### Extra, se puede saltear: visualización de la geolocalización de los registros en un mapa #########################3
library(sf)
library(tmap)
## transformamos "aves_ceibas" en un objeto espacial, indicando cuáles son las coordenadas de longitud y de latitud, para luego poder realizar un mapa
aves_ceibas_espacial <- st_as_sf(aves_ceibas, coords = c("decimalLongitude","decimalLatitude"), crs = 4326)

#para mapear
#tmap_mode("view") #activa el modo interactivo, con un mapa base de fondo. Se puede hacer click en los puntos para ver más detalles del registro
#consume mucha memoria, lo comento. en todo caso se pueden mapear menos registros

#qtm(aves_ceibas_espacial)
################################

##### Ahora sí vamos a ordenar/emprolijar la tabla y pre-procesarla para hacer análisis #########
#listamos las columnas 
colnames(aves_ceibas)

#Para este ejercicio sólo nos interesan: 
#"order" orden de la especie. Es la columna 7
#"family" familia de la especie. Es la columna 8
#"species" nombre de la especie. Es la columna 10
#"locality" nombre del sitio registrado en inaturalist. columna 17
#"ocurrenceStatus", si está presente. Es la columna 19
#"individualCount", si hay datos de abundancia: cuántos individuos se vieron. Es la columna 20. 
#"eventDate", es el día de la observación. Es la columna 30.
#Filtramos: aves_ceibas es igual a aves_ceibas [filas,columnas]. Entonces: [,c(10, 19, 20)] significa TODAS las filas (porque antes de la coma no dice nada) y las columnas 10, 19 y 20.
aves_ceibas <- aves_ceibas[,c(7, 8, 10, 17, 19, 20, 30)]

#conteo de registros por localidad
library(tidyverse)
aves_ceibas %>% 
  group_by(locality) %>%
  summarise(count=n()) %>% 
  arrange(desc(count))
  

#Para ver cómo es aves_ceibas
View(aves_ceibas)
#Para hacer una previsualización rápida de las primeras filas:
head(aves_ceibas)
#Podemos notar que las dos primeras columnas son de tipo texto (character), la tercera es numérica (double) y la última es de tipo fecha-hora (dttm).
#Para hacer un resumen rápido. 
summary(aves_ceibas)
#Si miramos las estadísticas de la columna individualCount, vemos que hay miles de registros con valor "NA", esto significa "Not Available" (No disponible), es decir que la celda está vacía.

#agregar columna de localidad
library(tidyverse)
aves_ceibas<- mutate(aves_ceibas, zona = case_when(
  locality == "Ceibas--Terraplén (AICA ER07)" ~ "Terraplen",
  locality == "Ceibas--Ea. Campo Bajo (AICA ER07)" ~ "Humedal",
  locality == "Shell - Ceibas" ~ "Ruta"))

aves_ceibas<- mutate(aves_ceibas, zona = case_when(
  locality == "Ceibas--humedales (AICA ER07)" ~ "Reserva",
  locality == "Ceibas--camino Arroyo Ñancay (AICA ER07)" ~ "CaminoNancay",
  locality == "Shell - Ceibas" ~ "Ruta"))

aves_ceibas <- subset(aves_ceibas, aves_ceibas$zona != "NA")

###########################
### Pasos a repetir #########
#A partir de ahora, generamos sub-listados por zonas #############
aves_ceibas_all <- aves_ceibas
aves_ceibas_terraplen <- subset(aves_ceibas_all, aves_ceibas_all$zona == "Terraplen")
aves_ceibas_bajo <- subset(aves_ceibas_all, aves_ceibas_all$zona == "Humedal")
aves_ceibas_ruta <- subset(aves_ceibas_all, aves_ceibas_all$zona == "Ruta")

aves_ceibas_all <- aves_ceibas
aves_ceibas_reserva <- subset(aves_ceibas_all, aves_ceibas_all$zona == "Reserva")
aves_ceibas_caminonancay <- subset(aves_ceibas_all, aves_ceibas_all$zona == "CaminoNancay")
aves_ceibas_ruta <- subset(aves_ceibas_all, aves_ceibas_all$zona == "Ruta")

#Los siguientes procesos hay que repetirlos en cada zona
aves_ceibas <- aves_ceibas_terraplen
aves_ceibas <- aves_ceibas_bajo
aves_ceibas <- aves_ceibas_ruta

aves_ceibas <- aves_ceibas_reserva
aves_ceibas <- aves_ceibas_caminonancay
aves_ceibas <- aves_ceibas_ruta


#Vamos a generar una columna de abundancia: cuando no hay datos en "individualCount", suponemos que hay 1 individuo (criterio más conservativo).
#Primero chequeamos qué valores posibles tiene "ocurrenceStatus"
levels(as.factor(aves_ceibas$occurrenceStatus))
#Si el único valor posible es "PRESENT", adelante, no hay problema. Generamos una nueva columna idéntica a individualCount, llamada "abundancia" y reemplazamos todos los NA por 1. Para usar la función "replace_na" debemos cargar una nueva librería. 
library(tidyverse)
aves_ceibas$abundancia <- aves_ceibas$individualCount
aves_ceibas$abundancia <- replace_na(data = aves_ceibas$abundancia, replace=1)

summary(aves_ceibas$abundancia) #resumen de la nueva columna

#Cuántos registros hay? Vemos que el largo de la columna species es de miles de registros. Pero esto no significa que haya miles de especies, sólo que algunas de las especies se observaron en más de un día e incluso más de una vez por día, por varias personas. Por ejemplo, hagamos un recorte de la base de datos ("subset", significa sub-conjunto) con solo los registros del hornero (Furnarius rufus).

hornero <- subset(aves_ceibas, species=="Furnarius rufus")
View(hornero)
summary(hornero) #podemos ver cuántos registros hay y también qué cantidad de individuos se contaron.
hist(hornero$abundancia, breaks=100) #un ejemplo de histograma de abundancias de horneros en cada registro

### Vamos a guardar un listado de especies ###########
colnames(aves_ceibas)
listado_spp<-aves_ceibas[,1:3]
listado_spp$spp <- listado_spp$species

listado_spp <- separate(listado_spp, spp, into = c("genero", "spp"), sep = " ")

#creo un código de 3 letras para el género y 3 letras para las spp, para facilitar los análisis posteriores
listado_spp$Codigo <- paste(toupper(substr(listado_spp$genero, start = 1, stop = 3)),sep="-", toupper(substr(listado_spp$spp, start = 1, stop = 3)))

aves_ceibas$Codigo <- listado_spp$Codigo 

# Generar el listado final, quitando las especies duplicadas
listado_spp  <- arrange(listado_spp , order, family, species) 

listado_spp <- listado_spp[!duplicated(listado_spp$species),]
listado_spp <- listado_spp[,c(1:3,6)]

#le cambio el nombre de columnas a castellano
colnames(listado_spp) <- c("Orden", "Familia", "Especie", "Codigo")

#cargo capas de nombres comunes
nombres_comunes <- read_csv("data/nombres_comunes.csv")

library(tidyverse)
listado_spp <- left_join(listado_spp, nombres_comunes, by= "Especie")

#guardo el listado a disco
write_csv(listado_spp, "output/listado_spp_terraplen.csv")
write_csv(listado_spp, "output/listado_spp_bajo.csv")
write_csv(listado_spp, "output/listado_spp_ruta.csv")
write_csv(listado_spp, "output/listado_spp_reserva.csv")
write_csv(listado_spp, "output/listado_spp_caminonancay.csv")


#Para cada especie y día, nos gustaría tener un único valor de abundancia. Si dos observadores/as registraron a la misma especie en el mismo día, ¿será que miraban individuos distintos?, ¿o las personas estaban cerca y registraron a los mismos individuos? Las abundancias de cada observador: se suman?, se promedian?, se toma el valor máximo? 
# Como criterio conservador, tomamos el MAXIMO diario. (En verdad: habría que analizar la distribución espacial de las observaciones de cada día y definir si son los mismos individuos o se trata de observaciones independientes).

#El siguiente código es un poco más complicado, pero lo que dice es que aves_max_diario es una nueva tabla de datos, que se crea a partir de aves_ceibas, agrupando por dos columnas (species y eventDate): en esos grupos le pedimos que calcule el máximo diario y lo grabe en una nueva columna llamada "maximo_diario".

aves_max_diario <- aves_ceibas %>% 
  group_by(species, eventDate) %>%
  summarize(maximo_diario = max(abundancia)) 

View(aves_max_diario)

#mirando la tabla me doy cuenta de que tiene varios NA en "species" (probablemente aves no identificadas). Aprovecho para eliminar esas filas.

aves_max_diario <- na.omit(aves_max_diario)

summary(aves_max_diario)

#genero una columna de anio que me sirve para después
aves_max_diario$anio <- as.numeric(format(aves_max_diario$eventDate, format="%Y"))



##########################
#### SEGUNDA PARTE - Definicion de objetivos: qué comparar ############

# En el marco del TP buscamos analizar si hay cambios en la diversidad en distintos momentos y cuáles son las comunidades de aves más disímiles.

# Ahora tenemos la tabla "aves_max_diario"bastante prolija! Tenemos que definir qué períodos nos interesa comparar. Por ejemplo: 
# a) comparar entre distintas estaciones del año.
# b) comparar entre distintos años (hay registros desde 1988, aunque más abundantes desde 2011: el primer cuantil de las fechas es el 2 de abril de 2011), 
# - más avanzado 1: comparar entre períodos secos y húmedos (necesitaríamos tener datos climáticos para el período 1988-2019).
# - más avanzado 2: comparar entre zonas dentro del área de estudio, que podrían a su vez asociarse a distintos hábitats o a distintos usos del suelo (tendríamos que haber guardado las coordenadas de latitud y longitud de las observaciones, que estaban en el archivo original y luego saber trabajar con datos espaciales en R o en un software SIG específico).

# Les sugiero que elijan qué análisis hacer en cada grupo: (a) o (b).

##### (a) Separar en estaciones del año #####
##### A efectos de este ejercicio, separaremos en trimestres: 
# 1 Octubre-diciembre (aprox primavera)
# 2 Enero-marzo (aprox verano)
# 3 Abril-junio (aprox otoño)
# 4 Julio-septiembre (aprox invierno)
#Empezamos en primavera porque llegan las especies migratorias que se quedan en el siguiente verano.

library(lubridate) #vamos a usar la libreria lubridate y la funcion quarter que divide en trimestres
aves_max_diario$estaciones <- quarter(aves_max_diario$eventDate, with_year = FALSE, fiscal_start = 10)


## me quedo con el último lustro

aves_max_diario <- mutate(aves_max_diario, lustro = case_when(
  between(anio, 2005, 2009) ~ "Periodo2005-2009",
  between(anio, 2010, 2014) ~ "Periodo2010-2014",
  between(anio, 2015, 2019) ~ "Periodo2015-2019"))

aves_max_diario_2015_19 <- subset(aves_max_diario, aves_max_diario$lustro == "Periodo2015-2019")

# aves_estaciones <- aves_max_diario %>% 
#   group_by(species, estaciones) %>%
#   summarize(abund_max_estacion = max(maximo_diario))  #aca elegi calular el máximo de aves en el trimestre, pero podría ser la media (mean) o la suma (sum) 

aves_estacion1 <- aves_max_diario_2015_19 %>% 
  group_by(species, estaciones) %>%
  summarize(abund_sum_estacion = sum(maximo_diario))  #aca elegi calular la SUMA de aves en el trimestre

View(aves_estacion1)
#Notar la cantidad de filas y de variables (columnas).

#Para hacer los análisis de diversidad, necesitamos tener una columna para cada estacion. Es decir: pasar de una tabla "larga" a una tabla más "ancha" en donde haya una columna para cada estacion.

# aves_estacion <- pivot_wider(aves_estaciones, names_from = estaciones, values_from = abund_max_estacion)
aves_estacion <- pivot_wider(aves_estacion1, names_from = estaciones, values_from = abund_sum_estacion, names_sort = TRUE)

#finalmente le cambiamos los nombres a las columnas
colnames(aves_estacion) <- c("Especie", "Primavera", "Verano", "Otonio", "Invierno")

#y voy a agregar nombres comunes
aves_estacion <- left_join(aves_estacion, listado_spp, by= "Especie") #le uno el código a la tabla de estaciones

#re-ordenar columnas
colnames(aves_estacion )
aves_estacion <- aves_estacion %>% relocate (c(Codigo, nombrecomun, Familia, Orden), .after= Especie)

View(aves_estacion)


# Guardamos la tabla a disco en un archivo csv (apto para Excel)
write.csv(aves_estacion, file = "output/2021/aves_terraplen_estaciones.csv" )
write.csv(aves_estacion, file = "output/2021/aves_bajo_estaciones.csv" )
write.csv(aves_estacion, file = "output/2021/aves_ruta_estaciones.csv" )
write.csv(aves_estacion, file = "output/2021/aves_reserva_estaciones.csv" )
write.csv(aves_estacion, file = "output/2021/aves_caminonancay_estaciones.csv" )

###############3
##### Generar tablas comparando las tres zonas #########
library(readr)

aves_terraplen <- read_delim("output/2021/aves_terraplen_estaciones.csv", ",", escape_double = FALSE, trim_ws = TRUE)
aves_bajo <- read_delim("output/2021/aves_bajo_estaciones.csv", ",", escape_double = FALSE, trim_ws = TRUE)
aves_ruta <- read_delim("output/2021/aves_ruta_estaciones.csv", ",", escape_double = FALSE, trim_ws = TRUE)

aves_reserva <- read_delim("output/2021/aves_reserva_estaciones.csv", ",", escape_double = FALSE, trim_ws = TRUE)
aves_camino <- read_delim("output/2021/aves_caminonancay_estaciones.csv", ",", escape_double = FALSE, trim_ws = TRUE)
aves_ruta <- read_delim("output/2021/aves_ruta_estaciones.csv", ",", escape_double = FALSE, trim_ws = TRUE)

library(tidyverse)
aves_reserva <- aves_reserva %>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  mutate(Reserva_Prim = Primavera) %>% 
  mutate(Reserva_Inv = Invierno) 

aves_camino <- aves_camino %>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  mutate(Camino_Prim = Primavera) %>% 
  mutate(Camino_Inv = Invierno) %>% 
  select(c(Especie, Camino_Prim, Camino_Inv))

aves_ruta <- aves_ruta %>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  mutate(Ruta_Prim = Primavera + Verano) %>% 
  mutate(Ruta_Inv = Otonio + Invierno) %>% 
  select(c(Especie, Ruta_Prim, Ruta_Inv))

aves_all <- full_join(x=aves_reserva, y=aves_camino, by= "Especie")
aves_all <- full_join(x=aves_all, y=aves_ruta, by= "Especie")
aves_all$X1 = NULL

aves_all <- aves_all  %>% 
  mutate_if(is.numeric, ~replace_na(., 0))

aves_all <- aves_all[order(aves_all$Especie),]

# a. primavera-verano
aves_zonas_Prim <- aves_all[, c(1:5, 10, 12, 14)]

aves_zonas_Prim <- aves_zonas_Prim %>%
    mutate(borrar = Reserva_Prim + Camino_Prim + Ruta_Prim) %>% 
    filter(borrar > 0) %>% 
    select(!borrar)

write.csv(aves_zonas_Prim, file = "output/2021/aves_zonas_Prim.csv" )


# b. otoño-invierno
aves_zonas_Inv <- aves_all[, c(1:5, 11, 13, 15)]

aves_zonas_Inv <- aves_zonas_Inv %>%
  mutate(borrar = Reserva_Inv + Camino_Inv + Ruta_Inv) %>% 
  filter(borrar > 0) %>% 
  select(!borrar)

write.csv(aves_zonas_Inv, file = "output/2021/aves_zonas_Inv.csv" )



#### Ahora podemos seguir con el otro código, para calcular índices de diversidad y de disimilitud #####
