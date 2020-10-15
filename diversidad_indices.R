####### Comunidades de aves de Ceibas, provincia de Entre Rios #############
####### Análisis de diversidad a partir de registros de GBIF ##############

####### Comunidades de aves de Ceibas, provincia de Entre Rios #############
####### Análisis de diversidad a partir de registros de GBIF ##############

####### Código escrito por Natalia Morandeira, nmorandeira@unsam.edu.ar ####
###### Colaboradora: Sofía Larrandart, slarrandart@gmail.com 
####### Septiembre-Octubre de 2020. Parte del código fue destinado a la asignatura Ecología, ECyT-3iA, Universidad Nacional de San Martín

# En el ejemplo usamos una base de datos de eBird obtenida a través de. El dataset de este ejercicio fue descargado por Sofía Larrandat y tiene una licencia CC 1.0: https://www.gbif.org/occurrence/download/0074472-200613084148143
# Cita del dataset: GBIF.org (03 October 2020) GBIF Occurrence Download https://doi.org/10.15468/dl.474hgg


#### Análisis de datos de similitud de comunidades y estimaciones de diversidad ##########
## INTRODUCCION ######

# Tenemos que definir qué períodos nos interesa comparar. Por ejemplo: 
  # a) comparar entre distintas estaciones del año.
  # b) comparar entre distintos años (hay registros desde 1988, aunque más abundantes desde 2011: el primer cuantil de las fechas es el 2 de abril de 2011), 
  # - más avanzado 1: comparar entre períodos secos y húmedos (necesitaríamos tener datos climáticos para el período 1988-2019).
  # - más avanzado 2: comparar entre zonas dentro del área de estudio, que podrían a su vez asociarse a distintos hábitats o a distintos usos del suelo (tendríamos que haber guardado las coordenadas de latitud y longitud de las observaciones, que estaban en el archivo original y luego saber trabajar con datos espaciales en R o en un software SIG específico).
  
  # Les sugiero que elijan qué análisis hacer en cada grupo: (a) o (b).

## Los datos fueron pre-procesados con el script "diversidad_gbif_prepro.R" para así obtener las tablas aves_lustro.csv y aves_estacion.csv. Es un script un poco más complejo, pero está comentado para que se entienda lo que hicimos! Lo pueden bajar si desean: https://github.com/nmorandeira/diversidad_gbif/blob/main/diversidad_gbif_prepro.R 
## De todos modos sugiero primero hacer primero este script de índices de diversidad y similitud


### CARGA DE DATOS ######
## Cargar las tablas de especies (filas) x períodos (columnas). Se puede hacer desde el RStudio (Environment > Import Dataset) o usando "read_csv" como muestro abajo

# Si cargan desde RStudio, llamen a los datos "aves_ceibas_estacion" o "aves_ceibas_lustro"


library(readr)
## OPCION A #### 
# Si decidimos trabajar comparando las comunidades de aves entre estaciones
aves_ceibas_estacion <- read_csv("data/aves_estacion.csv") #cambiar la ubicacion del archivo de acuerdo a tu computadora

head(aves_ceibas_estacion) #"head" nos muestra las primeras filas, para tener una idea de la tabla de datos. 
colnames(aves_ceibas_estacion) #nos muestra las columnas. Nos vamos a quedar sólo con los códigos y las comunidades a analizar
tabla_ceibas <- aves_ceibas_estacion[,5:8]  #Significa: [filas, columnas] que me quedo. Por ejemplo: [2,3] significa fila 2 y columna 3; [,1] significa todas las filas y la columna 1; [1,] significa la fila 1 y todas las columnas. En este caso le pedimos: todas las filas y las columnas 5 a 7: [,5:8]
head(tabla_ceibas) #chequeamos que esté bien

#vamos a reemplazar los NA por ceros
library(imputeTS)
tabla_ceibas <- na_replace(tabla_ceibas, 0)

head(tabla_ceibas)
View(tabla_ceibas)

## OPCION B ####
# O bien, si decidimos trabajar comparando las comundidades de aves entre lustros (pueden dejarlo para después y repetir el análisis)
aves_ceibas_lustro <- read_csv("data/aves_lustro.csv") #cambiar la ubicacion del archivo de acuerdo a tu computadora
head(aves_ceibas_lustro) #"head" nos muestra las primeras filas, para tener una idea de la tabla de datos. 
colnames(aves_ceibas_lustro) #nos muestra las columnas. Nos vamos a quedar sólo con los códigos y las comunidades a analizar
tabla_ceibas <- aves_ceibas_lustro[,5:7]  #Significa: [filas, columnas] que me quedo. Por ejemplo: [2,3] significa fila 2 y columna 3; [,1] significa todas las filas y la columna 1; [1,] significa la fila 1 y todas las columnas. En este caso le pedimos: todas las filas y las columnas 5 a 7: [,5:7]
head(tabla_ceibas) #chequeamos que esté bien

#vamos a reemplazar los NA por ceros
library(imputeTS)
tabla_ceibas <- na_replace(tabla_ceibas, 0)

head(tabla_ceibas)
View(tabla_ceibas)

###########################################
##### SEGUIMOS para ambas opciones! #######
library(vegan) #libreria vegan ("vegetation analysis")

### INDICES DE DIVERSIDAD ########
## Riqueza de especies #### 
riqueza <- specnumber(tabla_ceibas, MARGIN = 2) #margin = 2 significa que la matriz es de especies (filas) x comunidades (columnas)
riqueza

## Diversidad de Simpson
div_Simpson <- diversity(tabla_ceibas, index="simpson", MARGIN = 2)
div_Simpson

# Equitatividad de Shannon
equit_Simpson <- div_Simpson / (1-1/riqueza)
equit_Simpson

## Diversidad de Shannon
div_Shannon <- diversity(tabla_ceibas, index="shannon", MARGIN = 2)
div_Shannon

# Equitatividad de Shannon
equit_Shannon <- div_Shannon / log(riqueza)
equit_Shannon

# Vamos a exportar todos los datos a una tabla
resultados_diversidad <- rbind(riqueza, div_Simpson, equit_Simpson, div_Shannon, equit_Shannon) #rbind significa row-bind, o sea, unir por filas (los datos comparten las columnas)
#se ve bien, pero veamos su estructura
str(resultados_diversidad)
#dice "num" o sea que piensa que es una serie de números. Para guardarlo a disco es conveniente que sea un "data.frame¨  (tabla de datos):
resultados_diversidad <- as.data.frame(resultados_diversidad)
resultados_diversidad
#de aspecto no cambió... a ver su estructura
str(resultados_diversidad)

write_csv(resultados_diversidad, path = "output/resultados_diversidad.csv")

### INDICES DE DISIMILITUD ##########
# la función se llama betadiver. Primero vamos a pedirle que nos liste los índices disponibles
betadiver(help = TRUE)
# Buscar los índices de Jaccard y de Sorensen
# este índice necesita la matriz transpuesta
tabla_ceibas_transp <- t(tabla_ceibas)

#similitud de Jaccard
betadiver(tabla_ceibas_transp , method = "j")
# si quisiéramos una disimilitud (diversidad beta)
1 - betadiver(tabla_ceibas_transp , method = "j")

#similitud de Sorensen
betadiver(tabla_ceibas_transp , method = "sor")
# si quisiéramos una disimilitud (diversidad beta)
1 - betadiver(tabla_ceibas_transp , method = "sor")


## El índice de Czekanowski no está en "betadiver", habría que calcularlo armando las columnas de abundancia relativa
head(tabla_ceibas)

tabla_ceibas_pi <- tabla_ceibas #solo duplicamos la tabla

#para el caso de comunidades por estaciones
tabla_ceibas_pi$Primavera_pi <- tabla_ceibas$Primavera/sum(tabla_ceibas$Primavera)
tabla_ceibas_pi$Verano_pi <- tabla_ceibas$Verano/sum(tabla_ceibas$Verano)
tabla_ceibas_pi$Otonio_pi <- tabla_ceibas$Otonio/sum(tabla_ceibas$Otonio)
tabla_ceibas_pi$Invierno_pi <- tabla_ceibas$Invierno/sum(tabla_ceibas$Invierno)
#nos quedamos sólo con las columnas de abundancia relativa
#Para estaciones:
tabla_ceibas_pi<-tabla_ceibas_pi[,c(5:8)]


#para el caso de comunidades por lustros
tabla_ceibas_pi$Per1_pi <- tabla_ceibas$`Periodo2005-2009`/sum(tabla_ceibas$`Periodo2005-2009`)
tabla_ceibas_pi$Per2_pi <- tabla_ceibas$`Periodo2010-2014`/sum(tabla_ceibas$`Periodo2010-2014`)
tabla_ceibas_pi$Per3_pi <- tabla_ceibas$`Periodo2015-2019`/sum(tabla_ceibas$`Periodo2015-2019`)
#nos quedamos sólo con las columnas de abundancia relativa
#Para lustros:
tabla_ceibas_pi<-tabla_ceibas_pi[,c(4:6)]


#Ejemplo: para comparar periodo 1 con periodo 3 (cambiar los numeros de acuerdo a lo deseado)
tabla_ceibas_cze <- tabla_ceibas_pi[,c(1,3)]
indice_czekanowski <- sum(apply(tabla_ceibas_cze, 1, min)) #sumatoria del mínimo entre las columnas seleccionados
disimilitud_czekanowski <- 1- indice_czekanowski
disimilitud_czekanowski

#Si quieren hacer algún gráfico me pueden consultar :)