####### Comunidades de aves de Ceibas, provincia de Entre Rios #############
####### Análisis de diversidad a partir de registros de GBIF ##############

####### Código escrito por Natalia Morandeira, nmorandeira@unsam.edu.ar ####
####### Septiembre-Octubre de 2020. Parte del código fue destinado a la asignatura Ecología, ECyT-3iA, Universidad Nacional de San Martín


# Tenemos que definir qué períodos nos interesa comparar. Por ejemplo: 
  # a) comparar entre distintas estaciones del año.
  # b) comparar entre distintos años (hay registros desde 1988, aunque más abundantes desde 2011: el primer cuantil de las fechas es el 2 de abril de 2011), 
  # - más avanzado 1: comparar entre períodos secos y húmedos (necesitaríamos tener datos climáticos para el período 1988-2019).
  # - más avanzado 2: comparar entre zonas dentro del área de estudio, que podrían a su vez asociarse a distintos hábitats o a distintos usos del suelo (tendríamos que haber guardado las coordenadas de latitud y longitud de las observaciones, que estaban en el archivo original y luego saber trabajar con datos espaciales en R o en un software SIG específico).
  
  # Les sugiero que elijan qué análisis hacer en cada grupo: (a) o (b).

######## TERCERA PARTE - Indices de disimilitud #######
# Para comparar entre estaciones del año o entre los lustros,
# podemos usar índices de disimilitud.

# Primero, cargar la tabla de datos deseada (si la generaron con los pasos anteriores, se puede omitir este paso)
library(readr)

# Si definimos trabajar comparando lustros
aves <- read.csv("data/aves_lustro.csv", row.names=1)
View(aves) 

# Si definimos trabajar comparando estaciones
aves <- read.csv("data/aves_estacion.csv", row.names=1)
View(aves) 

### En cualquiera de los dos casos casos, se listan las especies y la abundancia máxima observada en un día durante el período (la estación o el lustro). 
### "NA" significa "Not available" (No disponible), es decir, que la especie no se observó en ese período.


## Riqueza específica
library(vegan)
specnumber(t(aves))


####### CUARTA PARTE - Estimaciones de diversidad #########

#estandarizar matriz
spp_escaladas=scale(spp, center=T, scale=T) #centrar y escalar. También se puede sólo centrar o sólo escalar

#diversidad
library(vegan)

#matriz de sitios (filas) x spp (columnas) = spp

shannon=diversity(spp125, index="shannon", base=exp(1)) #indice de Shannon, por defecto lo hace con LN
simpson=diversity(spp125, index="simpson") #indice de Simpson
hill=diversity(spp125, index="invsimpson") #indice de Hill

#riqueza específica
spp.diver$richness=specnumber(t(spp125))

#equitatividad
spp.diver$equit= (spp.diver$simpson) / (spp.diver$richness)