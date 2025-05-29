#Descargas masivas de ocurrencias desde GBIF
#Catalina Marín - BASE
#2025-05-22

#Settings----
setwd("C:/R/rgbif_tutorial") # selección de carpeta donde están los datos

#Instalación de paquetes
install.packages("rgbif")
install.packages("usethis")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("maps")
install.packages("tidyverse")
install.packages("rlang")

#Library----
library(rgbif)
library(rlang)
library(usethis)
library(dplyr)
library(ggplot2)
library(maps)


#Datos----
amphibia_rce <- read.csv("amphibia_rce.csv", fileEncoding = "UTF-8", sep = ";")
registros_amphibia <-  read.csv("registros_amphibia_gbif.csv", fileEncoding ="UTF-8",
                                sep = ",") #datos ya descargados desde GBIF
#1. Exploracion de los datos ----

View(amphibia_rce) #ver la tabla del ministerio

#de momento solo nos interesa trabajar con los nombres y la categoría

sp_amphibia <- amphibia_rce %>% 
              select(NOMBRE.CIENTÍFICO, NOMBRE.COMÚN, CATEGORÍA.VIGENTE) %>% #seleccionar columnas de interés
              rename( nombre_cientifico = NOMBRE.CIENTÍFICO,
                      nombre_comun = NOMBRE.COMÚN,
                     categoria = CATEGORÍA.VIGENTE) #renombrar columnas
# ¿Cuantos registros de cada especie hay en Chile?
# ¿En que categoría de amaneza hay más registros?

#2. Preparación para la descarga ----
usethis::edit_r_environ() #Se debe configurar solo una vez, abre otra pestaña 
#con una consola vacía donde tienes que escribir tus
#credenciales, a continuación se indica como
#En la consola que se abre debes copiar lo siguiente y completar con tus datos
GBIF_USER="nombredeusuario" #No correr
GBIF_PWD="contraseña"       #No correr
GBIF_EMAIL="correo@gbif.cl" #No correr

#3. Descarga de ocurrencias ----
  ## Pimer paso: obtener taxonKeys----
# Cada especie (taxon) en GBIF tiene un número asociado, y para poder descargar
# de manera masiva estos datos es necesario hacer busquedas con el número
# registrado en GBIF, es decir, el taxonkey

taxonkeys_amphibia <- sp_amphibia %>% # nombre del objeto donde esta la lista de especies
  pull("nombre_cientifico") %>% # nombre de la columna
  name_backbone_checklist() %>% # funcion a utilizar
  filter(!matchType == "NONE") %>% # obtener nombres coincidentes
  pull(usageKey) # obtener los taxonKey

  ## Segundo paso: realizar la descarga----

# Podemos comunicarnos con GBIF gracias a que tenemos los taxonkeys
solicitud_registros<- occ_download(
  pred_in("taxonKey", taxonkeys_amphibia), # selecionar los taxonkeys
  pred("hasCoordinate", TRUE), # puntos que tengan coordenadas
  pred("country","CL"), # solo en chile
  pred("hasGeospatialIssue", FALSE), # sin errores geoespaciales
  format = "SIMPLE_CSV") #formato simple

occ_download_wait(solicitud_registros) #estado de la descarga 

#Cuando finaliza dará la información de los metadatos de la descarga incluido el DOI
    ### <<gbif download metadata>>----
#Status: SUCCEEDED
#DOI: 10.15468/dl.gh3ngh
#Format: SIMPLE_CSV
#Download key: 0014746-250515123054153
#Created: 2025-05-22T19:21:37.978+00:00
#Modified: 2025-05-22T19:28:42.601+00:00
#Download link: https://api.gbif.org/v1/occurrence/download/request/0014746-250515123054153.zip
#Total records: 5296
  ## Tercer paso: extraer los datos descargados----
# un vez que el proceso de descarga finaliza, podemos extraer el archivo
# recordar que la información se guarda en la cuenta GBIF, ojo si en que el DOI no expire

registros_amphibia<- occ_download_get(solicitud_registros) %>% #obtenerlas occurrencias
  occ_download_import() #importar a R la tabla

#Guardar occurrencias en formato csv
write.csv(registros_amphibia, "registros_amphibia_gbif.csv", na= "NA", row.names = F)
#3. Trabajar con el archivo de ocurrencias----

#vamos a explorar el archivo descargado
unique(registros_amphibia$scientificName) #lista con los nombres de especies

#existe un error al momento de buscar en la columna de nombre científico

unique(registros_amphibia$species)#guarda el nombre de la especie sin el autor

#Comparar diferencias entre el archivo original y el archivo GBIF

sp_gbif <- unique(registros_amphibia$species)
sp_original <- unique(sp_amphibia$nombre_cientifico)
setdiff(sp_original, sp_gbif) #entrega los elementos únicos del primer objeto
setdiff(sp_gbif, sp_original)

#Revisar las diferencias taxonomicas
nombres_faltantes <- setdiff(sp_original, sp_gbif)

revision_taxonomia <- name_backbone_checklist(nombres_faltantes)

# si buscamos en GBIF https://www.gbif.org/es/search?q=Alsodes%20kaweshkari 
# podemos ver que el nombre si se encuentra aceptado, pero que no hay registros
# es importante conocer la historia de las especies para saber como trabajar con los datos

#agregar información sobre clasificación
registros_amphibia$categoria <- sp_amphibia$categoria[
              match(registros_amphibia$species,sp_amphibia$nombre_cientifico)]


#4. Resumen básico los datos----

conteo_especie <- registros_amphibia%>%
  group_by(species) %>% #Agrupar datos por especie
  summarise(conteo = n()) %>%  #solo estamos contando el numero de filas por especies
  ungroup()

conteo_categoria <-  registros_amphibia%>%
  group_by(categoria) %>% #Agrupar datos por categoría
  summarise(conteo = n()) %>% 
  ungroup()


#5. Gráficos ----
grafico_amenzas <- ggplot(conteo_categoria,
                          aes(x= categoria, y = conteo, 
                              fill = categoria))+
   geom_bar( stat='identity') +
   theme(axis.text.x=element_blank())
 
 print(grafico_amenzas)
 
#utilizar ggplot interactivo
 install.packages("esquisse")
 install.packages("plotly")
 install.packages("ragg")
 library(esquisse)
 library(ragg)
 esquisse::esquisser()
 
#6. Extra: Mapa de Chile con los registros categorizados por amenzas
 
#obtener el conteo por punto
 conteo_punto <- registros_amphibia%>%
   group_by(species, decimalLatitude, decimalLongitude) %>% #Agrupar datos por punto
   summarise(conteo = n()) %>% 
   ungroup()
 
 
 mapa_especies <- ggplot(registros_amphibia, 
                         aes(x = decimalLongitude, y = decimalLatitude, 
                             color = categoria)) + # los datos y respecto a que queremos el color
   borders(database = "world", regions = ("Chile"), size = 0.3) +  # el mapa de Chi
   # Change the colour and transparency of the plotted occurrence points 
   geom_point(alpha = 0.4)+ # agregamos puntos respecto a los datos y el alpha es transparencia
   labs(col= "Categorías de Riesgo") #  titulo de la leyenda
 
 print(mapa_especies)
 
