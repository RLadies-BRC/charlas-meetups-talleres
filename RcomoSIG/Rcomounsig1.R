############################
#R como un SIG: WorldClim###    
############################

#Intro a R como un SIG y 
#Extracci?n de datos clim?ticos de WorldClim a partir de un archivo de coordenadas


#Paula N. Fergnani
#En prep para Ecolog?a Austral

#Descargar los paquetes
install.packages("tidyverse")
install.packages("sp")
install.packages("rgdal") 
install.packages("maptools")
install.packages("raster")
install.packages("rgeos")


#Cargar a R los paquetes
library(tidyverse)
library(sp)
library(rgdal) 
library(rgeos)
library(maptools)
library(raster)

#Establecer el directorio de trabajo
setwd("/home/sofia/Rladies/taller_sig/")

#Crear un mapa con mis datos de ocurrencia en wgs 84#

#Cargar los datos y verlos
datos=read.table("Abund.txt",header=T)
View(datos)

#Cargar y guardar las coordenadas en un marco de datos llamado "coords"
coords=read.table("Abund.txt",header=T)

#Convertir las coordenadas a un objeto espacial con la funci?n "coordinates"
coordinates(coords)=~x+y

#Ver un sumario de las coordenadas "coords"
summary(coords)

#Ver la tabla de atributos del objeto espacial
coords@data
View(coords@data)
View(coords)

#Especificar la proyecci?n del objeto espacial
proj4string(coords)=CRS("+proj=longlat +datum=WGS84")

#Ver las propiedades del objeto espacial
summary(coords)

#Si quiero agregar datos manualmente, agregar un vector al dataframe
coords@data$Ambiente=c("bosque","bosque","bosque","bosque","bosque","bosque","bosque","bosque","bosque","bosque","bosque", "bosque","bosque","bosque","bosque","bosque","bosque","matorral","matorral","matorral","matorral","matorral","matorral","matorral",
"matorral","matorral", "estepa", "estepa","estepa","estepa","estepa","estepa","estepa","estepa","estepa","estepa","estepa","estepa","estepa","estepa","estepa","estepa","estepa","estepa","estepa","estepa","estepa","estepa","estepa","estepa")

#ver nuevamente la tabla de atributos
View(coords@data)

#ver las coordenadas
coords@coords

#Graficar para verificar que mis coordenadas est?n correctamente situadas
plot(coords)

plot(coords, pch=20, col="red", cex=4, main="Mis sitios")
plot(coords[ coords@data$Ambiente=="bosque", ], col = "turquoise", add = TRUE) 

data(wrld_simpl) #cargar el mapa del mundo que est? en el paquete maptools
plot(wrld_simpl)
summary(wrld_simpl) 
plot(coords, pch=20, col="red", cex=4, main="Mis sitios", add=TRUE)

head(wrld_simpl@data) #Ver que en NAME est?n los paises
plot(wrld_simpl[ wrld_simpl@data$NAME=="Argentina", ])
plot(coords, pch=18, col="red",  main="Mis sitios", add=TRUE)

#Estructura del objeto espacial
str(coords) #todo esto contiene dentro el objeto de coordenadas de muestreo

#Descargar los datos de WorldClim mundiales y visualizarlos usando una escala de celda grande (340 Km2)
#Con el argumento var = "bio", se descargan 19 variables
datos_bioG<-raster::getData("worldclim", var = "bio", res = 10)
plot(coords, pch=18, col="red",  main="Mis sitios", add=TRUE)


png(file='mapa.png',width=250,height=300)

dev.off()
#Ver la info de la funci?n getData del paquete raster. 
#Identificar qu? informaci?n se puede obtener con esta funci?n
?getData

#Ploteo todas las variables 
p=plot(datos_bioG)

#Las variables de WorldClim que obtuvimos son las "bio":
#BIO1 = Annual Mean Temperature
#BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
#BIO3 = Isothermality (BIO2/BIO7) (* 100)
#BIO4 = Temperature Seasonality (standard deviation *100)
#BIO5 = Max Temperature of Warmest Month
#BIO6 = Min Temperature of Coldest Month
#BIO7 = Temperature Annual Range (BIO5-BIO6)
#BIO8 = Mean Temperature of Wettest Quarter
#BIO9 = Mean Temperature of Driest Quarter
#BIO10 = Mean Temperature of Warmest Quarter
#BIO11 = Mean Temperature of Coldest Quarter
#BIO12 = Annual Precipitation
#BIO13 = Precipitation of Wettest Month
#BIO14 = Precipitation of Driest Month
#BIO15 = Precipitation Seasonality (Coefficient of Variation)
#BIO16 = Precipitation of Wettest Quarter
#BIO17 = Precipitation of Driest Quarter
#BIO18 = Precipitation of Warmest Quarter
#BIO19 = Precipitation of Coldest Quarter

#De esta forma selecciono variables si las necesito para otra cosa, as? las primeras 4 capas
datos_bioG[[1:4]]

#Graficar con colores de fr?os a c?lidos
tempcol <- colorRampPalette(c("purple", "blue", "skyblue", "green", "lightgreen", "yellow", "orange", "red", "darkred"))
plot(datos_bioG$bio1, main="Annual Mean Temperature", col=tempcol(100))
#El n?mero entre par?ntesis despu?s de "tempcol" le dice a  R cuantos colores "crear", usando los colores definidos en el objeto "tempcol"  como paleta. Entonces, tempcol(100) crea 100 colores formando una escala "suave" de colores.

#Marco la ubicaci?n de las coordenadas de muestreo
points(coords) 

#ver la informaci?n sumaria del RasterStack
datos_bioG

#Para obtener los datos de worldclim con resoluci?n de mucho detalle, 0.5 (aprox 1 km2)
#tengo que informar una latitud y una longitud que est? aprox en el medio de los puntos de muestreo (uso por ejemplo la mediana)
#porque la capa completa del mundo es un archivo muy pesado a esta resoluci?n, as? que solo se baja la secci?n donde est?n los datos de inter?s

median(datos$x) #usar? esta longitud
median(datos$y) #usar? esta latitud

datos_bio<-raster::getData("worldclim", var = "bio", res = 0.5, lon=-71.15579, lat=-40.62685)
#Analizamos los argumentos de getData:
#Selecci?n del dataset: El primer argumento especifica el dataset,  'worldclim' devuelve "the World Climate Data".
#Selecci?n de la variable: El segundo argumento especifica la variable  'tmin', 'tmax', 'prec' o 'bio'.
#Selecci?n de la resoluci?n:  0.5, 2.5, 5, y 10 (minutos de grado). En el caso de la resoluci?n res=0.5, hay que proveer argumentos lon y lat para que se obtener la secci?n correspondiente

#grafico la secci?n que obtuve
plot(datos_bio[[1]], main="Annual Mean Temperature")
points(coords)
plot(datos_bio)

#Grafico para que me quede una visualizaci?n
Argentina1 <- getData('GADM' , country="ARG", level=1)
plot(Argentina1)
plot(coords, pch=20, col="black", cex=1, add=TRUE)
s <- select(Argentina1) #herramienta para seleccionar pol?gonos manualmente
plot(s)
plot(coords, pch=20, col="black", cex=1, add=TRUE)
rect <- crop(datos_bio[[1]], s) # cortar un rectangulo de la extensi?n de mis provincias de inter?s
plot (rect)
mascara <- mask(rect, s) #hacer una m?scara, para que solo me muestre lo que est? dentro de mis provincias
plot(mascara, main="Sitios de muestreo y temperatura anual")
plot(coords, pch=20, col="black", cex=1, add=TRUE)
plot(s, add=TRUE)


#Extraer las variables bio de WorldClim para mis coordenadas de muestreo
valores <- extract(datos_bio, coords)

#Armar un marco de datos con las coordenadas, los valores extraidos y la Abundancia
df <- cbind.data.frame(coordinates(coords),valores, abundancia=coords@data$Abundancia)

View(df)

#Exportar los datos como una tabla
write.table(df, "df.txt", row.names = FALSE)


#Creo un objeto espacial con los resultados y lo exporto como un shape
AbundF=SpatialPointsDataFrame(coords,df) #creo el objeto espacial que contiene los valores extraidos
head(AbundF)
summary(AbundF)
writePolyShape(AbundF, "Abund_bio")  #exporto como shape


#Exploraci?n de los datos extraidos usando estadistica descriptiva

cor(df)  #correlaciones

boxplot(df$bio1_43)  #gr?fico de cajas, variaci?n de la temperatura entre mis sitios de muestreo

hist(df$bio1_43)  #histograma

plot(df$bio1_43, df$abundancia)  #dispersi?n

plot(df$bio1_43, log10(df$abundancia))

reg= lm(log10(df$abundancia) ~ df$bio1_43)  #regresi?n lineal simple con distribuci?n normal
plot(df$bio1_43, log10(df$abundancia))
abline(reg, col="blue")
summary(reg)


################################################################################################
#CUANDO LA EXTENSI?N DEL ESTUDIO ES GRANDE Y QUIERO USAR LA BASE DE WORLDCLIM DE RESOLUCI?N 0.5#
################################################################################################

#Ir a  la web de Worldclim, descargar las variables de inter?s a escala de 30 segundos
#Deszipear la carpeta y guardar las variables en el directorio de trabajo
#Cargar los rasters en R y continuar como antes
#Para este ejemplo uso la variable descargada bio_1, dentro de la carpeta bio

t <- raster(paste(getwd(), "/bio/bio_1", sep = ""))
#La funci?n raster lee un mont?n de formatos diferentes de raster
plot(t)

#Si quiero ver los datos divididos por 10
#t_corregida <- t/10  #esto est? comentado porque pude demorar
#plot(t_corregida)



















