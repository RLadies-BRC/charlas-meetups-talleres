##########################################################
## 1.R como uso de calculadora y carga manual de datos ###
##########################################################

# R como calculadora
1
1+5

# Carga manual de datos. Hay muchas clases de objetos en R: vectores, matrices, listas, 
# data.frames, funciones, etc. Es importante saber con que tipo de objeto estamos trabajando
# y cuales son sus propiedades. En R los vectores se ingresan con el comando c()

c(1,5,6,7,8,9,9)
datos=c(1,5,6,7,8,9,9)

# Ejemplito: aca tenemos datos de la relación entre la edad de un ave y el tamaño de su ala
edad=c(3,4,5,6,8,11,12,15,17)	
edad
ala= c(1, 1.5, 2, 2, 3,  3, 4,  4, 5)
hist(ala)
mean(ala)
plot(edad, ala)	
# edad y ala son vectores, mean, hist y plot funciones. 

##########################################################
############### 2.Descarga de paquetes ###################
##########################################################


## Descargamos el/los paquetes
install.packages("tidyverse")
install.packages("skimr")

## Ahora lo cargamos 
library(tidyverse)
library(skimr)
library(readr)
library(dplyr)

##########################################################
############### 3.Directorio de trabajo ##################
##########################################################

## Chequeemos el directorio donde estamos trabajando 
getwd()
## si lo queremos cambiar lo hacemos con el comando 
?setwd

##########################################################
################## 4.Carga de datos ######################
##########################################################

## Revisar siempre la direccion del archivo !
heights <- read_delim("heights.csv", ";", escape_double = FALSE, trim_ws = TRUE)

## Veamos un poco que es lo que tiene este archivo
View(heights)
names(heights)
skim(heights) 

##########################################################
################ 5.Hacer los datos tidy ##################
##########################################################

# En este caso la base de datos 'height' esta tidy. 
# Veamos que pasa cuando tenemos una base de datos que no lo está. Miremos table4a

?table4a
table4a
# Esta tabla nos da informacion sobre el numero de casos de tuberculosis por año y por pais

# Los nombres de las columnas 1999 y 2000 representar valores de la variable año
# y cada fila representa en este caso dos observaciones.
# Usamos la funcion gather 

table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "casos")

?table2
table2
# En este caso NO tenemos una observacion por fila, sino que están repartidas
# Usamos la funcion spread() 
spread(table2, key = type, value = count)

?table3
table3

#En este caso tenemos informacion de dos variables en un unica columna
# usamos la funcion separate()

table3 %>% 
  separate(rate, into = c("cases", "population"),sep="/")

?table5
table5

# En este caso century y year nos están dando informacion repetida. Usamos entonces
# la funcion unite()

table5 %>% 
  unite(new, century, year)


##########################################################
##################### 6. Funciones tidy ##################
##########################################################

## volvamos a usar los datos heights

# supongamos que nos queremos quedar unicamente con las observaciones provenientes
# de los hombres mayores a 45 años: Usamos la funcion filter

filter(heights, sex == "male", age > 45)

# No usamos mas el which!!

# Ahora supongamos que nos queremos quedar solamente con algunas de las variables
# usamos la funcion select. Si queremos solo los valores de sex and height

select(heights,sex,height)

# Podemos combinar las dos cosas con los pipes. Por ejemplo
heights %>%   filter(sex == "male",age>45) %>%   select(sex, height)

# Ahora supongamos que queremos agregar una variable que sea funcion dos 

heights %>% mutate(new_variable = earn * age)

# Ahora supongamos que lo que quremos hacer es cambiar el orden de las filas
# queremos que esten ordenadas por sex y despues por age

heights %>% arrange(sex,age)

# Por último, supongamos que queremos calcular alguna medida resumen sobre 
# alguna parte especifica de la base de datos. Por ejemplo la media de 
# la altura dependiendo del sexo.

heights %>%
  group_by(sex) %>%
  summarize(Mean.height = mean(height))
