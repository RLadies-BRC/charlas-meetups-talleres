# Entorno de RStudio -------------
#   1. Superior izq.: Scripts (Aqui!)
#   2. Inferior izq.: Consola (Console)
#   3. Superior der.: Entorno (Environment)
#   4. Inferior izq.: Plots, Ayuda, Paquetes, etc



# Función numérica con salida implícita:
suma_nueva <- function(x = 10, y = 7){
  x + y # instruccion 1
}

suma_nueva() #porque los argumentos ya estan definidos
suma_nueva(5) #considera x=5
suma_nueva(y = 2) 
suma_nueva(5,2) 

#Vamos a cambiar la ultima instruccion
suma_nueva2 <- function(x = 10, y = 7){
  z <- x + y # instruccion 1 
  2 * x + z # instruccion 2
}
suma_nueva2() 




#Vamos a cambiar la última instrucción
suma_nueva2 <- function(x = 10, y = 2){
  x + y
  2 * x 
  y + 5 # última instrucción
}

suma_nueva2()

# Función  con salida explícita:
suma_nueva3 <- function(x = 10, y = 2){
  z <- x + y
  2 * x 
  y + 5 # ultima instruccion
  
  s <- c(z,y)
  return(s)
}

suma_nueva3(8,19)

suma_nueva4 <- function(x,y){
  z1 <- x + y
  z2 <- 2 * x 
  z3 <- y + 5 
  z <- c(z1,z2,z3)
  return(z)
}

suma_nueva4(2,8)
vector_salida <- suma_nueva4(2,8)
 

vector_salida[2]

# Entornos --------------------
# Mismos nombres, diferentes valores 
# (variables locales)

# //////////////////////////////////////
suma_nueva5 <- function(x = 10, y = 2){
  z <- x + y
  print(paste("El valor de y es ",y))
  return(z)
}
# //////////////////////////////////////

y <- 10
suma_nueva5() # Considera "y" que esta definido por default en la función
suma_nueva5(y = 4)
suma_nueva5(y = y) # en este caso considera "y" como el valor que aparece previamente

# Se pueden usar los valores externos si no son
# argumentos (variables globales)

# //////////////////////////////////////
# En este caso el valor de y no se toma de los parametros
# Busca el valor de "y" afuera del entorno de la funcion 
# 

suma_nueva5 <- function(x = 10){
  z <- x + y
  print(paste("El valor de y: ",y))
  return(z)
}
# //////////////////////////////////////

y <- 10
suma_nueva5()

suma_nueva5(y = 4)
suma_nueva5(x = 4)


multiplico_2numeros<-function(x,y){
  x*y
}

multiplico_2numeros(3,2)

resta_entre2<-function(x,y){
  x-y
}

resta_entre2(x,y)


resta_entre2(3,5)
resta_entre2(5,3)

resta_entre2(y=8,x=3)

resta_entre2(x=3,y=8)

desigualdad <- function(x,y)
{
  if (x<y){
    return(TRUE)
  }
  if (x>=y){
    return(FALSE)
  }
}

desigualdad(3,5)


# Definiendo operadores
# Concatenar strings
`%+%` <- function(palabra_1, palabra_2){
  palabra_final <- paste(palabra_1,palabra_2)
  return(palabra_final)
}

# La funcion va entre los argumentos
nombre <- "tu nombre"
apellido <- "tu apellido"
telefono <- "+54"
nombre %+% apellido
nombre %+% apellido %+% telefono

nombre <- c("Lau", "Lina")


# Entorno: re-definiendo operadores
`+` <- function(x,y){
  return(x - y)
}

2 + 2  # !!!
rm('+')

2 + 2  


#Ejercicios : 
# 1) Crear una función que convierta temperaturas de grados Celsius a Fahrenheit.
# 2) Crear una función que calcule el área de un círculo dado su radio.
# 3) Crear una función llamada "evaluar_numero" que tome un número como argumento.
  # Nota: La función debe devolver:
  # "Positivo" si el número es mayor que 0.
  # "Negativo" si el número es menor que 0.
  # "Cero" si el número es igual a 0.

#Solución: 
# Definimos la función
celsius_a_fahrenheit <- function(celsius) {
  # Fórmula para convertir a Fahrenheit
  fahrenheit <- celsius * 9/5 + 32
  # Muestra el resultado
  return(fahrenheit)
}

# Ejemplo de uso
celsius_a_fahrenheit(25)  # Debería devolver 77

#solucion eje 2:

# Definimos la función
area_circulo <- function(radio) {
  # Calculamos el área usando la fórmula
  area <- pi * radio^2
  # Devuelve el resultado
  return(area)
}

# Ejemplo de uso
area_circulo(3)  # Debería devolver 28.27433

#Eje 3 solucion: 

evaluar_numero <- function(x) {
  if (x > 0) {
    return("Positivo")
  } else if (x < 0) {
    return("Negativo")
  } else {
    return("Cero")
  }
}

evaluar_numero(3)


#install.packages("dplyr")
#install.packages("ggplot2")

library(dplyr)
library(ggplot2)

calcular_medidas <- function(data) {
  
  #  Utilizando la función select del paquete dplyr filtramos solo las 
  #  columnas numéricas de nuestra base de datos
  #  donde where(is.numeric) es una función auxiliar que selecciona
  #  las columnas que son de tipo numérico.
  data_num <- select(data, where(is.numeric))
  
  # Guarda los nombres de las columnas de mi base data_num en una
  # variable llamada nombres
  nombres <- names(data_num)
  
  # Calcular la media, mediana, sd.
  # sapply aplica una función (en este caso, mean, median y sd) a cada columna 
  # del data frame data_num y devuelve un vector.
  medias <- sapply(data_num, mean)
  medianas <- sapply(data_num, median)
  sds <- sapply(data_num, sd)
  
  # Creamos una matriz 11x3
  # Para esto usamos la funcion cbind que combina los 3 vectores por columna.
  # y tenemos tantas filas como columnas numéricas tenga data_num.
  
  matriz <- cbind(medias, medianas, sds)
  colnames(matriz) <- c("Media", "Mediana", "Sd")
  
  rownames(matriz) <- names(data_num)
  View(matriz)
  # Guardar en un archivo CSV
  write.table(matriz, file = "mi_resumen.csv", sep = ",")
  print("Archivo CSV guardado exitosamente")
  
  return(matriz) # Para ver la matriz al ejecutar la función
}

View(mtcars)

calcular_medidas(mtcars)


salida=read.table("mi_resumen.csv", header=TRUE, sep=",") 
View(salida)



graficamos <- function(archivo){
  salida <- read.table(archivo, header=TRUE, sep=",") 
  
  numero_cols <- dim(salida)[2] 
  nombres <- rownames(salida)
  
  for (i in 1: numero_cols){
    
  migrafico <- salida %>% ggplot(aes(x = nombres, y = salida[,i])) + geom_point( size=2,color= "blue4" )+ 
  theme_set(theme_bw(base_size = 24))+ 
  labs(x = "Variables", y = colnames(salida)[i]) + theme(plot.title = element_text(size=24)) +
  theme(axis.text.x=element_text(angle=45,hjust=1))
  print(migrafico)
  }
}

graficamos("mi_resumen.csv")
