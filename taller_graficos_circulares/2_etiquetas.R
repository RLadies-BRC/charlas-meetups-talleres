
# Librerias
library(ggplot2)
library(tidyverse)

# Se crea el dataset
data <- data.frame(
  id=seq(1,60),
  individual=paste( "R ladie ", seq(1,60), sep=""),
  value=sample( seq(10,100), 60, replace=T)
)

# ----- creamos un dataframe para las etiquetas ---- #
# primero definimos el nombre, la posicion de las etiquetas y el angulo de las etiquetas
label_data <- data

# calcular el angulo de las etiquetas
number_of_bar <- nrow(label_data) #numero de filas (una para cada barra)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # sacamos 0.5 porque necesitamos que la letra este en el medio la barra. 

# calculamos la alineacion horizontal (hjust)
# en la parte izquierda del plot, las etiquetas tienen que tener un angulo de < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# damos vuelta la etiqueta para que se lea bien
label_data$angle<-ifelse(angle < -90, angle+180, angle)
# ----- ------------------------------------------- ---- #


# Hacemos el plot
con_etiquetas <- ggplot(data, aes(x=as.factor(id), y=value)) +       # id como factor
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
  
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-100,120) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")      # Ajusta los margenes para que las etiquetas no se den vuelta!
  ) +
  
  # Agregamos coordenadas polares en vez de cartesianas
  coord_polar(start = 0) +
  
  # Agregamos las etiquetas, usando label_data dataframe que creamos anteriormente
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 

con_etiquetas



?geom_text
