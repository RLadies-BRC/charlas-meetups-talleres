
# Librerias
library(tidyverse)
library(ggplot2)

# Creamos el data set
data <- data.frame(
  id=seq(1,60),
  individual=paste( "R ladie ", seq(1,60), sep=""),
  value=sample( seq(10,100), 60, replace=T)
)

# y hacemos el plot
p <- ggplot(data, aes(x=as.factor(id), y=value)) +       # id lo hacemos factor. 
  
  
  geom_bar(stat="identity", fill=alpha("blue", 0.3)) + # agrgamos las barras de color azul
  
  # Limites del circulo = re importante!
  #El valor negativo controla el tamaño del circulo interior
  #El positivo controla el tamaño de las barras
  
  ylim(-100,110) +
  
  # armo el tema: sin titulo ni grilla de coordenadas cartesianas porque no las necesito
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm")   # Para remover los margenes que no se usan del plot
  ) +
  
  # y lo nuevo, que hace que grafico sea circulo
  coord_polar(start = 0)
p

?coord_polar
