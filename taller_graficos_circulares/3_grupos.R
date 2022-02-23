
# Librerias
library(ggplot2)
library(tidyverse)

# Creamos el dataset
data <- data.frame(
  individual=paste( "R ladie ", seq(1,60), sep=""),
  group=c( rep('A', 10), rep('B', 30), rep('C', 14), rep('D', 6)) ,
  value=sample( seq(10,100), 60, replace=T)
)

#importante!
data$group<-as.factor(data$group)

# Seteamos el numero de lugares vacios que queremos dejar entre las barras de los diferentes grupos
empty_bar <- 4 #filas voy a dejar entre grupos
to_add <- data.frame(matrix(NA, empty_bar*nlevels(data$group), ncol(data)))# lleno las filas vacias con NA
colnames(to_add) <- colnames(data) #pego los nombres de las columnas
to_add$group <- rep(levels(data$group), each=empty_bar) #agrego el grupo 
data <- rbind(data, to_add)# agrego en mi tabla las nuevas filas 
data <- data %>% arrange(group) #organizo por grupos para que me queden ordenados en el grafico
data$id <- seq(1, nrow(data)) # agrego el id

# Nombre y posicion de las etiquetas
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # sacamos 0.5 porque necesitamos que la letra este en el medio la barra. 
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# Hago el gráfico
con_grupos <- ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +      
  geom_bar(stat="identity", alpha=0.5) +
  ylim(-100,140) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE )+scale_fill_brewer(palette="Dark2")# elijo una paleta de colores

con_grupos



