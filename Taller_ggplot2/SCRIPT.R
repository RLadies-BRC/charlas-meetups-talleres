library(tidyverse)
library(ggplot2)
library(nlme)
library(ggpubr)
library(maptools)
library(gganimate)
library(plotly)
library(gridExtra)
library(RColorBrewer)
library(tidyr)
library(dplyr)
library(gapminder)
library(viridis)


setwd("C:/Users/Win/Desktop/meetup ggplot2")
# 1- Indicar con qué datos se va a trabajar.
ggplot(data=iris)
# 2.Especificar los componentes “estéticos”
ggplot(data=iris, aes(x=Sepal.Length, y= Sepal.Width))
View(iris)
# 3.Añadir capa: especificar el tipo de objeto geométrico
ggplot(data=iris, aes(x=Sepal.Length, y= Sepal.Width)) + geom_line()
# 4. Añadir más capas:
ggplot(data=iris,aes(x=Sepal.Length, y= Sepal.Width,
                     color=Species)) + geom_point() + geom_smooth()
# 5. Añadir más capas aplicando 'aes' a una sola:
ggplot(data=iris,aes(x=Sepal.Length, y= Sepal.Width)) + geom_point() + 
         geom_smooth(aes(color=Species)) + theme (legend.position = "bottom")


# 6. Mapear vs Setear una estética

ggplot(data=iris) + geom_point(aes(x=Sepal.Length, y= Sepal.Width,
                    color='blue'))
ggplot(data = iris) +
  geom_point(aes(x=Sepal.Length, y= Sepal.Width), color = 'blue')


# 7. Simplificando el script (volviéndolo más "elegante"):
p= ggplot(data=iris,aes(x=Sepal.Length, y= Sepal.Width,color=Species)) +
  theme (legend.position = "bottom")
p
p + geom_point() 
p + geom_smooth(aes(color=Species))
p + geom_point() + geom_smooth(aes(color=Species))

# 8. facetado
p + geom_point() +facet_wrap(~Species)+ theme(legend.position = "none")
p + geom_point() +facet_grid(~Species)+ theme(legend.position = "none")
head(Milk)
#densities of protein, colored by Diet, faceted by Time
ggplot(Milk, aes(x=protein, color=Diet)) + 
  geom_density() + 
  facet_wrap(~Time)+ theme(legend.position = "none")

ggplot(Milk, aes(x=protein, color=Diet)) + 
  geom_density() + 
  facet_grid(~Time)+ theme(legend.position = "bottom")
# 9. Boxplot

p= ggplot(data=iris,aes(x=Sepal.Length, y= Sepal.Width,color=Species)) 
p + geom_boxplot() 
#algo no está bien, cierto?
s= ggplot(data=iris,aes(x=Species, y= Sepal.Width))
s + geom_boxplot(aes(fill= Species))+ theme(legend.position = "none")

# 10. Sistema de coordenadas

s + geom_boxplot(aes(fill= Species))+ coord_flip()+ theme(legend.position = "none")


# 11. Stats

ggplot(iris, aes(x=Sepal.Length)) +  stat_bin()

ggplot(data=iris,aes(x=Sepal.Length, y= Sepal.Width)) + 
  stat_summary()
 
ggplot(data=iris,aes(x=Sepal.Length, y= Sepal.Width))+ 
  stat_summary(fun.data="mean_cl_boot")

# 12. Escalas

W= ggplot(data=iris,aes(x=Sepal.Length, y= Sepal.Width)) + 
  geom_point(aes(colour=Petal.Width))

W+ scale_colour_distiller(palette = "Oranges")

W+ scale_colour_gradient(low= "orange", high= "red")

W + scale_y_reverse()

W + scale_colour_gradient(low= "orange", high= "red")+
  scale_shape_manual(values=c(3, 16, 17))
  
ggplot(data=iris,aes(x=Sepal.Length, y= Sepal.Width)) + 
  geom_point(aes(colour=Petal.Width))+ scale_color_viridis()

ggplot(iris, aes(x=Sepal.Length, color=Species)) + 
  geom_density() + scale_color_brewer(palette="Dark2")

# 13. Position
df <- diamonds %>% #dataset de ejemplo
  filter(color %in% c("J", "D")) %>%
  group_by(cut, color) %>%
  summarise(counts = n()) 
head(df, 4)

# Gráfico de barras apiladas de y = counts por x = cut, 
# coloreados por la variable 'color'

ggplot(df, aes(x = cut, y = counts)) +
  geom_bar(aes(color = color, fill = color),
           stat = "identity", position = position_stack()) 


# position = position_dodge() 
ggplot(df, aes(x = cut, y = counts)) +
  geom_bar(aes(color = color, fill = color),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7) +
  scale_color_manual(values = c("#0073C2FF", "#EFC000FF"))+
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF"))

# 14. Themes
ggplot(iris, aes(x=Sepal.Length)) + stat_bin() + theme_classic()

# ejemplo tomado de "http://www.sthda.com/english/articles/
# 32-r-graphics-essentials/132-plot-grouped-data-
# box-plot-bar-plot-and-more/"


ggplot(iris, aes(x=Sepal.Length)) + stat_bin()
ggplot(iris, aes(x=Sepal.Length)) + stat_bin() + theme_classic()

#Establecer un formato pre-armado estandar para publicacion
theme_set(theme_pubclean())

#armar tu propio tema!
rladies_theme <- theme_bw() +
  theme(
    axis.text = element_text(colour = "white"), #color del texto en ejes
    axis.text.x = element_text(angle = 90), # inclinación de los ejes
    text = element_text(family = "Palatino", size = 14), #tipo y tamaño de letra (general)
    plot.background = element_rect(fill="#88398a", colour=NA), #fondo y márgenes
    panel.border = element_rect(colour = 'grey80'), #borde del recuadro
    panel.grid.minor = element_blank(),#grilla interna
    legend.position = 'bottom'#ubicación de la leyenda
  )
theme_set(rladies_theme) #establezco el tema general

ggplot(iris, aes(x=Sepal.Length)) + stat_bin()

#a mi no me reconocía las familias de fuentes así que hice esto:
#library(extrafont)
#font_import()
# loadfonts(device = "win") ojo las que tienen mac!

# 15. Guardar el gráfico

ggsave("grafico1.pdf", width = 20, height = 20, units = "cm")

#armar una grilla de gráficos antes de guardarlos

d<-ggplot(data=iris,aes(x=Sepal.Length, y= Sepal.Width,
  color=Species)) + geom_point() 

f<-ggplot(data=iris,aes(x=Sepal.Length, y= Sepal.Width)) + geom_point() + 
  geom_smooth(aes(color=Species))+ theme(legend.position = 'bottom')

combo<- grid.arrange(d, f, ncol = 2, widths = c(9, 9))
combo

ggsave("plot1ayb.pdf", combo, width = 20, height = 20, units = "cm")

# 16. series temporales

head(economics)
help(economics)
#psavert= tasa de ahorros personales
#pce= gastos de consumo personal en billones de dólares (¡¡¿¿??!!)
#unemploy= número de desempleados en miles
# uempmed= duración media del desempleo en semanas
#pop= población total en miles

# Gráfico de línea básico
ggplot(data = economics, aes(x = date, y = pop))+
  geom_line(color = "#00AFBB", size = 2)

# Graficar un subset de los datos
ss <- subset(economics, date > as.Date("2006-1-1"))

ggplot(data = ss, aes(x = date, y = pop)) + 
  geom_line(color = "#FC4E07", size = 2)

ggplot(data = economics, aes(x = date, y = pop)) +
  geom_line(aes(size = unemploy/pop), color = "#FC4E07")

# Graficar multiples series temporales

#Agrupar los valores de psavert y uempmed en la misma columna (nueva). 
df <- economics %>%
  select(date, psavert, uempmed) %>%
  gather(key = "variable", value = "value", -date)
head(df, 3)

# Multiple line plot
ggplot(df, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_bw() + theme(axis.text.x = element_text(angle = 45))+
  theme(legend.position = 'bottom')

# Area plot
ggplot(df, aes(x = date, y = value)) + 
  geom_area(aes(color = variable, fill = variable), 
            alpha = 0.5, position = position_dodge(0.8)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))

# 17. gráficos animados

#https://gganimate.com/articles/gganimate.html

#gráfico interactivo con plotly

dataz<- gapminder %>% 
  filter(continent== 'Americas')
#miro los datos
head(dataz)
#gráfico
g <- ggplot(dataz) +
  aes(x = factor(year), y = lifeExp, color = country) +
  geom_point() +
  geom_line(aes(group = country))
# facetado por país
g + facet_wrap(~ country) +  labs(x = "Año", y = "Esperanza de vida", title = "Gráfico Americas", caption = "RLadiesBRC")

#lo transformo en un gráfico plotly
ggplotly(g)

#gráfico animado 1
h<- gapminder %>% 
  ggplot(aes(x=gdpPercap, y=lifeExp,color=continent)) +
  geom_point(aes(size=gdpPercap))

h+ transition_states(year, 1, 0) + 
  ggtitle("{closest_state}")
anim_save("animado.gif")

#gráfico animado 2

j<- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = TRUE) +
  scale_colour_manual(values = country_colors) + 
  scale_size(range = c(2, 12)) + scale_x_log10() + 
  facet_wrap(~continent)
 j+  labs(title = 'Año: {frame_time}', x = 'GDP per capita', y = 'Esperanza de vida', caption = "RLadiesBRC") +
 # Here comes the gganimate specific bits 
  transition_time(year) + ease_aes('linear') 
help("ease_aes")
anim_save("wraped.gif")
#tomado de https://github.com/thomasp85/gganimate


