library(sp)
library(raster)
library(maptools)
library(maps)
library(dismo)
library(rgdal)
library(ggmap)
library(ggplot2)
library(rgbif)

sp_1 <- gbif("Liolaemus","darwinii")
dim(sp_1)
names(sp_1)
sp1_points <- sp_1[,c("lon","lat")]
head(sp1_points)
phyma <- occ_data(scientificName = 'Phymaturus', hasCoordinate = TRUE)


library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(rgbif)

sp_1 <- gbif("Liolaemus","darwinii")
dim(sp_1)
names(sp_1)
sp1_points <- sp_1[,c("lon","lat")]
head(sp1_points)
# compute the mean lat and lon

library(rworldmap)
map <- getMap(resolution = "low")#para high necesito el otro paquete
plot(map, xlim = c(-77,-55), ylim = c(-57,-21), asp = 1)
#otro sistema... probando
base = get_map(location=c(-77,-57,-55,-21), zoom=7, maptype="terrain-background")
map1 = ggmap(base)
map1
map1 + geom_point(data=sp1_points, aes(x=lon, y=lat), color="orange", cex=1) + # plot the points
  scale_fill_manual(values = "#59A044", labels="Liolaemus darwinii") +
  scale_shape_manual(values = 15) + # define shape/color scales
  labs(x="Latitude", y="Longitude", title="Registros de presencia") + # label the axes
  theme_bw() + theme(legend.position="none", axis.text = element_text(size = rel(0.75)), axis.text.x = element_text(angle=45, vjust=0.5)) # tweak the plot's appearance and legend position
