#######################################################
###Analsis de componentes principales: FactorMineR#####
##Creado por Dra. Patricia E. Garcia ##################
##para R-ladies Bariloche##############################

getwd() 
setwd("/home/sofia/Rladies/taller_PCA/")

### 0 Instalación de los paquetes ####
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("corrplot")
install.packages("Hmisc")

### 1 Carga de los paquetes ####

library("FactoMineR")
library("factoextra")
library("corrplot")
library("Hmisc")
### 2 Carga de los datos ####
data=(decathlon2)
data
### 3 Opcional centrar y standarizar los datos ####
# Depende de la naturaleza de lo datos###
#data.stan<-scale((data), center=T)

### 4 Matriz de correlación y el grafico ####
res2 <- rcorr(as.matrix(data [1:10])) # correlacion como matriz
# plot de la matriz (grafica solo las significaticas de nivel sig,=.level)
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank", tl.col="black", tl.srt=45)## grafico

### 5 Analisis de componentes principales ###
res.pca <- PCA(data[1:10], graph = FALSE)
eigenvalues <- res.pca$eig# eigenvalues
head(eigenvalues[, 1:2])

fviz_eig(res.pca, addlabels = TRUE) ## scree plot de los eigenvalues


# Variables#
plot(res.pca, choix="var")
res.pca$var

dimdesc(res.pca)
# Individuos#
plot(res.pca, choix="ind")
### 6 Graficos ###

fviz_pca_biplot(res.pca, label="var", repel=T, invisible = "quali")

fviz_pca_biplot(res.pca, label="var", repel=T, axes=c(2,3),invisible = "quali")

fviz_pca_biplot(res.pca, label="ind", repel=T, invisible = "quali")

fviz_pca_biplot(res.pca, label="ind", repel=T, axes=c(2,3),invisible = "quali")

##Clasificar los datos##

fviz_pca_biplot(res.pca, label="var", repel=T, habillage=data$Competition,invisible = "quali")
fviz_pca_biplot(res.pca, label="var", repel=T, habillage=data$Competition,invisible = "quali", addEllipses = T)

fviz_pca_biplot(res.pca, label="ind", repel=T, habillage=data$Competition,invisible = "quali")
fviz_pca_biplot(res.pca,  geom.ind = "point",
                fill.ind = data$Competition, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = "jco",
                invisible="quali",col.var = "contrib",
                legend.title = list(fill = "Competencia"
                                    ))

### 7 Usando variables cualitativas ###
res.pcaC <- PCA(data, quanti.sup = 11:12, quali.sup = 13)
summary(res.pcaC)


##

























### Bonus Extra Datos faltantes ####
rm(list=ls())# comando para limpiar el entorno
install.packages("missMDA")
library(missMDA)
library(FactoMineR)

data=data(orange) ## base de datos orange
orange
res.pca0<-PCA(na.omit(orange))
nb= estim_ncpPCA(orange, ncp.max=6) # estimo la cantidad de ejes
nb
data.imputed<-imputePCA(orange, ncp=2) ## hago una predicción de los datos faltantes
data.imputed$completeObs## predicciones de los valores faltantes
res.pca <- PCA(data.imputed$completeObs) #PCA sobre los datos completos
