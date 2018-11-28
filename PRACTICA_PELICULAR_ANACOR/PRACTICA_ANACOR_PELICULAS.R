peliculas<-matrix(c(70,45,30,0,35,0,45,30,80,5,0,0,30,20,10), nrow = 5, ncol = 3)
peliculas
rownames(peliculas)<-c("Terror", "Comedia", "Drama", "Accion", "Otras")
colnames(peliculas)<-c("<25", "25-50", ">50")

library(gplots)
tabla.peliculas <- as.table(as.matrix(peliculas))
tabla.peliculas
test.ind= chisq.test(tabla.peliculas)
test.ind
balloonplot(t(tabla.peliculas), main ="Peliculas", xlab ="Edad", ylab="Género",
            label = FALSE, show.margins = FALSE)
library(ggpubr)
library(ggplot2)
library(magrittr)
library("graphics")
mosaicplot(tabla.peliculas, shade = TRUE, las=1, main = "Peliculas")
library(vcd)
library(grid)
assoc(head(tabla.peliculas), shade = T, las=3, main = "Relación películas y edad")
#Vamos con el análisis de correspondencias o ANACOR
library(FactoMineR)
library(factoextra)
library(datasets)
peliculas.ca <- CA(tabla.peliculas, graph = FALSE)
peliculas.ca
summary(peliculas.ca, nb.dec = 2, ncp = 2)
eig= get_eigenvalue(peliculas.ca)
traza= sum(eig[1:2])
chi2 = traza*sum(as.matrix(peliculas)) 
chi2
gdl=(nrow(peliculas) - 1) * (ncol(peliculas) - 1)
gdl #8 GRADOS DE LIBERTAD
p.val = pchisq(chi2, df = gdl, lower.tail = FALSE)
p.val #0 DE P-VALUE
#Identificamos el número de dimensiones a retener
round(peliculas.ca$eig, 2)
fviz_screeplot(peliculas.ca)
fviz_screeplot(peliculas.ca) +
  geom_hline(yintercept=25, linetype=2, color="red")
plot(peliculas.ca, axes=c(1, 2), col.row = "blue", col.col ="red", title = "Relación entre género películas y edades")
fviz_ca_biplot(peliculas.ca) +
  ggtitle("Relación entre género películas y edades")+
  theme_minimal()
filas=get_ca_row(peliculas.ca)
filas
filas$coord[, 1:2]
fviz_ca_row(peliculas.ca, col.row="steelblue", shape.row = 15)+
  ggtitle("Puntuaciones de fila")
plot(peliculas.ca, axes=c(1, 2), col.row = "steelblue", col.col ="red", title = "Relación entre género películas y edades",
     invisible = "col")
filas$contrib
library(corrplot)
corrplot(filas$contrib[, 1:2], is.corr = FALSE)
fviz_contrib(peliculas.ca, choice = "row", axes =1)+
  ggtitle("Contribución de las categorías de fila a la explicación de la Dim 1")
fviz_contrib(peliculas.ca, choice = "row", axes =2)+
  ggtitle("Contribución de las categorías de fila a la explicación de la Dim 2")
fviz_ca_row(peliculas.ca, col.row = "contrib")+
  ggtitle("Contribución de las categorías de fila a la explicación de la Dim 1")
fviz_ca_row(peliculas.ca, col.row = "contrib")+
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=10)+
  ggtitle("Contribución de las categorías de fila a la explicación de la Dim 2")+
  theme_minimal()
fviz_ca_row(peliculas.ca, alpha.row="contrib")+
  theme_minimal()
#Calidad de la representación
round(filas$cos2,4)
apply(filas$cos2, 1, sum)
corrplot(filas$cos2, is.corr = FALSE)
fviz_cos2(peliculas.ca, choice="row", axes=2)+
  ggtitle("Cos2 de las filas en las dimensión 2")+
  ylab("Calidad de la representación - Cos2")
#Representación asimétrica de filas y columnas
fviz_ca_biplot(peliculas.ca)+ 
  ggtitle("Gráfico simétrico de filas y columnas")+
  annotate(geom="text", x=0.125, y=-0.1, label="No podemos relacionar las distancias de filas con columnas", color="black")+
  theme_minimal()
fviz_ca_biplot(peliculas.ca, map ="rowprincipal", arrow = c(TRUE, TRUE))
#Gráficos de contribución
fviz_ca_biplot(peliculas.ca, map ="colgreen",
               arrow = c(TRUE, FALSE))+
  ggtitle("Contribución de la edad a las dimensiones")
fviz_ca_biplot(peliculas.ca, map ="rowgreen",
               arrow = c(FALSE, TRUE))+
  ggtitle("Contribución del género de películas a las dimensiones")



