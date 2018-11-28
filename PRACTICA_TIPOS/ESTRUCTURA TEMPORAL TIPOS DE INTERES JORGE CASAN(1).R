databonos <- read.csv2('C:/Users/Usuario/Desktop/MASTER IN DATA SCIENCE/ACPTIUSD(1).csv', dec = ".", sep = ";")
show(databonos)
View(databonos)
library(tidyverse)
library(dplyr)
library(factoextra)
library(FactoMineR)

databonos2<- databonos[1:949, c(-1, -11)]
databonos2


databonos2 <- databonos$X <- NULL
##Por lo que podemos observar esto es un dataframe, que nos muestra los precios de los diferentes bonos en función de su durabilidad y su tipo de interés.
## Como se puede observar al ser un dataframe de cientos de datos, los cuales, nos van a interesar reducir, una tecnica... componentes principales...definicon.Habrá por ejemplo que analizar la correlación pra interpretarlo demanera correcta.
#Hemos cambiado los valores faltantes por 0, ya que era necesario para realizar el ACP.
##https://www.youtube.com/watch?v=oiR3k9H-7K0
?attach

##Preprocesamiento de los datos(esto implica )

boxplot(databonos) ## Nos hablan de medidads de centralizacion o dispersion que distan de la media, la linea de la caja es el valor de la mediana, cuanto mas separados estén los puntos, nos lo muestra para que sepamos detectarlo y eliminarlo.
summary(databonos)
eeee <- pairs(databonos)##nos permite observar las relaciones entre las variables
eeee

cor(databonos)
databonos2 <- databonos$X <- NULL
cor(databonos, use = 'complete.obs') # comentar los valores de correlacion que son muy cercanos a 1.

head(databonos)

##FALTA LA CORRELACIÓN.PREGUNTAR MAÑANA A FERRO, TIENE SENTIDO realizar el analisis de componentes principales debido a que existe una correlacion entre algunas variables, las cuales depues de una reduccion de las mismas no se pierda valor explicativo.
correlaciondatabonos <- cor(databonos)
#Detección de valores NA, de los valores perdidos.
which(is.na(databonos))#Esto se hace para ver cuantos valores ausentes existen, como están todos definidos podemos continuar con el analisis.

#Discretización, se trata de pasar una variable de tipo numérica a tipo categórica

#2Normalización:Estandarización,la cual consiste escalar los datos, representarlos en la misma magnitud,para que por ejemplo una variable con varianza alta no tenga mayor influencia que el resto en el análisis.

scaledatos <- scale(databonos)#analisis de componentes principales necesita que esté todo en una misma magnitud
scaledatos

##Esfericidad de bartlet
library(psych)
cortest.bartlett(correlaciondatabonos, n = 949) ##La prueba de esfericidad de Bartlett contrasta la hipótesis nula de que la matriz de correlaciones es una matriz identidad, en cuyo caso no existirían correlaciones significativas entre las variables y el modelo factorial no sería pertinente.
## Si Sig. (p-valor) < 0.05 aceptamos H0 (hipótesis nula) > se puede aplicar el análisis.

##KMO es una prueba estadistica la cual es una MSA, medida de adecuacion muestral, definicion(compara los valores de las correlaciones entre las variables y sus correlaciones parciales si el indice Kmo esta proximo a 1 el ACP se puede hacer si es proximo a 0, el ACP no será relevante)
KMO(databonos)## definición de kmo, como se puede observar en el kmo todos con mayores de 0.7, por tanto 
##Análisis de componentes principales. Cuanto más cerca de 1 tenga el valor obtenido del test KMO, implica que la relación entres las variables es alta. Si KMO ≥ 0.9
library(missMDA)
acp <- prcomp(databonos) ## esta nos arroja resultados mas precisos. Nos retorna la dv estandar de cada uno de los componentes principales , los cuales coinciden con los autovalores de los componentes principales, y nos retorna el conjunto de componentes principales
                        ##Utilizamos imputePCA, para eliminar los valores faltantes.
acp

##Cual es la proporcion de varianza explicada por cada de una de estos componente y despues de ello elegir cual es el que nos vamos a quedar

summary(acp) ##Mirar proportion of variance 

#para decidir cuantos componentes nos quedamos, un ejemplo es la regla de kaiser(definir regla)La suma debe de ser menor a 1 evidentemente.
#ahora miramos las varianzas individuales de cada uno.
#Se puede observar que en la varianza acumulada hasta la dim2, existe un 92 de varianza explicada acumulada. Podemos desprendernos de las 8 siguientes dimensiones, ya que solo perdemos el 10% de la información, y nos quedamos solo con 2 variables.


desv_standar <- acp[[1]] ##El primer valor nos da las desviaciones estandar
desv_standar
##Tienen que ser cercanas a 1 pero que pasa que eso es la desviación típica necesitamos la 

varianza <- desv_standar^2
varianza                  ##hay que fijarse en los que sean mayor que 1 , elijo tantos autovectores como autovalores >1 tenga la matriz de correlaciones delas variables.
                          ## miramos la varianza acumulada, y nos quedamos con el 85%, los 2 primeros componentes por que no perdemos capacidad explicativa.

##Gráfico de sedimentacion

sedimentacion <- princomp(databonos, scores = TRUE, cor = TRUE)
plot(sedimentacion, type = 'lines')

CP1 = acp[[2]][,1] ## hay que hacer todos los analisis con el los diferentes componentes. Observamos que los unicos componentes principales mayores que 1, son el 1 y el 2 , por tanto vamos a quedarnos con dichos componentes principales, los cuales, nos explican el 92% de la informacion.
CP1

CP2 = acp[[2]][,3]
CP2

Comp_prin = cbind(CP1,CP2)
Comp_prin
##El componente principal 1 le da mas importancia al deposito a 1 mes, y a los de a partir de 1 año y el CP2 el de 1 mes lo explica peor, el que peor. Y el CP2, explica bien los demás, desde 3 meses hasta 12 meses.


## Los componentes que explica 

##SOlo queda el varimax y ponerlo bonito.

