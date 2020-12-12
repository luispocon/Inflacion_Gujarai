# Crear el directorio donde se encuentra el archivo
#dir_tab_1.3 <- ("C:/Users/Dell Inspiron/Desktop/ECONOMETRIA_EN_R/TABLAS EDITADAS")
setwd(dir_tab_1.3)
getwd()

# Cargar las librerias necesarias para el funcionamiento de las funciones
# si no estan disponibles deben instalarlas
library(ggplot2) # libraria para las imagenes
library(tidyverse)
library(xlsx) # para abrir el archivo en excel
library(lubridate) # libreria para las fechas
library(car)
library(readr)
library(dplyr)
library(data.table)
library(GGally)
library(tidyr)
library(xgboost)
library(pROC)
library(ggthemes)
library(gridExtra)
library(Rcmdr)# esta libreria pertenece al de R commander

# importar los datos a traves 

table_1.3  <-  readxl::read_excel("Table 1.3.xls")

#Calculando la inflación para cada Pais
#mutate lo que hace es crear una variable en la misma matriz
# -lag- toma la función de calcalcula en un periodo atras.

table_1.3 <- table_1.3 %>% mutate(inflacion_USA =(USA/lag(USA)-1)*100, 
                                  inflacion_canada = (Canada/lag(Canada)-1)*100,
                                  inflacion_Japan = (Japan/ lag(Japan)-1)*100,
                                  inflacion_France = (France/lag(France)-1)*100,
                                  inflacion_Germany = (Germany/lag(Germany)-1)*100,
                                  inflacion_italy = (Italy/lag(Italy)-1)*100,
                                  inflacion_UK = (UK / lag(UK)-1 )*100)



#Extrayendo la inflacion los columnas de la inflación, y no incluir el resto de columnas
Inflacion <- table_1.3  [,c(1,9:15)]

#extrayendo el año y la inflacion de cada Pais del vector "Inflacion"
Usa <- Inflacion[,c(1,2)]
Canada <- Inflacion[,c(1,3)]
Japon <- Inflacion[,c(1,4)] 
France <- Inflacion[,c(1,5)]
Germany <-Inflacion[,c(1,6)] 
Italy <- Inflacion [,c(1,7)]


# Agregando el nombre a las columnas de los vectores anteriores
#además se le agrega una columna extra con el nombre de la inflación
# cbin lo que esta haciendo es crear la columna Pais para repetir el numero de veces el nombre del país

colnames(Usa) <- c("year","Inflacion") 
Usa =cbind(Usa,Pais=rep("USA",26))

colnames(Canada) <- c("year","Inflacion")
Canada =cbind(Canada,Pais=rep("Canada",26))

colnames(Japon) <- c("year","Inflacion")
Japon =cbind(Japon,Pais=rep("Japon",26))

colnames(France) <- c("year","Inflacion")
France= cbind(France,Pais=rep("France",26))

colnames(Germany) <- c("year","Inflacion")
Germany=cbind(Germany,Pais=rep("Germany",26))

colnames(Italy) <- c("year","Inflacion")
Italy = cbind(Italy, Pais=rep("Italia",26))

# uniendo la variable por variable como metodo largo, haciendo enfasis
# que hay libreria como tydiverse que ahorra tiempo en esto, pero con el fin de aprender 
# se realiza el metodo largo para comprender como unir

# se utiliza mergerow para unir de dos variables en dos, esta es de la libreria de rcomander
# library(Rcmdr)
# es necesario que el nombre de las columnas coincidan porque al unirlo se colocara por debajo


data_inflacion1 <- mergeRows(Usa,Canada)
data_inflacion2 <- mergeRows(Japon,France)
data_inflacion3 <- mergeRows(Germany,Italy)
data_inflacion4 <- mergeRows(data_inflacion1,data_inflacion2)
data_inflacion5x <- mergeRows(data_inflacion3,data_inflacion4)

#trabajando con dos decimales 
round(data_inflacion5x$Inflacion, digits = 2)

# realizando la grafica, utilizando la libreria de ggplot
charter_inflacionx <- ggplot(data_inflacion5x, aes(x=year,y=Inflacion, group=Pais, colour=Pais))
charter_inflacionx+ geom_line()+ geom_point()+ 
         ggtitle("Inflación de países desarrollados", subtitle = "Ritmo inflacionario: Año 1981-2005")+
         theme_minimal()+
         xlab("Año") + ylab("Ritmo Inflacionario")+
         
         theme(axis.title.x = element_text(colour = "Darkgreen", size = 15),
               axis.title.y = element_text(colour = "Red", size = 15),
               
               axis.text.x = element_text(size = 11),
               axis.text.y = element_text(size = 11),
               
               legend.title = element_text(colour = "Blue", size = 15),
               legend.text = element_text(colour = "Blue", size = 12),
               legend.position = c(1,1),
               legend.justification = c(1,1),
               
               plot.title = element_text(colour = "Blue", size = 15))+
         
         
         geom_point(size=1.2, shape=21, fill="White")+
         
         labs(caption = "Fuente: Elaboración propia con base datos del libro de Gujarate 5ta edición, Tabla 1.3")+
         theme(plot.caption=element_text(size=8, hjust=0.5, face="italic", color="black"))

#Estos pasos son de la manera más larga, usando la libreria tidyverse se puede
#optimizar el codigo, con el fin de saber como funciona cada paso se realiza de esta maner








