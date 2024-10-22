library(dplyr)
library(ggpubr)
library(modeest)

# Grupo N�2
# Integrantes:
# Christofer Rodriguez
# Christian M�ndez
# Israel Arias

#Actividad 1
datos <- read.csv2("C:\\Users\\yelpa\\Downloads\\EP02 Datos Casen 2017.csv") #Modificar ruta
#Las variables que son relevantes para la realizaci�n de la actividad en nuestro caso son sexo de tipo categorica nominal
#y la segunda vatiable a considerar es ytot de tipo n�merica discreta.

#Actividad 2

#�C�mo dir�a que es el ingreso de los hombres de la RM (sim�trico/asim�trico, concentrado/disperso, unimodal/multimodal, etc.)?

#Actividad 3: Discutir y consensuar qu� medidas estad�sticas (media, mediana, moda, etc.) y qu� forma gr�fica ayudar�a a
#responder la pregunta asignada.

#Respuesta: Las medidas estad�sticas que nos servir�an para poder responder a la pregunta
#Ser�an la media, moda y mediana, que permiten conocer la simetria, concentracion y si es uni o multimodal
#Un histograma deber�a bastar para poder apreciar gr�ficamente la distribuci�n de los datos.

#Actividad 4. Construir un script en R que produzca los estad�sticos y el gr�fico seleccionado.
hombresRM <- datos %>% filter(sexo == "Hombre")
#Se pone el ingreso total en funci�n de miles de pesos
hombresRMAjustado <- hombresRM %>% mutate(ytot = ytot/1000)

# Histograma para el ingreso total
grafico <- gghistogram ( hombresRMAjustado ,
                       x = "ytot",
                       add = "mean",
                       xlab = "Ingreso total en miles de pesos",
                       ylab = "Frecuencia",
                       color = "blue",
                       fill = "blue")

print (grafico)

#Calcula la media del ingreso total
media <- mean(hombresRM[["ytot"]])
cat("La media es: ",media)

#Calcula la mediana del ingreso total
mediana <- median(hombresRM$ytot)
cat("La mediana es: ",mediana)

#Calcula la moda del ingreso total
moda <- mfv(hombresRM$ytot)
cat("La moda es: ",moda)

#Actividad 5
#Respuesta:
# Cada estadistico nos permite conocer la media, moda y mediana del ingreso total para
# los hombres de la regi�n metropolitana, el histograma permite ver gr�ficamente los datos
# en el cual es posible observar los rangos de ingresos totales en miles de pesos
# y la frecuencia de estas.
#Es posible observar que estos datos son asim�tricos, debido a que la mayor�a se concentra
# al inicio de la grafica, presentando una asim�tria a la izquierda al tener m�s datos
# a la izquierda de la media.
# Tambi�n es posible observar que los datos est�n concentrados, esto queda muy claro 
#al observar el histograma, ya que la gran mayor�a se encuentran en el mismo rango de ingreso.
# Por otro lado es unimodal, ya que solo hay una moda que corresponde a 11091 pesos que llama
# la atenci�n al ser datos de menores de edad que tienen ingresos.