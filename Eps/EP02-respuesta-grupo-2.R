library(dplyr)
library(ggpubr)
library(modeest)

# Grupo N°2
# Integrantes:
# Christofer Rodriguez
# Christian Méndez
# Israel Arias

#Actividad 1
datos <- read.csv2("C:\\Users\\yelpa\\Downloads\\EP02 Datos Casen 2017.csv") #Modificar ruta
#Las variables que son relevantes para la realización de la actividad en nuestro caso son sexo de tipo categorica nominal
#y la segunda vatiable a considerar es ytot de tipo númerica discreta.

#Actividad 2

#¿Cómo diría que es el ingreso de los hombres de la RM (simétrico/asimétrico, concentrado/disperso, unimodal/multimodal, etc.)?

#Actividad 3: Discutir y consensuar qué medidas estadísticas (media, mediana, moda, etc.) y qué forma gráfica ayudaría a
#responder la pregunta asignada.

#Respuesta: Las medidas estadísticas que nos servirían para poder responder a la pregunta
#Serían la media, moda y mediana, que permiten conocer la simetria, concentracion y si es uni o multimodal
#Un histograma debería bastar para poder apreciar gráficamente la distribución de los datos.

#Actividad 4. Construir un script en R que produzca los estadísticos y el gráfico seleccionado.
hombresRM <- datos %>% filter(sexo == "Hombre")
#Se pone el ingreso total en función de miles de pesos
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
# los hombres de la región metropolitana, el histograma permite ver gráficamente los datos
# en el cual es posible observar los rangos de ingresos totales en miles de pesos
# y la frecuencia de estas.
#Es posible observar que estos datos son asimétricos, debido a que la mayoría se concentra
# al inicio de la grafica, presentando una asimétria a la izquierda al tener más datos
# a la izquierda de la media.
# También es posible observar que los datos están concentrados, esto queda muy claro 
#al observar el histograma, ya que la gran mayoría se encuentran en el mismo rango de ingreso.
# Por otro lado es unimodal, ya que solo hay una moda que corresponde a 11091 pesos que llama
# la atención al ser datos de menores de edad que tienen ingresos.