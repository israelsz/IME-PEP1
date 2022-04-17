# Grupo N°2

# Integrantes:
# Christofer Rodriguez
# Christian Mendez
# Israel Arias

#Se importa la librería
library(dplyr)

#Actividad 1
datos <- read.csv2("F:\\Descargas\\EP01 Datos Covid.csv",check.names = FALSE)


#2. Importar el conjunto de datos al entorno de programación R y responder las siguientes preguntas:
#  . ¿Qué variables se han cargado?
# R: Se han cargado regiones y cantidad de contagiados por fecha.

#  . ¿Qué tipo tiene cada una de estas variables?
# R: Región: Categorica Nominal.
#    Fechas: Númericas discretas

#  . ¿Qué escala parecen tener estas variables?
# R: Región: Son de escala nominal, debido a que una jerarquía establecida en las regiones.
#   Fechas: Escala de razón, debido a que tiene su origen en un cero que viene siendo que no hay contagiados ese día.

#3. Asegurar que las variables tienen el tipo correcto en la tabla de datos (data frame) cargada
# R: Los datos corresponden al tipo de dato adecuado.

#4. Utilizando herramientas de R (se recomienda usar el paquete dplyr) responda las preguntas asignadas a su grupo.

#Grupo 2:
#  1. ¿Qué día se produjo el mayor número de casos con síntomas en la región de Tarapacá entre el 01-mar-2021 y el 31-ago-2021?

# Seleccionar los datos de Tarapacá
tarapaca <- datos %>% filter ( Region == "Tarapacá")
#Se seleccionan los datos entre las fechas indicadas
tarapacaFechas <- tarapaca %>% select("01-03-2021":"31-08-2021")
# Se busca la columna que tenga el mayor valor, se extrae el nombre de la columna
maxTarapaca <- colnames(tarapacaFechas)[apply(tarapacaFechas,1,which.max)]
#Se imprime el día con mayor número  de casos con sintomas por consola
cat("El día con mayor número de casos con síntomas en la región de Tarapacá es: ",maxTarapaca)


#  2. ¿Cuál fue el total de casos con síntomas para cada mes de este periodo?

#Se realiza la suma de casos con síntomas para cada mes del periodo
sumaMarzo <- rowSums(tarapacaFechas %>% select("01-03-2021": "31-03-2021"))
sumaAbril <- rowSums(tarapacaFechas %>% select("01-04-2021": "30-04-2021"))
sumaMayo <- rowSums(tarapacaFechas %>% select("01-05-2021": "31-05-2021"))
sumaJunio <- rowSums(tarapacaFechas %>% select("01-06-2021": "30-06-2021"))
sumaJulio <- rowSums(tarapacaFechas %>% select("01-07-2021": "31-07-2021"))
sumaAgosto <- rowSums(tarapacaFechas %>% select("01-08-2021": "31-08-2021"))

# Se crea el vector de meses
meses <- c("Marzo 2021", "Abril 2021", "Mayo 2021", "Junio 2021", "Julio 2021", "Agosto 2021")
#Se crea el vector de suma
totales <- c(sumaMarzo, sumaAbril, sumaMayo, sumaJunio, sumaJulio, sumaAgosto)
#Se crea el dataframe resultante
totalesPorMes <- data.frame(meses, totales)
#Se imprime el dataframe por consola
totalesPorMes