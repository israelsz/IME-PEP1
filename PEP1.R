# PEP N°1 - Forma 4
# Alumno: Israel Arias Panez

# 1. Usted ha sido designado asesor del mediador enviado por Latonia, quien
# le ha solicitado responder las siguientes preguntas usando para ello una
# muestra de 400 trabajadores obtenida con la semilla 478.

# a. ¿Es igual la proporción de bajas(abollados y abiertos) en Salsacia y Conservia?

# b. ¿Qué poder estadístico tiene la prueba realizada?

# c. ¿A cúantas latas se debería encuestar para tener un poder estadístico
#   de 80% y una confianza de 95%? (manteniendo las proporciones observadas en la muestra.)

#Importación de paquetes
if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE)
  require(tidyverse)
}

if(!require(pwr)){
  install.packages("pwr",dependencies = TRUE)
  require(pwr)
}


if(!require(Hmisc)){
  install.packages("Hmisc",dependencies = TRUE)
  require(Hmisc)
}

# Se carga el archivo de datos CSV
datos <- read.csv2(file.choose(new = FALSE))
# A continuación, se consigue la muestra de 400 trabajadores
set.seed(478) #Se setea la seed pedida en el enunciado.
muestra <- sample_n(datos, 400)

#####################################################################################
# a. ¿Es igual la proporción de bajas(abollados y abiertos) en Salsacia y Conservia?
#####################################################################################

# Esta pregunta corresponde a una inferencia acerca de la diferencia entre
# dos proporciones. Se puede ocupar el método de Wilson
# para la diferencia entre dos proporciones. Se prefiere este método por su robustez


# Hipótesis
# H0: La proporción de bajas es la misma en Salsacia y Conservia (p1 - p2 = 0).
# HA: La proporción de bajas son distintas en Salsacia y Conservia (p1 - p2 != 0).

# Siendo p1 la proporción de bajas en Salsacia y
# p2 la proporción de bajas en Conservia


#Se filtran los datos separandolos por país
datos_salsacia <- datos %>% filter(Pais == "Salsacia")
datos_conservia <- datos %>% filter(Pais == "Conservia")

#Se seleccionan los datos de abollados y abiertos para Salsacia
abollados_salsacia <- datos_salsacia %>% filter(Estado == "Abollado")
abiertos_salsacia <- datos_salsacia %>% filter(Estado == "Abierto")

#Se seleccionan los datos de abollados y abiertos para Conservia
abollados_conservia <- datos_conservia %>% filter(Estado == "Abollado")
abiertos_conservia <- datos_conservia %>% filter(Estado == "Abierto")

#Se calcula la cantidad de bajas totales tanto para Salsacia y Conservia
numero_bajas_salsacia = as.numeric(count(abollados_salsacia)) + as.numeric(count(abiertos_salsacia))
numero_bajas_conservia = as.numeric(count(abollados_conservia)) + as.numeric(count(abiertos_conservia))

#Se calcula el total de datos
total_salsacia = as.numeric(count(datos_salsacia))
total_conservia = as.numeric(count(datos_conservia))

# Para verificar que es posible aplicar el método de Wilson se comprueban las condiciones
# Se comprueba en primer lugar, la primera condición, ya que cada muestra 
# por separado sigue el modelo normal, las observaciones son independientes,
# al haber sido escogidas aleatoriamente y no guardar correlación entre sí,
# además también se cumple la condición de éxito-fracaso al tener al menos 10 éxitos
# y 10 fracasos. Cosa que es facilmente verificable al observar los datos de la muestra
# Por lo que es posible utilizar la prueba de Wilson.

# Una vez verificadas las condiciones, se procede a aplicar la prueba
# se fija un valor de significación igual a 0.05

alfa = 0.05

prueba_wilson <- prop.test(x = c(numero_bajas_salsacia, numero_bajas_conservia),
                      n = c(total_salsacia, total_conservia),
                      alternative = "two.sided", conf.level = 1-alfa,
                      correct = FALSE)
print(prueba_wilson)


# El valor p obtenido, 0.3618, es mucho mayor que el nivel de significación de
# 0.05, por lo que se falla al rechazar la hipótesis nula. Se concluye entonces,
# con 95% de confianza, que la proporción de bajas en Salsacia y Conservia es la misma.

##############################################################
# b. ¿Qué poder estadístico tiene la prueba realizada?
#################################################################

alfa <- 0.05
# Se calcula el tamaño del efecto
tamaño_efecto <- ES.h (numero_bajas_conservia/total_conservia,numero_bajas_salsacia/total_salsacia)
#Se calcula el poder
prueba_poder <- pwr.2p2n.test(h = tamaño_efecto,
                n1 = numero_bajas_salsacia, 
                n2= numero_bajas_conservia,
                sig.level = alfa,
                power = NULL, 
                alternative ="two.sided")
print(prueba_poder)

# El poder de la prueba estadística realizada es de 0.116812

##############################################################################
# c. ¿A cúantas latas se debería encuestar para tener un poder estadístico
#   de 80% y una confianza de 95%? (manteniendo las proporciones observadas en la muestra.)
######################################################################################

# Fijar valores conocidos
alfa = 0.05
poder <- 0.80
p1 <- numero_bajas_salsacia/total_salsacia
n1 <- total_salsacia
n2 <- total_conservia
p2 <- numero_bajas_conservia/total_salsacia

fraction <- n1/(n1+n2)

#Cálculo del tamaño de las muestras utilizando prueba de Wilson con dos muestras
n <- bsamsize(p1, p2, fraction, alfa, poder)
cat('Se deben encuestar a', ceiling(n[[1]]), 'y', ceiling(n[[2]]), "latas de cada pais para conseguir un poder de 80% con 95% de confianza")
