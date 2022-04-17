# Ejercicio práctico N°4

# Grupo N°2
# Integrantes:
# Christofer Rodriguez - Christian Méndez  - Israel Arias


#Se carga el paquete para graficar
if(!require(ggpubr)){
 install.packages("ggpubr",dependencies = TRUE)
 require(ggpubr)
}

if(!require(dplyr)){
  install.packages("dplyr",dependencies = TRUE)
  require(dplyr)
}


# Se carga el archivo de datos CSV
datos <- read.csv2(file.choose(new = FALSE))

# Preguntas asignadas al grupo 2

###################################################################################################################
#  1. El Comité Olímpico cree que el mejor tiempo medio de los atletas orientales antes de ingresar al programa de
#entrenamiento es inferior a 20,32 segundos. ¿Soportan los datos esta afirmación?
###################################################################################################################

# Se seleccionan los datos solo de atletas orientales
orientales <- datos %>% filter(Raza == "Oriental")

# Las hipótesis para responder a esta pregunta serán las siguientes:

# H0: El tiempo medio de los atletas orientales es igual 20,32 segundos (Hipótesis nula)
# Ha: El tiempo medio de los atletas orientales es menor 20,32 segundos (Hipótesis alternativa)

# H0: μ = 20,32 (Hipótesis nula)
# Ha: μ < 20,32 (Hipótesis alternativa)

# La prueba estadística por utilizar para contrastar las hipótesis planteadas es la prueba T de student, 
# se elige esta prueba debido a que no conocemos la desviación estándar de la población, 
# además de que no poseemos tantos datos en la muestra, por lo que esta prueba parece ideal para este caso.

# A continuación, se verificará que la muestra cumpla los requisitos para aplicar la prueba

# En primer lugar, cabe notar que la muestra cumple la primera condición de la prueba T Student, 
# o sea sus observaciones son independientes entre sí en este caso.


# Se aplica la prueba de Shapiro-Wilk para evaluar la normalidad de la muestra

shapiroPrevio <- shapiro.test (orientales[ ,c("Previo")])
print (shapiroPrevio)

# La prueba de Shapiro arroja un valor de p = 0.48, este valor es mayor al alfa o nivel de significancia 0.05,
# lo que significa que la hipótesis nula no puede ser rechazada, lo que significa que la variable puede estar
# distribuida normalmente 

# Tomando en consideración los resultados de la prueba de Shapiro-Wilk es posible suponer con
# confianza que la población de la muestra sigue una distribución cercana a la normal por lo que se cumple la 
# condición para la aplicación de la prueba T de student.

#Se fija el nivel de significación
alfa <- 0.05

# Se realiza la prueba T de student
prueba <- t.test (orientales[ ,c("Previo")],
                   paired = FALSE,
                   alternative = "less",
                   mu = 20.32,
                   conf.level = 1 - alfa)

print (prueba)


# El valor p=0.002 es muchísimo menor que el nivel de significación = 0.05, por lo que la evidencia
# a favor de Ha es muy fuerte. En consecuencia, se rechaza H0 en favor de Ha.
# Se puede afirmar, con 95 % de confianza, que el mejor tiempo promedio de los atletas orientales antes de
# ingresar al programa de entrenamiento es inferior a 20,32 segundos. Los datos soportan esta afirmación.



###################################################################################################################
#  2. ¿Sugieren los datos que la mejor marca de los atletas negros se reduce en más de 2,27 segundos tras el
#entrenamiento?
###################################################################################################################

# Se seleccionan los datos solo de atletas negros
negros <- datos %>% filter(Raza == "Negra")

# Este es un caso de muestras pareadas, en la cual al igual que para la variable de la pregunta anterior,
# no conocemos la desviación estándar de la población y por la cantidad de datos, además al ser datos pareados
# existe una correspondencia entre las dos observaciones, por lo que parece pertinente la aplicación de la prueba
# de T student en la cual las hipótesis serán propuestas mediante la media de las diferencias entre los tiempos
# registrados de los atletas.

# Las hipótesis son:

# H0: La media de las diferencias en los tiempos de los atletas negros es igual a 2,27 segundos. (Hipótesis nula)
# Ha: La media de las diferencias en los tiempos de los atletas negros es mayor a 2,27 (Hipótesis alternativa)

# μdif = μA - μB donde μA = Previo de atletas negros y μB = Posterior de atletas negros

# H0: μdif = 2,27 (Hipótesis nula)
# Ha: μdif > 2,27 (Hipótesis alternativa)

# Se calcula la diferencia
diferencia <- negros[["Previo"]] - negros[["Posterior"]]

# Se aplica la prueba de Shapiro-Wilk para evaluar la normalidad de la diferencia
shapiroNegros <- shapiro.test(diferencia)
print(shapiroNegros)

# La prueba de Shapiro arroja un valor de p = 0.66, este valor es mayor al alfa o nivel de significancia 0.05,
# lo que significa que la hipótesis nula no puede ser rechazada, lo que significa que la variable puede estar
# distribuida normalmente.


# Tomando en consideración los resultados de la prueba de Shapiro-Wilk es posible suponer con
# confianza que la población de la muestra sigue una distribución cercana a la normal por lo que se cumple la 
# condición para la aplicación de la prueba T de student para datos pareados. Además,
# las observaciones son independientes entre ellas. 

#Se fija el nivel de significación
alfa <- 0.05

# Se realiza la prueba T de student
pruebaNegros <- t.test (x = negros[ ,c("Previo")],
                  y = negros[ ,c("Posterior")],
                  paired = TRUE,
                  alternative = "greater",
                  mu = 2.27,
                  conf.level = 1 - alfa)

print (pruebaNegros)


# El valor p = 0.00283 es muchísimo menor que el nivel de significación = 0.05, por lo que la evidencia
# a favor de Ha es muy fuerte. En consecuencia, se rechaza H0 en favor de Ha.
# Se puede afirmar, con 95 % de confianza, la mejor marca de los atletas negros se redujo en más de 2,27 segundos
# tras el entrenamiento.


###################################################################################################################
#  3. ¿Es posible afirmar que, en promedio, los atletas negros superan a los orientales por 3,08 segundos después #
# del entrenamiento?                                                                                              #
###################################################################################################################

#Al igual que en los casos anteriores, se aplicará la prueba T student, ya que las observaciones
#son pocas e independientes, para este caso la hipótesis será:


# H0: La diferencia entre las medias de los atletas orientales y negros es igual a 3,08 segundos (Hipótesis nula).
# Ha: La diferencia entre las medias de los atletas orientales y negros es mayor a 3,08 segundos (Hipótesis alternativa).

# μdif = μA - μB donde μA = Posterior de atletas orientales y μB = Posterior de atletas negros

# H0: μdif = 3,08 (Hipótesis nula).
# H1: μdif > 3,08 (Hipótesis alternativa).

# Se aplica la prueba de Shapiro-Wilk para evaluar la normalidad de las muestras

shapiroNegrosP <- shapiro.test (negros[ ,c("Posterior")])
print (shapiroNegrosP)

shapiroOrientalesP <- shapiro.test (orientales[ ,c("Posterior")])
print (shapiroOrientalesP)

# La prueba de Shapiro arroja valores de p = 0.15 y p = 0.81, para ambas variables. Estos valores
# son mayores al alfa o nivel de significancia 0.05,
# lo que significa que la hipótesis nula no puede ser rechazada, lo que significa que las variables pueden estar
# distribuidas normalmente.

# Tomando en consideración los resultados de la prueba de Shapiro-Wilk es posible suponer con
# confianza que la población de la muestra sigue una distribución cercana a la normal por lo que se cumple la 
# condición para la aplicación de la prueba T de student para datos pareados. Además,
# las observaciones son independientes entre ellas.


#Se fija el nivel de significación
alfa <- 0.05

# Se realiza la prueba T de student
pruebaOrientalNegro <- t.test (x = orientales[ ,c("Posterior")],
                        y = negros[ ,c("Posterior")],
                        paired = FALSE,
                        alternative = "greater",
                        mu = 3.08,
                        conf.level = 1 - alfa)

print (pruebaOrientalNegro)

# Se calcula la diferencia entre las medias
diferenciaOrientalesNegros <- mean(orientales[ ,c("Posterior")]) - mean(negros[ ,c("Posterior")])
cat("La diferencia entre la media de atleltas orientales y negros es:", diferenciaOrientalesNegros,"[s]")

# El valor p = 0.1408 es mayor que el nivel de significación = 0.05, por lo que la evidencia
# a favor de Ha es débil. En consecuencia, se falla en rechazar H0.
# Se puede afirmar, con 95 % de confianza, que tras el entrenamiento los atletas negros NO superan
# a los atletas orientales por 3,08 segundos.

# Al calcular la diferencia real está es de 3,80 segundos, lo que es poca diferencia comparada
# a 3,08 segundos, es por esto por lo que el valor de p fue mayor al nivel de significación.

