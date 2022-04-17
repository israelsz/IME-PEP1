# Ejercicio práctico N°6

# Grupo N°2
# Integrantes:
# Christofer Rodriguez - Christian Méndez  - Israel Arias

#Importación de paquetes

if(!require(Hmisc)){
  install.packages("Hmisc",dependencies = TRUE)
  require(Hmisc)
}

#########################################################################
# 1. Estudios previos habían determinado que la proporción de autoras   #
# en la especialidad de obstetricia era de                              #
# 59%. ¿Respaldan estos datos tal estimación?                           #
#########################################################################

# Se comprueba la primera condición al poseer observaciones independientes
# Se comprueba la segunda condición de éxito-fracaso, ya que poseemos al menos 
# 10 observaciones correspondientes a éxito y al menos 10 correspondientes a fracaso
# Por lo que podemos utilizar la prueba de Wald o Wilson

# Las hipótesis para responder a esta pregunta serán las siguientes:

# H0: La proporción de autoras de la especialidad de obstetricia es de 59%
# Ha: La proporción de autoras de la especialidad de obstetricia no es 59%

# H0: p = 0.59 (Hipótesis nula)
# Ha: p != 0.59 (Hipótesis alternativa)


# Fijar valores conocidos
n <- 137 #Cantidad total de autores de artículos de obstetricia
p_exito <- 71/137 #Probabilidad de escoger a una autora mujer
alfa <- 0.05  #Nivel de significación
valor_nulo <- 0.59 # Valor nulo dado por estudio previo

# La prueba por utilizar para poder contrastar las hipótesis es la prueba de Wilson 
# para la diferencia entre dos proporciones

# Calcular cantidad de éxitos.
exitos <- p_exito * n
 
# Prueba de Wilson para una proporción en R.
prueba <- prop.test (exitos , n = n , p = valor_nulo ,
                            alternative = "two.sided", conf.level = 1 - alfa )

print(prueba)

# el valor de p = 0.1051 es mayor a alfa 0.05, entonces se falla en rechazar
# la hipótesis nula. En conclusión, los datos si respaldan que la proporción
# de autoras en la especialidad de obstetricia es de 59%.

#########################################################################
# 2. Según estos datos, ¿es igual la proporción de autoras en las áreas #
# de neurología y obstetricia?                                          #
#########################################################################

# Se comprueba la primera condición, ya que cada muestra por separado sigue el 
# modelo normal (observaciones independientes, condición de éxito-fracaso)
# Al poseer dos muestras independientes entre sí, por lo que se comprueba la segunda condición
# Por lo que podemos utilizar la prueba de Wald o Wilson

# Las hipótesis para responder a esta pregunta serán las siguientes:

# H0: La proporción de autoras de las áreas de neurología y obstetricia son iguales
# Ha: La proporción de autoras de las áreas de neurología y obstetricia son distintos

# H0: p1 - p2 = 0 (Hipótesis nula)
# Ha: p1 - p2 != 0 (Hipótesis alternativa)

# Siendo p1 la proporción de autoras del área de neurología y p2 la proporción de
# autoras en el área de obstetricia

# Fijar valores conocidos (neurología ,obstetricia)
n <- c(144, 137)  #Tamaño de la muesta (neurología ,obstetricia)
exitos <- c(56, 71)  #Cantidad de éxitos (neurología ,obstetricia)
alfa <- 0.05  #Nivel de significación

 # Prueba de Wilson para dos proporciones en R.
prueba2 <- prop.test (exitos , n = n , alternative = "two.sided",
                           conf.level = 1 - alfa)
print(prueba2)

# el valor de p = 0.040 es menor a alfa 0.05, entonces rechaza
# la hipótesis nula en favor de la hipótesis alternativa.
# En conclusión, la proporción de autoras en las áreas de neurología
# y obstetricia es distinta.

#########################################################################
# 3. Suponiendo que la diferencia en la proporción de autoras en        #
# la especialidad de psiquiatría y la de neurología es de 0,25.         #
# ¿A cuántos autores deberíamos monitorear para obtener un intervalo    #
# de confianza del 95% y poder estadístico de 75%, si se intenta        #
# mantener aproximadamente la misma proporción de gente estudiada       #
# en cada caso?                                                         #
#########################################################################

# Fijar valores conocidos
alfa = 0.05
poder <- 0.75
#Psiquiatría
p1 <- 30/72
n1 <- 72
#Neurología
n2 <- 144
p2 <- 56 /144

fraction <- n1/(n1+n2)

#Cálculo del tamaño de las muestras utilizando prueba de Wilson con dos muestras
n <- bsamsize(p1, p2, fraction, alfa, poder)
cat('Se deben monitorear a', ceiling(n[[1]]), 'y', ceiling(n[[2]]), "autores para psiquiatría y neurología respectivamente")

# Para mantener la misma proporción con un intervalo de confianza del 95% y un poder estadístico del 75% 
# es necesario monitorear a 3239 autores para psiquiatría y 6478 autores para neurología. 

