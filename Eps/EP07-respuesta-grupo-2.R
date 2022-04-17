# Ejercicio práctico N°7

# Grupo N°2
# Integrantes:
# Christofer Rodriguez - Christian Méndez  - Israel Arias

#Importación de paquetes

if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE)
  require(tidyverse)
}
if(!require(RVAideMemoire)){
  install.packages("RVAideMemoire",dependencies = TRUE)
  require(RVAideMemoire)
}
if(!require(rcompanion)){
  install.packages("rcompanion",dependencies = TRUE)
  require(rcompanion)
}




##########################################################################
# 1. Se ha realizado un estudio acerca de la prevalencia de trastornos del  #
# lenguaje con un grupo de 9 niñas y 11  
# niños de segundo básico. Los datos obtenidos muestran que 10 de los
# niños presentan dificultades, mientras
# que solo 3 de las niñas lo hacen. ¿Existe relación entre el sexo
# y la prevalencia de trastornos del lenguaje?
#########################################################################

# Para poder resolver esta pregunta es necesario
# determinar si existe una relación entre el género
# y la presencia de dificultades en el lenguaje, por lo cual
# la prueba de chi-cuadrado de independencia resulta idónea
# al verificar justamente si dos variables son independientes o están relacionadas.


# Hipótesis:

#H0: Las variables género y dificultad son independientes entre ellas.
#Ha: Las variables género y dificultad están relacionadas.

# Crear tabla de contingencia .
 niño <- c(10 , 1)
 niña <- c(3 , 6)

 tabla <- as.table ( rbind ( niño , niña ))

 dimnames ( tabla ) <- list ( genero = c("niño", "niña") ,
                                dificultad = c("Con dificultades", "Sin dificultades") )


 print ( tabla )

 # Hacer prueba chi - cuadrado de independencia.
 # Se muestra la tabla de frencuencias esperadas.
 prueba <- chisq.test ( tabla )
 cat ("\ nLa prueba internamente calcula los valores esperados :\n")
 esperados <- round ( prueba [["expected"]] , 3)
 print ( esperados )

 cat ("\ nResultado de la prueba :\n")
 print ( prueba )
 
 # No se cumplen las condiciones para realizar la prueba chi-cuadrado,
 # ya que no se cumple las 5 observaciones esperadas en cada grupo.

 # Sin embargo, es posible ocupar alternativamente la prueba de Fisher, 
 # para la cual cumple todas las condiciones, que serían tener muestras
 # independientes, tener menos de 5 observaciones esperadas en un grupo
 # y que las variables sean dicotómicas como en este caso.
 
 dificultad <- c ( rep ( "Con dificultad" , 10) , rep ( "Sin dificultad" , 1)
                   ,rep("Con dificultad",3),rep("Sin dificultad",6) )
 genero <- c ( rep ( "niño" , 11) , rep ( "niña" , 9) )
 datos <- data.frame ( genero , dificultad )
 tabla <- xtabs (~. , datos )
 print ( tabla )
 # Aplicar prueba exacta de Fisher .
 alfa <- 0.05
 prueba <- fisher.test ( tabla , 1 - alfa )
 print(prueba)
 


 # Debido a que el valor de p es igual a 0.017, menor al nivel
 # de significación se puede concluir con 95% que las variables de
 # género y dificultad están relacionadas.


 #################################################################### 
# 2. Siempre tenaz en su lucha para erradicar a los vampiros de la faz
# de la tierra, Van Helsing desea probar una vacuna que, según él,
# causará una grave enfermedad en estos seres una vez que beban la
# sangre de sus víctimas. Para ello, ha almacenado una gran cantidad
# de dosis de su propia sangre, separadas en dos grupos:
#   uno de ellos contiene el químico de la vacuna, mientras el otro
# está completamente limpio. Adicionalmente,
# Van Helsing cuenta con 13 vampiros cautivos, a los que alimentó con
# sangre limpia por una semana. Luego de un periodo de limpieza
# (durante el cual los vampiros fueron alimentados con su dieta normal, por lo que
# eliminaron todo rastro de la sangre de Van Helsing), repitió 
# el experimento con la sangre que contiene la vacuna. 
# Para ambos casos, registró cuántos vampiros enfermaron,
# con los siguientes resultados:
# ▪ 3 vampiros no presentaron enfermedad alguna con ninguna
#     de las dietas de Van Helsing.
# ▪ 2 vampiros enfermaron tras ambas dietas de Van Helsing.
# ▪ 2 vampiros enfermaron con la sangre limpia de Van Helsing, 
#     pero no con la sangre que contiene la vacuna.
# ▪ 6 vampiros enfermaron con la sangre que contiene la vacuna,
#     pero no con la sangre limpia de Van Helsing.
# ¿Es posible decir que la vacuna de Van Helsing causa una enfermedad en los vampiros?
 ###################################################################################
 
# Para poder resolver esta pregunta es necesario
# determinar si existe una relación entre la dieta con vacuna y si los vampiros
# se enferman o no. La prueba de Fisher resulta idónea debido a que las variables
# son dicotómicas entre sí.
 
 
 # Hipótesis:
 
 #H0: Las variables enfermedad y dieta son independientes entre ellas.
 #Ha: Las variables enfermedad y dieta están relacionadas.
 
 
 
 # Construir la tabla de contingencia.
enfermedad <- c(rep ("Sano", 3),rep ("Enfermo", 2) , rep("Enfermo", 2), rep ("Enfermo", 6) )
dieta <- c(rep("Ambas dietas", 3), rep("Ambas dietas", 2), rep("Dieta Limpia", 2) , rep("Dieta con vacuna", 6) )
datos <- data.frame ( enfermedad , dieta )
tabla <- xtabs (~. , datos )
print ( tabla )
 
 #Condición esperados
 prueba <- chisq.test ( tabla )
 cat ("\ nLa prueba internamente calcula los valores esperados :\n")
 esperados <- round ( prueba [["expected"]] , 3)
 print ( esperados )
 # No se cumplen las condiciones para realizar la prueba chi-cuadrado,
 # ya que no se cumple las 5 observaciones esperadas en cada grupo.

 # Sin embargo, es posible ocupar alternativamente la prueba de Fisher, 
 # para la cual cumple todas las condiciones, que serían tener muestras
 # independientes, tener menos de 5 observaciones esperadas en un grupo
 # y que las variables sean dicotómicas como en este caso.


  # Aplicar prueba exacta de Fisher .
alfa <- 0.05
prueba <- fisher.test (tabla , 1 - alfa)
print(prueba)

#Debido a que el valor de p es igual a 0.073 es mayor al nivel de significancia 0.05
#Se falla al rechazar la hipótesis nula. En conclusión, se puede asegurar
#Con 95% de confianza que no hay relación entre las variables
#Para asegurar que la vacuna de Van Helsing causa una enfermedad en los vampiros.

################################################################################
# 3. El 21 de marzo de 2022 se realizó un estudio acerca de la aprobación al
#   presidente Gabriel Boric entre los estudiantes de una prestigiosa
#   universidad a fin de comparar los resultados con los obtenidos en la misma
#  encuesta a nivel nacional, obteniéndose los resultados que se muestran
#  en la tabla. ¿Refleja la opinión estudiantil la percepción del país?
################################################################################

# La prueba que se aplicara para responder a la pregunta es la prueba Chi-cuadrado
# de bondad de ajuste, debido a que permite conocer si la proporción de una muestra
# de una población es o no representativa, en este caso tenemos una muestra(estudiantes) 
# derivados de la población(nación) y se desea saber si la proporción en las respuestas 
# de los estudiantes es representativa al compararla a los resultados de las respuestas de
# la nación. Por lo que la prueba de chi-cuadrado de bondad de ajuste resulta idónea.

#Hipotesis:
#H0: Las proporciones del estudio de aprobación son las mismas para los estudiantes y la nación.
#Ha: Las proporciones del estudio de aprobación son diferentes para los estudiantes y la nación.


# Crear tabla de contingencia.
estudiantes <- c(208, 7, 2)
nacional <- c(5046, 3421, 706)

tabla <- as.table ( rbind ( estudiantes , nacional ) )

dimnames ( tabla ) <- list ( grupo = c("Estudiantes ", "Nacional") ,
                               opinion = c("Aprueba", "Desaprueba", "Ninguna") )

print ( tabla )

# Verificar si se esperan más de 5 observaciones por cada grupo y con esto verificar la 
#segunda condición de aplicación para la prueba de chi-cuadrado.

n_nacional <- sum( nacional )
n_estudiantes <- sum(estudiantes)
proporciones <- round ( nacional / n_nacional , 3)
esperados <- round ( proporciones * n_estudiantes , 3)
print ( esperados )

# Al revisar la tabla, es posible observar que efectivamente hay al menos 5 observaciones
# por grupo, por lo que es posible la aplicación de la prueba de chi-cuadrado de homogeneidad.

# Hacer prueba chi - cuadrado de bondad de ajuste.
prueba <- chisq.test ( tabla , correct = FALSE )
print ( prueba )

#Debido a que el valor de p es muy pequeño = 2.2e-16 siendo mucho menor al nivel de significancia 0.05
#Se rechaza la hipótesis nula en favor de la hipótesis alternativa.
#Es posible concluir con 95% de confianza que las proporciones de aprobación son distintas
#para el grupo de los estudiantes y la nación. Por ende, la opinión estudiantil no refleja la percepción del país.

#####################################################################################################################
# 4. La Facultad de Ingeniería desea saber si existe diferencia significativa en el desempeño
# de los estudiantes en  asignaturas críticas de primer semestre. Para ello, le ha entregado 
# un archivo de datos que, para 3 asignaturas, indica si una muestra de 50 estudiantes 
# aprobó o reprobó. ¿Qué puede concluir la Facultad? Indicación:
# obtenga la muestra a partir del archivo EP07 Datos.csv, usando la semilla 440. Considere un nivel de
# significación α=0,05
#####################################################################################################################

# Se carga el archivo de datos CSV
datos <- read.csv2(file.choose(new = FALSE))

# A continuación, se consigue la muestra de 50 estudiantes
set.seed(440)
muestra <- sample_n(datos, 50)

# Para responder a la pregunta se usará la prueba Q de Cochran,
# debido a que esta prueba permite saber cuándo se tienen más de dos 
# observaciones pareadas, si la proporción de éxito es la misma para todos los grupos o no.
# En este caso la prueba permitiría saber si la proporción de la aprobación es la misma
# para las asignaturas de cálculo, algebra y física lo que permitiría saber si existe
# diferencia significativa en el desempeño de los estudiantes en estas asignaturas, o no.

#Hipótesis:
#H0: la proporción de aprobación entre las asignaturas es la misma.
#Ha: la proporción de aprobación es distinta al menos para una asignatura.

# Cabe destacar que se cumplen las condiciones para aplicar la prueba Q de Cochran,
# ya que la variable de respuesta es dicotómica (Aprobar o Reprobar), la variable 
# independiente es categórica, las observaciones son independientes entre si y el tamaño
# de la muestra multiplicado por la cantidad de niveles de la variable independiente es mayor a 24. 


#Se reemplazan las A por 1
muestra[muestra == "A"] <- 1
#Se reemplazan los R por 0
muestra[muestra == "R"] <- 0


# Llevar matriz de datos a formato largo.
muestra <- muestra %>% pivot_longer ( c ( "Calculo" , "Algebra" , "Fisica" ) ,
                                        names_to = "asignaturas" ,
                                        values_to = "resultado" )

muestra[["Id"]] <- factor(muestra[["Id"]])
muestra[["asignaturas"]] <- factor(muestra[["asignaturas"]])

# Hacer prueba Q de Cochran.
prueba <- cochran.qtest ( resultado ~ asignaturas | Id,
                               data = muestra , alpha = 0.05)

print (prueba)

# La aplicación de la prueba Q de Cochran recién hecha es el procedimiento ómnibus, el cual
# busca saber en este caso si existe al menos una asignatura en la cual haya disparidad en
# los niveles de aprobación del alumnado en comparación a las demás. El valor p conseguido
# de esta prueba es p = 0.2008 lo que significa que se falla en rechazar la hipótesis nula,
# en conclusión, podemos afirmar con 95% de confianza que la proporción de aprobación del 
# alumnado entre las asignaturas Calculo, algebra y física es la misma.

# Como se fallo en rechazar la hipótesis nula no se realizará un procedimiento post-hoc, 
# ya que no es posible hacerlo, porque al no poder rechazar la hipótesis nula, ya no se
# puede continuar con el análisis de los datos. El procedimiento post-hoc solo
# se realiza si en la prueba ómnibus se rechaza la hipótesis nula.
