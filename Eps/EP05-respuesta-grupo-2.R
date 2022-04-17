# Ejercicio práctico N°5

# Grupo N°2
# Integrantes:
# Christofer Rodríguez - Christian Méndez  - Israel Arias

#Importación de paquetes
if(!require(pwr)){
    install.packages("pwr",dependencies = TRUE)
    require(pwr)
}

if(!require(ggplot2)){
  install.packages("ggplot2",dependencies = TRUE)
  require(ggplot2)
}

if(!require(ggpubr)){
  install.packages("ggpubr",dependencies = TRUE)
  require(ggpubr)
}
###############################################################################
# 1. Si el ingeniero está seguro de que el verdadero volumen medio no puede   #
# ser inferior a 10 litros y piensa rechazar la hipótesis nula cuando la      #
# muestra presente una media mayor a 10,3 litros, ¿cuál es la probabilidad de #
# que cometa un error de tipo I?                                              #
###############################################################################

#Se pide encontrar la probabilidad de que el ingeniero cometa un error de tipo I, esto
#significa que lo que se pide es encontrar el valor del nivel de significación alfa,
#que justamente es lo que cuantifica la probabilidad de cometer un error de tipo I.

#Se registran los valores conocidos
n <- 100 #Cantidad de muestra
desv_est <- 1 #1 litro
media <- 10 #Media de 10 litros
#Se calcula el error estándar
es <- desv_est/sqrt(n)              
#Se fija un valor de secuencia con 4 desviaciones estándar de la media.
x <- seq (9.6, 10.4, 0.01)
#Cálculo de las densidades
y <- dnorm (x , mean = media , sd = es) 

#Se realiza el grafico de la distribución conseguida
normal <- data.frame(x,y)
g <- ggplot (normal, aes (x , y )) + geom_line (color = "red")
g <- g + xlab("Volumen medio [L]")
g <- g + scale_y_continuous(breaks = NULL)
g <- g + scale_x_continuous(breaks = seq(9.7,10.4,0.1))
g <- g + geom_vline ( xintercept = media, linetype = "dashed")

g <- g + geom_area ( data = subset (data.frame(x,y), x > 10.3) ,
                     aes ( y = y ) ,
                     colour = "red",
                     fill = "red",
                     alpha = 0.1)

g <- g + annotate("text", x = 10.35, y = 0.6, label = "Área de rechazo ->")

print(g)


# La probabilidad de cometer un error de tipo I está dada por el valor de alfa que es igual
# a la probabilidad de tomar un valor dentro del área de rechazo de H0, o sea
# alfa = P(x > 10.3)
#Se calcula alfa
probMayor10_3 <- pnorm(10.3, media, es, lower.tail = FALSE)
alfa <- probMayor10_3
#alfa = 0.001349
cat("La probabilidad de cometer un error de tipo I es de:",alfa*100,"%.\n\n")
# La probabilidad de que se cometa un error de tipo I es de 0.001349 dada por alfa, es decir
# que un 0.13% de las veces el ingeniero cometerá un error de tipo I.


###############################################################################
# 2. Si el verdadero volumen medio de los bidones fuera de 10,2 litros,       #
# ¿cuál sería la probabilidad de que el ingeniero, que obviamente             #
#  no conoce este dato, cometa un error de tipo II?                           #
###############################################################################

#Se pide encontrar la probabilidad de que el ingeniero cometa un error de tipo II, esto
#significa que lo que se pide es encontrar el valor de beta, que justamente es la 
#probabilidad de cometer errores de tipo II.

#Se registran los valores conocidos
n <- 100 #Cantidad de muestras
media_nula <- 10 #Media de 10 litros
media_efecto<- 10.2 #Media de 10.2 litros
desv_est <- 1 #1 litro
#Cálculo del error estándar
es <- desv_est/sqrt(n) 

#Se calcula el valor crítico de la región de rechazo
#Ya que el valor de alfa no varía, se utiliza el ya calculado en la pregunta anterior
q_critico<- qnorm(alfa, media_nula, es, lower.tail = FALSE)

#Gráfico teórico
x <- seq (9.7, 10.4, 0.01)
y <- dnorm (x , mean = media_nula , sd = es)
normal <- data.frame(x,y)
g <- ggplot (normal, aes (x , y )) + geom_line (color = "red")
g <- g + xlab("Volumen medio [L]")
g <- g + scale_y_continuous(breaks =  NULL)
g <- g + scale_x_continuous(breaks = seq(9.7,10.4,0.1))


g <- g + geom_area ( data = subset (data.frame(x,y), x > 10.3) ,
                     aes ( y = y ) ,
                     colour = "red",
                     fill = "red",
                     alpha = 0.1)


g <- g + geom_vline ( xintercept = media_nula, linetype = "dashed")

print(g)

#Gráfico verdadero
x1 <- seq (9.7, 10.4, 0.01)
y1 <- dnorm (x1 , mean = media_efecto , sd = es)
normal1 <- data.frame(x1,y1)
g <- g + stat_function(fun = dnorm,
                       args = list(mean = media_efecto, sd = es),
                       colour = "blue", size = 1)

g <- g + geom_area ( data = subset ( data.frame (x1,y1 ) ,x > q_critico ),
                     aes ( x = x1 , y = y1 ),
                     colour = "blue", fill = "blue", alpha = 0.5)
g <- g + geom_vline ( xintercept = media_efecto , linetype = "dashed")

print(g)

#Cálculo del poder 
poder <- pnorm (q_critico,
            mean = media_efecto,
            sd = es,
            lower.tail = FALSE)


#Puesto de que el poder estadístico corresponde a la probabilidad de no cometer un error de tipo II,
#se calcula el valor de beta, probabilidad de cometer un error de tipo II como su complemento
beta <- 1 - poder
#beta = 0.8413447
cat("La probabilidad de cometer un error de tipo II es de:",beta*100,"%.\n\n")
# La probabilidad de que se cometa un error de tipo II es de 0.8413447 dada por beta, es decir
# que un 84,13% de las veces el ingeniero cometerá un error de tipo II.


############################################################################################
# 3. Como no se conoce el verdadero volumen medio, genere un gráfico del poder estadístico #
# con las condiciones anteriores, pero suponiendo que el verdadero volumen medio podría    #
# variar de 10 a 10,5 litros.                                                              #
############################################################################################

#Se registran los valores conocidos
n <- 100 #Cantidad de muestras
media_nula <- 10 #Media de 10 litros
desv_est <- 1 #1 litro
es <- desv_est/sqrt(n) #Error estándar
#Ya que el valor de alfa no varía, se utiliza el ya calculado en la pregunta numero 1

#Se crea un vector con la variación de las medias
varMedias <- seq(10,10.5,0.01)

#Se calcula el efecto
tamano_efecto <- (varMedias - media_nula) / desv_est

#Se calcula el poder
poder <- power.t.test(n = n, delta = tamano_efecto, sd = desv_est, sig.level = alfa, 
                      type="one.sample", alternative = "one.sided")$power

#Se genera el gráfico solicitado.
df_datos <- data.frame(tamano_efecto, poder)
g2 <- ggplot(df_datos, aes(tamano_efecto, poder))
g2 <- g2 + geom_line(colour="blue")
g2 <- g2 + xlab("Tamaño del efecto")
g2 <- g2 + ylab("Poder estadístico")
g2 <- g2 + ggtitle("Poder estadístico v/s Tamaño del efecto")
g2 <- g2 + theme_pubr()
print (g2)

# Es posible apreciar en el grafico que poder crece a medida que aumenta el tamaño de efecto, 
# o sea a más tamaño del efecto hay más probabilidad de no cometer un error de tipo II.
#También al disminuir el tamaño de efecto el poder se aproxima al valor de alfa = 0.0013.


##############################################################################################################
# 4. Considerando un volumen medio de 10 litros, ¿cuántos bidones deberían revisarse para conseguir un poder #
# estadístico de 0,75 y un nivel de significación de 0,05?                                                   #
##############################################################################################################

#Para calcular el tamaño de la muestra necesaria para obtener un poder estadístico de 0.75 y
#nivel de significancia 0.05, utilizamos la función power.t.test, la cual nos entrega el valor
#que dejemos como NULL, en este caso el tamaño de la muestra, entregando el resto de los valores

resultado <- power.t.test ( n = NULL ,
                              delta = media_efecto-media_nula, 
                              sd = desv_est,
                              sig.level = 0.05,
                              power = 0.75,
                              type = "one.sample",
                              alternative = "one.sided")

n <- ceiling (resultado [["n"]]) #136
cat("La cantidad de bidones a revisar es de:",n,"\n\n")

#La cantidad necesaria de bidones que se debe revisar para obtener un poder estadístico de 0.75 y
#nivel de significancia 0.05 es de 136 bidones

######################################################################################################################
# 5. ¿Y si el ingeniero fuese muy exigente y quisiera reducir la probabilidad de cometer un error de tipo I a un 1%  #
#solamente?                                                                                                          #
######################################################################################################################

#Ya que la probabilidad de cometer un error de tipo I corresponde al valor de alfa,
#se modifica el valor de significancia (alfa) a 0.01 y se mantiene el resto de los
#valores para los otros parámetros utilizados en la pregunta anterior, utilizando la misma función.

resultado <- power.t.test ( n = NULL ,
                              delta = media_efecto-media_nula,
                              sd = desv_est,
                              sig.level = 0.01,
                              power = 0.75,
                              type = "one.sample",
                              alternative = "one.sided")

n <- ceiling ( resultado [["n"]]) #228
cat("La cantidad de bidones a revisar para que la probabilidad de cometer un error de tipo I sea de 1% es:", n,"\n\n")

#La cantidad necesaria de bidones que se debe revisar para que la probabilidad de cometer un error de tipo I
#sea de un 1% es de 228 bidones.