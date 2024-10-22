# Ejercicio pr�ctico N�3

# Grupo N�2
# Integrantes:
# Christofer Rodriguez - Christian M�ndez  - Israel Arias


# Se carga el paquete para graficar
if(!require(ggplot2)){
  install.packages("ggplot2",dependencies = TRUE)
  require(ggplot2)
}

# Se carga el archivo de datos CSV
poblacion <- read.csv2(file.choose(new = FALSE))

#-----------------C�digo Base---------------------------
tama�o <- nrow(poblacion)
ingreso <- as.numeric(poblacion[["ytot"]])
poda <- 0.2
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tama�o.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tama�o.podado )
#------------------------------------------------------

# --------- Actividad 1 ------------------------------
set.seed(590)
#Se generan los datos con distribuci�n normal
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)
#Se imprime por pantalla la distribuci�n normal conseguida
print(ingreso.normal)

#------------- Actividad 2 ---------------------------
#Se calcula la media y desviaci�n de los datos normales
media <- mean(ingreso.normal)
desviacion <- sd(ingreso.normal)
#Se consigue la distribuci�n z usando la formula Z = (x - media) / desviaci�n
distribucionZ <- (ingreso.normal - media)/desviacion
#Se imprime por pantalla la distribuci�n Z conseguida
print(distribucionZ)

#------------- Actividad 3 ---------------------------
#Se construir� la distribuci�n Chi cuadrado a partir de la distribuci�n Z, primero con 5 grados con libertad
#Se inicializa el vector vac�o que contendr� la distribuci�n
chiCuadrado5deg <- vector()
for (i in 1:length(distribucionZ)) {
  #Se toman 5 variables aleatorias dentro de la distribuci�n Z
  numerosRandoms <- sample(distribucionZ, 5)
  #Se elevan al cuadrado estas variables
  numerosRandoms <-  numerosRandoms^2
  #Se suman las variables al cuadrado para conseguir el n�mero en distribuci�n chi cuadrada
  sumaCuadrados <- sum(numerosRandoms)
  #Se almacena el valor conseguido en el vector creado previamente
  chiCuadrado5deg[i] <- sumaCuadrados
}

#De la misma forma se construye la segunda distribuci�n Chi cuadrada, ahora con 9 grados de libertad
chiCuadrado9deg <- vector()
for (i in 1:length(distribucionZ)) {
  #Se toman 5 variables aleatorias dentro de la distribuci�n Z
  numerosRandoms <- sample(distribucionZ, 9)
  #Se elevan al cuadrado estas variables
  numerosRandoms <-  numerosRandoms^2
  #Se suman las variables al cuadrado para conseguir el n�mero en distribuci�n chi cuadrada
  sumaCuadrados <- sum(numerosRandoms)
  #Se almacena el valor conseguido en el vector creado previamente
  chiCuadrado9deg[i] <- sumaCuadrados
}

#------------- Actividad 4 ---------------------------
#Se construye la distribuci�n F usando las dos distribuciones Chi cuadrado
distribucionF <- (chiCuadrado5deg/5)/(chiCuadrado9deg/9)

#------------- Actividad 5 ---------------------------
#Gr�fico para distribuci�n normal usando la librer�a ggplot2
#Se convierte el vector con los datos a dataframe
dnormal <- as.data.frame(ingreso.normal)
#Se crea el gr�fico
graficoNormal <- ggplot(dnormal, aes(x = ingreso.normal)) +
  stat_function(fun = dnorm,
                args = list(mean = mean(dnormal$ingreso.normal),
                            sd = sd(dnormal$ingreso.normal)),
                col = "#1b98e0",
                size = 2)
#Se imprime por pantalla
print(graficoNormal)

#Gr�fico para las dos distribuciones chi cuadrado
dcs1 <- as.data.frame(chiCuadrado5deg)
graficoCs1 <- ggplot(dcs1, aes(x = chiCuadrado5deg)) +
  stat_function(fun = dchisq,
                args = list(df = 5),
                col = "#1b98e0",
                size = 2)
#Se imprime por pantalla
print(graficoCs1)

#Segundo gr�fico chi cuadrado
dcs2 <- as.data.frame(chiCuadrado9deg)
graficoCs2 <- ggplot(dcs2, aes(x = chiCuadrado9deg)) +
  stat_function(fun = dchisq,
                args = list(df = 9),
                col = "#1b98e0",
                size = 2)
#Se imprime por pantalla
print(graficoCs2)

#Gr�fico para distribuci�n F
distF <- as.data.frame(distribucionF)
graficoF<- ggplot(distF, aes(x = distribucionF)) +
  stat_function(fun = df,
                args = list(df1 = 5, df2= 9),
                col = "#1b98e0",
                size = 2)
#Se imprime por pantalla
print(graficoF)

# ------------------ Segunda actividad -------------------------- #
set.seed(699)
n.repeticiones <- 45
ensayo <- function(x)
  ifelse(sample(poblacion[["sexo"]], 1) == "Mujer", 1, 0)
veinte.repeticiones <- sapply(1:n.repeticiones, ensayo)
p <- sum(veinte.repeticiones)/n.repeticiones #probabilidad de exito
# ----------- Actividad 1 ------------------------
# Se fijo la semilla en 699 y el n�mero de repeticiones en 45

#------------ Actividad 2 --------------------------
# n = cantidad de intentos
n <- 45
# k = cantidad exacta de �xitos en n intentos
#p = probabilidad de �xito, en este caso que la encuesta corresponda a una mujer, calculada a partir de los datos
# Se crea vector
distribucionBinomial <- vector()
# Se calcula la distribuci�n binomial
for (k in 0:n) {
  combinatoria <- factorial(n) / (factorial(k) * (factorial(n-k)))
  probUnicoExito <- p^k* ((1-p)^(n-k))
  # Se calcula el valor de X
  resultadoX <- combinatoria*probUnicoExito
  #Se almacena en el vector que contendra la ditribuci�n
  distribucionBinomial[k] <- resultadoX
}

# ---------------- Actividad 3 --------------------------
distribucionGeom <- vector()
for (i in 0:n) {
  #Se calcula seg�n la formula
  resultadoGeom <- ((1-p)^(i-1))*p
  distribucionGeom[i] <- resultadoGeom
}

# --------------- Actividad 4 -------------------------
#Se crea la distribuci�n binomial Negativa
distribucionBinomialN <- vector()
for(k in 1:n){
  #Se calcula seg�n la formula
  combinatoria <- factorial(n-1) / (factorial(k-1) * (factorial((n-k))))
  probExito <- p^k * ((1-p)^(n-k))
  resultadoBinomialN <- combinatoria * probExito
  distribucionBinomialN[k] <- resultadoBinomialN
}

# --------------- Actividad 5 --------------------------
#Gr�fico Distribuci�n Binomial
names(distribucionBinomial) <- 1:n.repeticiones
barplot(distribucionBinomial, main = "Distribuci�n Binomial")

#Gr�fico Distribuci�n Geom�trica
names(distribucionGeom) <- 1:n.repeticiones
barplot(distribucionGeom, main = "Distribuci�n Geom�trica")

#Gr�fico Binomial Negativa
names(distribucionBinomialN) <- 1:n.repeticiones
barplot(distribucionBinomialN, main = "Distribuci�n Binomial Negativa")