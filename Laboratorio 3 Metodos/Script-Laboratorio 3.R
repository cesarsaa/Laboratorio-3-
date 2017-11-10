
# Laboratorio 3 Metodos Estadisticos 

# Diana C. Arias Sinisterra
# Kevin S. Garcia Chica
# Cesar A. Saavedra Vanegas

#--------------- Punto 1 ---------------#

#---Para n=10---#

B <- 5000 ## número de experimentos
n <- 10 ## tamaño de muestra
mu <- 5
sd <- 1
alpha <- 0.05
cuantil <- qnorm(1-alpha/2)

muestras <- replicate(B, rnorm(n, mu, sd))

calcula.el.intervalo <- function(columna) {
  
  m <- muestras[, columna]
  
  sem <- sd(m)/sqrt(length(m))
  
  lim.inf <- mean(m) - cuantil * sqrt(sd)/sqrt(n)
  lim.sup <- mean(m) + cuantil * sqrt(sd)/sqrt(n)
  
  c(lim.inf, lim.sup)
}

#---Cálculo de los nuevos intervalos---#

mis.intervalos <- matrix(rep(0, 10000), nrow = 2) ## matriz de ceros

for(i in 1:5000) {
  mis.intervalos[,i] <- calcula.el.intervalo(i)
}

#---Gráfico---#

plot(1:5000, type = "n",
     xlim = range(mis.intervalos),
     ylab = "Muestreos")

abline(v = mu, lty = 2, col=("red"))

for(i in 1:5000) {
  segments(mis.intervalos[1,i], i, mis.intervalos[2,i], i,
           col = ifelse(mis.intervalos[1, i] < mu & mis.intervalos[2, i] > mu, "gray", "blue"))  
  #if (mis.intervalos < calcula.el.intervalo()[i] && mis.intervalos > calcula.el.intervalo()[i]) {
      #contador = contador + 1 }
  #else {contador = contador}
}

hist(muestras)


#---Para n=30---#

#---Para n=50---#

#---Para n=100---#

#--------------- Punto 4 ---------------#
  


#--------------- Punto 7 ---------------#

#---------------------------------------#



