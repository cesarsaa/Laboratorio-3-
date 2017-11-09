
# Laboratorio 3 Metodos Estadisticos 

# Diana C. Arias Sinisterra
# Kevin S. Garcia Chica
# Cesar A. Saavedra Vanegas

#--------------- Punto 1 ---------------#

#----------Para n=10----------#
e <- 5000
n <- 10
mu <- 5
sd <- 1
alpha <- 0.05
cuantil <- qnorm(1-alpha/2)
muestras <- replicate(e,rnorm(n,mu,sd))


intervalo <- function(columna) {
  m <- muestras[ , columna]
  sem <- sd(m)/sqrt(length(m))
  lim.inf <- mean(m) - cuantil * sqrt(sd)/sqrt(n)
  lim.sup <- mean(m) + cuantil * sqrt(sd)/sqrt(n)
  
  c(lim.inf,lim.sup)
  
}

mis.intervalos <- matrix(rep(1:5000),nrow=2)

for(i in 1:5000) {
  mis.intervalos[,i] <- intervalo(i)
}

mis.intervalos[ ,1:30]

plot(1:10, type = "n", main="Intervalo de confianza para una poblacion Normal(5,1) con n=10",
     xlim = range(mis.intervalos),
     ylab = "Muestreos")

abline(v = mu, lty = 2) 
for(i in 1:5000) {
  segments(mis.intervalos[1,i], i, mis.intervalos[2,i], i)
}

#----------Para n=30----------#

#----------Para n=50----------#

#----------Para n=100----------#


#--------------- Punto 4 ---------------#
  


#--------------- Punto 7 ---------------#
x11()
#---------------------------------------#



