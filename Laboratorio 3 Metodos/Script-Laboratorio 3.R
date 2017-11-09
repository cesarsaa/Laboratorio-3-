
# Laboratorio 3 Metodos Estadisticos 

# Diana C. Arias Sinisterra
# Kevin S. Garcia Chica
# Cesar A. Saavedra Vanegas

#--------------- Punto 1 ---------------#
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

mis.intervalos <- matrix(rep(0:5001),nrow=2)

for(i in 1:5000) {
  mis.intervalos[,i] <- intervalo(i)
}

mis.intervalos[ ,1:30]

plot(1:10, type = "n",
     xlim = range(mis.intervalos),
     ylab = "Muestreos")

abline(v = mu, lty = 2) 
for(i in 1:5000) {
  segments(mis.intervalos[1,i], i, mis.intervalos[2,i], i)
}

#mu1 <- muestras[ ,1:30]
#mu2 <- muestras[ ,1:50]
#mu3 <- muestras[ , 1:100]

#lim_inf <- media - cuantil * sqrt(varianza)/sqrt(n)
#lim_sup <- media + cuantil * sqrt(varianza)/sqrt(n)

#para30 <- muchasnormales[1:30]
#para50 <- muchasnormales[1:50]
#para100 <- muchasnormales[1:100]


#--------------- Punto 4 ---------------#
  


#--------------- Punto 7 ---------------#

#---------------------------------------#



