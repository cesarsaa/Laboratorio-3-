### Para la Varianza ###

B <- 5000 ## número de experimentos
coeficiente<- 0.95
alpha <- 1-coeficiente

#---Para n=10---#
n1 <- 10 ## tamaño de muestra

muestras1 <- replicate(B, rnorm(n1, 5, 1))

calcula.el.intervalo1 <- function(columna) {
  
  m1 <- muestras1[, columna]
  
  lim.inf1 <- (n1-1)*(var(m1)) / qchisq(1-alpha/2,n1-1)
  lim.sup1 <- (n1-1)*(var(m1)) / qchisq(alpha/2,n1-1)
  
  c(lim.inf1, lim.sup1)
}

#Cálculo de los nuevos intervalos#
c1=0
mis.intervalos1 <- matrix(rep(0, 10000), nrow = 2) ## matriz de ceros

for(i in 1:5000) {
  mis.intervalos1[,i] <- calcula.el.intervalo1(i)
  if (mis.intervalos1[1, i] <= 1 && mis.intervalos1[2, i] >= 1){
    c1= c1+1
  }
  else{
    c1=c1
  }
}

Porcentaje1 <- c1/5000 #Proporcion de intervalos que atrapan al parametro

longitud1 <- numeric(length = B)
for (i in 1:B){
  longitud1[i]=(mis.intervalos1[2,i]-mis.intervalos1[1,i])
  longitudpro1=sum(longitud1)/B
}

#---Para n=30---#
n2 <- 30 ## tamaño de muestra

muestras2 <- replicate(B, rnorm(n2, 5, 1))

calcula.el.intervalo2 <- function(columna) {
  
  m2 <- muestras2[, columna]
  
  lim.inf2 <- (n2-1)*(var(m2)) / qchisq(1-alpha/2,n2-1)
  lim.sup2 <- (n2-1)*(var(m2)) / qchisq(alpha/2,n2-1)
  
  c(lim.inf2, lim.sup2)
}

#Cálculo de los nuevos intervalos#
c2=0
mis.intervalos2 <- matrix(rep(0, 10000), nrow = 2) ## matriz de ceros

for(i in 1:5000) {
  mis.intervalos2[,i] <- calcula.el.intervalo2(i)
  if (mis.intervalos2[1, i] <= 1 && mis.intervalos2[2, i] >= 1){
    c2= c2+1
  }
  else{
    c2=c2
  }
}

Porcentaje2 <- c2/5000 #Proporcion de intervalos que atrapan al parametro

longitud2 <- numeric(length = B)
for (i in 1:B){
  longitud2[i]=(mis.intervalos2[2,i]-mis.intervalos2[1,i])
  longitudpro2=sum(longitud2)/B
}

#---Para n=50---#
n3 <- 50 ## tamaño de muestra


muestras3 <- replicate(B, rnorm(n3, 5, 1))

calcula.el.intervalo3 <- function(columna) {
  
  m3 <- muestras3[, columna]
  
  lim.inf3 <- (n3-1)*(var(m3)) / qchisq(1-alpha/2,n3-1)
  lim.sup3 <- (n3-1)*(var(m3)) / qchisq(alpha/2,n3-1)
  
  c(lim.inf3, lim.sup3)
}

#Cálculo de los nuevos intervalos#
c3=0
mis.intervalos3 <- matrix(rep(0, 10000), nrow = 2) ## matriz de ceros

for(i in 1:5000) {
  mis.intervalos3[,i] <- calcula.el.intervalo3(i)
  if (mis.intervalos3[1, i] <= 1 && mis.intervalos3[2, i] >= 1){
    c3= c3+1
  }
  else{
    c3=c3
  }
}

Porcentaje3 <- c3/5000 #Proporcion de intervalos que atrapan al parametro

longitud3 <- numeric(length = B)
for (i in 1:B){
  longitud3[i]=(mis.intervalos3[2,i]-mis.intervalos3[1,i])
  longitudpro3=sum(longitud3)/B
}

#---Para n=100---#
n4 <- 100 ## tamaño de muestra

muestras4 <- replicate(B, rnorm(n4, 5, 1))

calcula.el.intervalo4 <- function(columna) {
  
  m4 <- muestras4[, columna]
  
  lim.inf4 <- (n4-1)*(var(m4)) / qchisq(1-alpha/2,n4-1)
  lim.sup4 <- (n4-1)*(var(m4)) / qchisq(alpha/2,n4-1)
  
  c(lim.inf4, lim.sup4)
}

#Cálculo de los nuevos intervalos#
c4=0
mis.intervalos4 <- matrix(rep(0, 10000), nrow = 2) ## matriz de ceros

for(i in 1:5000) {
  mis.intervalos4[,i] <- calcula.el.intervalo4(i)
  if (mis.intervalos4[1, i] <= 1 && mis.intervalos4[2, i] >= 1){
    c4= c4+1
  }
  else{
    c4=c4
  }
}

Porcentaje4 <- c4/5000 #Proporcion de intervalos que atrapan al parametro

longitud4 <- numeric(length = B)
for (i in 1:B){
  longitud4[i]=(mis.intervalos4[2,i]-mis.intervalos4[1,i])
  longitudpro4=sum(longitud4)/B
}

tabla_cv<-c(Porcentaje1,Porcentaje2,Porcentaje3,Porcentaje4)
tabla_sd<-c(longitudpro1,longitudpro2,longitudpro3,longitudpro4)
tabla_cs<-c(n1,n2,n3,n4)

### Graficos ###
plot(tabla_cs, tabla_cv, type="b", main="Porcentaje cubrimiento de la varianza para n=10, 30, 50, 100", 
     xlab="tamaño de la muestra", ylab="porcentaje")
plot(tabla_cs, tabla_sd, type="b", main="Longitud de la varianza para n=10, 30, 50, 100", 
     xlab="tamaño de la muestra", ylab="Longitud")

<<<<<<< HEAD
=======

>>>>>>> f7181e9f8ee512ee0259e5b0fae26e9ed96be588
