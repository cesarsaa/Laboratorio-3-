### Para la Varianza ###

B <- 5000 ## número de experimentos
mu <- 5
sd <- 1
coeficiente<- 0.95
alpha <- 1-coeficiente

#---Para n=10---#
n1 <- 10 ## tamaño de muestra

muestras1 <- replicate(B, rchisq(n1, mu, sd))

calcula.el.intervalo1 <- function(columna) {
  
  m1 <- muestras1[, columna]
  
  lim.inf1 <- (n1-1)*(sd(m1)) / qchisq(1-alpha/2,n1-1)
  lim.sup1 <- (n1-1)*(sd(m1)) / qchisq(alpha/2,n1-1)
  
  c(lim.inf1, lim.sup1)
}

#Cálculo de los nuevos intervalos#
c1=0
mis.intervalos1 <- matrix(rep(0, 10000), nrow = 2) ## matriz de ceros

for(i in 1:5000) {
  mis.intervalos1[,i] <- calcula.el.intervalo1(i)
  if (mis.intervalos1[1, i] <= mu && mis.intervalos1[2, i] >= mu){
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

#---Para n=10---#
n1 <- 10 ## tamaño de muestra

muestras1 <- replicate(B, rchisq(n1, mu, sd))

calcula.el.intervalo1 <- function(columna) {
  
  m1 <- muestras1[, columna]
  
  lim.inf1 <- (n1-1)*(sd(m1)) / qchisq(1-alpha/2,n1-1)
  lim.sup1 <- (n1-1)*(sd(m1)) / qchisq(alpha/2,n1-1)
  
  c(lim.inf1, lim.sup1)
}

#Cálculo de los nuevos intervalos#
c1=0
mis.intervalos1 <- matrix(rep(0, 10000), nrow = 2) ## matriz de ceros

for(i in 1:5000) {
  mis.intervalos1[,i] <- calcula.el.intervalo1(i)
  if (mis.intervalos1[1, i] <= mu && mis.intervalos1[2, i] >= mu){
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


tabla_cv<-c(Porcentaje1,Porcentaje2,Porcentaje3,Porcentaje4)
tabla_sd<-c(longitudpro1,longitudpro2,longitudpro3,longitudpro4)
tabla_cs<-c(n1,n2,n3,n4)

### Graficos ###
plot(tabla_cs, tabla_cv, type="b", main="Porcentaje de cubrimiento esperado para n=10, 30, 50, 100", 
     xlab="tamaño de la muestra", ylab="porcentaje")
plot(tabla_cs, tabla_sd, type="b", main="Longitud promedio para n=10, 30, 50, 100", 
     xlab="tamaño de la muestra", ylab="Longitud")

