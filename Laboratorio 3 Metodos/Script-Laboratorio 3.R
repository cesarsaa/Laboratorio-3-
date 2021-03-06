
# Laboratorio 3 Metodos Estadisticos 

# Diana C. Arias Sinisterra
# Kevin S. Garcia Chica
# Cesar A. Saavedra Vanegas

#--------------- Punto 1 ---------------#

B <- 5000 ## n??mero de experimentos
mu <- 5
sd <- 1
alpha <- 0.05
cuantil <- qnorm(1-alpha/2)

#---Para n=10---#
n1 <- 10 ## tama??o de muestra

muestras1 <- replicate(B, rnorm(n1, mu, sd))

calcula.el.intervalo1 <- function(columna) {
  
  m1 <- muestras1[, columna]
  
  sem1 <- sd(m1)/sqrt(length(m1))
  
  lim.inf1 <- mean(m1) - cuantil * sqrt(sd)/sqrt(n1)
  lim.sup1 <- mean(m1) + cuantil * sqrt(sd)/sqrt(n1)
  
  c(lim.inf1, lim.sup1)
}

#C??lculo de los nuevos intervalos#
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

#---Para n=30---#
n2 <- 30 ## tama??o de muestra

muestras2 <- replicate(B, rnorm(n2, mu, sd))

calcula.el.intervalo2 <- function(columna) {
  
  m2 <- muestras2[, columna]
  
  sem <- sd(m2)/sqrt(length(m2))
  
  lim.inf2 <- mean(m2) - cuantil * sqrt(sd)/sqrt(n2)
  lim.sup2 <- mean(m2) + cuantil * sqrt(sd)/sqrt(n2)
  
  c(lim.inf2, lim.sup2)
}

#C??lculo de los nuevos intervalos#
c2=0
mis.intervalos2 <- matrix(rep(0, 10000), nrow = 2) ## matriz de ceros

for(i in 1:5000) {
  mis.intervalos2[,i] <- calcula.el.intervalo2(i)
  if (mis.intervalos2[1, i] <= mu && mis.intervalos2[2, i] >= mu){
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
n3 <- 50 ## tama??o de muestra

muestras3 <- replicate(B, rnorm(n3, mu, sd))

calcula.el.intervalo3 <- function(columna) {
  
  m3 <- muestras3[, columna]
  
  sem <- sd(m3)/sqrt(length(m3))
  
  lim.inf3 <- mean(m3) - cuantil * sqrt(sd)/sqrt(n3)
  lim.sup3 <- mean(m3) + cuantil * sqrt(sd)/sqrt(n3)
  
  c(lim.inf3, lim.sup3)
}

#C??lculo de los nuevos intervalos#
c3=0
mis.intervalos3 <- matrix(rep(0, 10000), nrow = 2) ## matriz de ceros

for(i in 1:5000) {
  mis.intervalos3[,i] <- calcula.el.intervalo3(i)
  if (mis.intervalos3[1, i] <= mu && mis.intervalos3[2, i] >= mu){
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

n4 <- 100 ## tama??o de muestra

muestras4 <- replicate(B, rnorm(n4, mu, sd))

calcula.el.intervalo4 <- function(columna) {
  
  m4 <- muestras4[, columna]
  
  sem <- sd(m4)/sqrt(length(m4))
  
  lim.inf4 <- mean(m4) - cuantil * sqrt(sd)/sqrt(n4)
  lim.sup4 <- mean(m4) + cuantil * sqrt(sd)/sqrt(n4)
  
  c(lim.inf4, lim.sup4)
}

#Calculo de los nuevos intervalos#
c4=0
mis.intervalos4 <- matrix(rep(0, 10000), nrow = 2) ## matriz de ceros

for(i in 1:5000) {
  mis.intervalos4[,i] <- calcula.el.intervalo4(i)
  if (mis.intervalos4[1, i] <= mu && mis.intervalos4[2, i] >= mu){
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
plot(tabla_cs, tabla_cv, type="b", main="Porcentaje de cubrimiento esperado para n=10, 30, 50, 100", 
     xlab="tama??o de la muestra", ylab="porcentaje")
plot(tabla_cs, tabla_sd, type="b", main="Longitud promedio para n=10, 30, 50, 100", 
     xlab="tama??o de la muestra", ylab="Longitud")

par(mfrow=c(2,2))
plot(1:100, type = "n",
     xlim = range(mis.intervalos1), main = "Intervalo de confianza Distribucion Normal con n=10",
     ylab = "Muestreos", xlab = "Intervalo")

abline(v = mu, lty = 2, col=("black"))

for(i in 1:100) {
  segments(mis.intervalos1[1,i], i, mis.intervalos1[2,i], i,
           col = ifelse(mis.intervalos1[1, i] < mu & mis.intervalos1[2, i] > mu, "gray", "red"))  
}

plot(1:100, type = "n",
     xlim = range(mis.intervalos2), main = "Intervalo de confianza Distribucion Normal con n=30",
     ylab = "Muestreos", xlab = "Intervalo")

abline(v = mu, lty = 2, col=("black"))

for(i in 1:100) {
  segments(mis.intervalos2[1,i], i, mis.intervalos2[2,i], i,
           col = ifelse(mis.intervalos2[1, i] < mu & mis.intervalos2[2, i] > mu, "gray", "red"))  
}

plot(1:100, type = "n",
     xlim = range(mis.intervalos3), main = "Intervalo de confianza Distribucion Normal con n=50",
     ylab = "Muestreos", xlab = "Intervalo")

abline(v = mu, lty = 2, col=("black"))

for(i in 1:100) {
  segments(mis.intervalos3[1,i], i, mis.intervalos3[2,i], i,
           col = ifelse(mis.intervalos3[1, i] < mu & mis.intervalos3[2, i] > mu, "gray", "red"))  
}

plot(1:100, type = "n",
     xlim = range(mis.intervalos4), main = "Intervalo de confianza Distribucion Normal con n=100",
     ylab = "Muestreos", xlab = "Intervalo")

abline(v = mu, lty = 2, col=("black"))

for(i in 1:100) {
  segments(mis.intervalos4[1,i], i, mis.intervalos4[2,i], i,
           col = ifelse(mis.intervalos4[1, i] < mu & mis.intervalos4[2, i] > mu, "gray", "red"))  
}
#-------------------------------------------------------------#
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
 
#-------------------------------------------------------------#
# Para una Exponencial
n1=c(10,30,50,100)
lim_inf1<- 0
lim_sup1<- 0
lim_inf_V1 <- 0
lim_sup_V1<-0
contadorMedia1<- 0
contadorVarianza1<-0
longitudMedias1<-0
longitudVarianzas1<-0
for(j in 1:length(n1)){
  alpha1<- 0.05
  varianza1 <- 1
  media1<- 5
  medias1<-0
  varianzas1 <- 25
  contadorMedia1[j]<- 0
  contadorVarianza1[j]<-0
  
  for (i in 1:5000){
    
    muestra1 <- rexp (n1[j], rate=5)
    medias1[i]<- mean(muestra1)
    varianzas1[i] <- var(muestra1)
    cuantil1<- qt((1-(alpha1)/2),n1[j]-1)
    lim_inf1[j]<-  medias1[i] - cuantil1 * sqrt(varianzas1[i]) / sqrt(n1[j])
    lim_sup1[j]<- medias1[i] + cuantil1 *  sqrt(varianzas1[i]) / sqrt(n1[j])
    if(media1>=lim_inf1[j] & media1 <=lim_sup1[j])
    { contadorMedia1[j] = contadorMedia1[j] + 1 } 
    longitudMedias1[j] <- mean(lim_sup1[j]-lim_inf1[j])
    lim_inf_V1[j] <- (n1[j] - 1) * varianzas1[i] / qchisq(1 - alpha1 / 2,n1[j] - 1)
    lim_sup_V1[j] <- (n1[j] - 1) * varianzas1[i] / qchisq(alpha1 / 2,n1[j] - 1)
    if(varianza1>= lim_inf_V1[j] & varianza1 <= lim_sup_V1[j])
    {contadorVarianza1[j]= contadorVarianza1[j] + 1  }
    longitudVarianzas1[j]<- mean(lim_sup_V1[j]-lim_inf_V1[j])
  }
}
porcentajeMedias1 <-  contadorMedia1/5000
porcentajeVarianza1<-   contadorVarianza1/5000
porcentajeMedias1
porcentajeVarianza1
longitudMedias1
longitudVarianzas1

#-------- Punto 3.a ----------$
x<-c(12,10,15,8,19,14,12,21,16,11,8,15)
y<-c(11,11,17,9,21,13,16,25,20,18,10,17)
n<-c(1,2,3,4,5,6,7,8,9,10,11,12)
x11()
plot(n, x, type = "b",pch = 19, ylab = "Unidades Vendidas",xlab = "N?mero de negocio", 
     ylim=c(min(x)-1,max(y)+1),main = "Gr?fica de unidades vendidas antes y despues de la publicidad")
lines(n, y, type = "b", col = 2, pch = 19)
legend(x=9.3,y=25.8,c(x="Antes",y="Despues") , 
       col = c(1, 2), lwd = c(2, 2))

#--------------- Punto 4 ---------------#
#Intervalo de confianza 
Ic<-function(alpha,p_proporcion,n){
  b<-(1-(alpha/2))
  z<-qnorm(b,mean=0,sd=1)
  Limite_inferior<-(p_proporcion-z*sqrt(p_proporcion*(1-p_proporcion)/n))
  Limite_superior<-(p_proporcion+z*sqrt(p_proporcion*(1-p_proporcion)/n))
  Ic<-c(Limite_inferior,Limite_superior)
  return(Ic)}

#### Punto b
n <- 40
muestra <- 0
contadorIc<-0

for(j in 1:length(n)){
  alpha<- 0.05
  p <- 0.85
  contadorIc[j]<-0
  
  for (i in 1:5000){
    muestra[i] <-rbinom (5000,n[j],0.85)
    
    pro_Muestra<-(muestra[i]/n[j])
    Intervalo_confianza<- Ic(alpha,pro_Muestra, n[j])
    if(p>= Intervalo_confianza[1] & p <= Intervalo_confianza[2])
    {contadorIc[j]= contadorIc[j] + 1 }
    
  }
  
}
porcentajeConta<- contadorIc/5000
porcentajeConta
Intervalo_confianza
contadorIc

#### Punto c #### para n=10
n <- 10
muestra <- 0
contadorIc<-0

for(j in 1:length(n)){
  alpha<- 0.05
  p <- 0.85
  contadorIc[j]<-0
  
  for (i in 1:5000){
    muestra[i] <-rbinom (5000,n[j],0.85)
    
    pro_Muestra<-(muestra[i]/n[j])
    Intervalo_confianza<- Ic(alpha,pro_Muestra, n[j])
    if(p>= Intervalo_confianza[1] & p <= Intervalo_confianza[2])
    {contadorIc[j]= contadorIc[j] + 1 }
    
  }
  
}
porcentajeConta<- contadorIc/5000
porcentajeConta
Intervalo_confianza
contadorIc

#### para n=20
n <- 20
muestra <- 0
contadorIc<-0

for(j in 1:length(n)){
  alpha<- 0.05
  p <- 0.85
  contadorIc[j]<-0
  
  for (i in 1:5000){
    muestra[i] <-rbinom (5000,n[j],0.85)
    
    pro_Muestra<-(muestra[i]/n[j])
    Intervalo_confianza<- Ic(alpha,pro_Muestra, n[j])
    if(p>= Intervalo_confianza[1] & p <= Intervalo_confianza[2])
    {contadorIc[j]= contadorIc[j] + 1 }
    
  }
  
}
porcentajeConta<- contadorIc/5000
porcentajeConta
Intervalo_confianza
contadorIc

#### para n=30
n <- 30
muestra <- 0
contadorIc<-0

for(j in 1:length(n)){
  alpha<- 0.05
  p <- 0.85
  contadorIc[j]<-0
  
  for (i in 1:5000){
    muestra[i] <-rbinom (5000,n[j],0.85)
    
    pro_Muestra<-(muestra[i]/n[j])
    Intervalo_confianza<- Ic(alpha,pro_Muestra, n[j])
    if(p>= Intervalo_confianza[1] & p <= Intervalo_confianza[2])
    {contadorIc[j]= contadorIc[j] + 1 }
    
  }
  
}
porcentajeConta<- contadorIc/5000
porcentajeConta
Intervalo_confianza
contadorIc

#### para n=50
n <- 50
muestra <- 0
contadorIc<-0

for(j in 1:length(n)){
  alpha<- 0.05
  p <- 0.85
  contadorIc[j]<-0
  
  for (i in 1:5000){
    muestra[i] <-rbinom (5000,n[j],0.85)
    
    pro_Muestra<-(muestra[i]/n[j])
    Intervalo_confianza<- Ic(alpha,pro_Muestra, n[j])
    if(p>= Intervalo_confianza[1] & p <= Intervalo_confianza[2])
    {contadorIc[j]= contadorIc[j] + 1 }
    
  }
  
}
porcentajeConta<- contadorIc/5000
porcentajeConta
Intervalo_confianza
contadorIc

#### para n=100
n <- 100
muestra <- 0
contadorIc<-0

for(j in 1:length(n)){
  alpha<- 0.05
  p <- 0.85
  contadorIc[j]<-0
  
  for (i in 1:5000){
    muestra[i] <-rbinom (5000,n[j],0.85)
    
    pro_Muestra<-(muestra[i]/n[j])
    Intervalo_confianza<- Ic(alpha,pro_Muestra, n[j])
    if(p>= Intervalo_confianza[1] & p <= Intervalo_confianza[2])
    {contadorIc[j]= contadorIc[j] + 1 }
    
  }
  
}
porcentajeConta<- contadorIc/5000
porcentajeConta
Intervalo_confianza
contadorIc

#--------------- Punto 5 ---------------#
x1 <- c(11.23,14.36,8.33,10.5,23.42,9.15,13.47,6.47,12.4,19.38)
x2 <- c(11.27,14.41,8.35,10.52,23.41,9.17,13.52,6.46,12.45,19.35)
n1 <-10
n2 <-10
n <- n1 + n2

media1 <- mean(x1)
media2 <- mean(x2)
des1 <- sd(x1)
des2 <- sd(x2)
desc1 <- des1^2
desc2 <- des2^2
sp <- sqrt(((n1-1)*desc1+(n2-1)*desc2)/(n1+n2-2))

coefi <- 0.98
alpha <- 1-coefi

ic1 <- ((media2)-(media1)) - qt((1-(alpha)/2),18)*sp*sqrt((1/n1)+(1/n2))
ic2 <- ((media2)-(media1)) + qt((1-(alpha)/2),18)*sp*sqrt((1/n1)+(1/n2))

intervalo <- c(ic1,ic2) 
print(intervalo)
 
#--------------- Punto 7 ---------------#
################   Parametros de las distribuciones #######################

k=100               ## Parametro de forma de la gamma
lambda_ga=0.5       ## Parametro de escala de la gamma
media=200           ## Media de la normal y parametro de locacion de laplace
sigma=sqrt(400)     ## Desviacion estandar de la normal
c=200 		    ## Parametro uniforme
d=600 		    ## Parametro uniforme

########################      distribucion normal    ##################################
cv_r=sigma/media    ## Coeficiente de variaci??n
alpha=0.05          ## Trabajamos con un nivel de confianza del 0.05
nsim=5000           ## numero de simulaciones
nsim2=100          ## Numero de remuestras
n<-c(5,seq(10,100,10)) ## Tama??o de muestras

tabla_sd<-matrix(nrow=11,ncol=6)               ## Matriz que almacena los 
colnames(tabla_sd)<-c("L.I 1","C.I 1", "L.I 2" #valores de la longitud media 
                      ,"C.I 2","L.I 3","C.I 3")#y la cobertura media de cada 
rownames(tabla_sd)<-c("5","10","20","30","40"  #intervalo para cada tama??o 
                      ,"50","60", "70","80",   #de muestra para la desviacion
                      "90","100")

tabla_cv<-matrix(nrow=11,ncol=8)               ## Matriz que almacena los 
colnames(tabla_cv)<-c("L.I 1","C.I 1", "L.I 2" #valores de la longitud media 
                      ,"C.I 2","L.I 3","C.I 3",#y la cobertura media de cada
                      "L.I 4", "C.I 4")        #intervalo para cada tama??o 
rownames(tabla_cv)<-c("5","10","20","30",      #de muestra para la desviacion
                      "40","50","60",
                      "70","80","90","100")

dist<-function(n,a,b){    ## Ditribucion a evaluar 
  x<-rnorm(n,a,b)
}

###################### Intevalos para la desviaci??n #########################

for(l in 1:length(n)){
  a=0      
  b=0
  long1=0   ## Vector de longitudes para el estimador 1
  long2=0   ## Vector de longitudes para el estimador 2
  contador1=0
  contador2=0
  p=1/(2*sqrt(n[l]-4)) ##Proporcion para el promedio recortado
  for(m in 1:nsim){
    x<-dist(n[l],media,sigma)   ## Distribucion a evaluar
    
    ##Calcular ro4 #######
    
    for(j in 1:n[l]){    
      b= b+((x[j]-mean(x))^2)  ## Denominador de ro
    }
    for(i in 1:n[l]){
      a=a+(((x[i]-mean(x, trim=p))^4)/(b^2)) #### ro4/n
    }
    r=n[l]*a  ## Ro 4
    
    ##Calcular c
    c=n[l]/(n[l]+qnorm((alpha/2),0,1))
    
    ## Calcular se
    se=c*(sqrt(((r*(n[l]-3))/n[l])/(n[l]-1)))
    
    ##Limites de los intervalos 
    
    #### Intervalo 1
    
    li1=sqrt(exp(log(c*var(x))+(qnorm((alpha/2),0,1)*se))) ## Inferior
    ls1=sqrt(exp(log(c*var(x))+(qnorm((1-(alpha/2)),0,1)*se))) ## Superior
    
    ## Intervalo 2
    
    li2<-sqrt(((n[l]-1)*var(x))/qchisq(1-(alpha/2),n[l]-1)) ## Inferior
    ls2<-sqrt(((n[l]-1)*var(x))/qchisq((alpha/2),n[l]-1)) ## Superior
    
    ## Calculo de la longitud
    
    long1[m]<- ls1-li1 ##Longitud intervalo 1
    long2[m]<- ls2-li2 ## Longitud intervalo 2
    
    if (sigma>=li1 & sigma<=ls1){
      contador1= contador1+1}   
    
    if (sigma>=li2 & sigma<=ls2){
      contador2= contador2+1 }
  } 
  
  ## Almacenamos todos los valores para cada n en un matriz
  
  tabla_sd[l,1]=mean(long1)
  tabla_sd[l,3]=mean(long2)
  tabla_sd[l,2]=contador1/nsim
  tabla_sd[l,4]=contador2/nsim
}

##### Intervalo bootrap para la desviaci??n estandar 

sd_boot=0  ## Vector para la distribcion de la desviaci??n

for(m in 1:length(n)){
  long7=0  ## vector de longitudes
  contador7=0 
  for(l in 1:nsim){
    x<-dist(n[m],media,sigma) ## Muestra de la distribucon a evaluar
    for(j in 1:nsim2){
      mues_boot<-sample(x,n[m],replace=T) ## Muestras con reemplazo
      sd_boot[j]= sd(mues_boot)}
    li7=quantile(sd_boot, (alpha/2))  #limite inferior
    ls7=quantile(sd_boot, 1-(alpha/2)) #limite superior
    
    long7[l]=ls7-li7  ## Calculo de la longitud
    
    if(sigma>=li7 & sigma<=ls7){
      contador7=contador7+1}   
  }
  tabla_sd[m,5]=mean(long7)  ## Longitud promedio
  tabla_sd[m,6]=contador7/nsim ## Cobertura
}

tabla_sd
############ Intervalos de confianza para CV #######################

for(j in 1:length(n)){
  long3=0  ## Vector de longitudes para el intervalo 1
  long4=0  ## Vector de longitudes para el intervalo 2 
  long5=0  ## Vector de longitudes para el intervalo 3
  contador3=0
  contador4=0
  contador5=0
  
  for(i in 1:nsim){ 
    x<-dist(n[j],media,sigma) ## distribucion a evaluar
    
    cv= sd(x)/mean(x)  ## Coeficiente de varianci??n estiamado
    
    ## Inverlo 1
    li3=cv+(qnorm((alpha/2),0,1)*sqrt(1/(n[j]-1))*(cv^2)*(0.5+(cv^2)))
    ls3=cv+(qnorm(1-(alpha/2),0,1)*sqrt(1/(n[j]-1))*(cv^2)*(0.5+(cv^2)))
    
    ## Intervalo 2
    li4=((sqrt((n[j]-1))*cv)/(sqrt(qchisq(1-(alpha/2),n[j]-1))))
    ls4=((sqrt((n[j]-1))*cv)/(sqrt(qchisq((alpha/2),n[j]-1))))
    
    ## Intervalo 3
    li5=cv+(qnorm((alpha/2),0,1)*(sqrt((cv^4)+0.5*(cv^2))/sqrt(n[j])))
    ls5=cv+(qnorm(1-(alpha/2),0,1)*(sqrt((cv^4)+0.5*(cv^2))/sqrt(n[j])))
    
    ## Calculo de la Longitud
    
    long3[i]=ls3-li3
    long4[i]=ls4-li4
    long5[i]=ls5-li5
    
    # Calculo de la Cobertura para cada intervalo 
    if (cv_r>=li3 & cv_r<=ls3){
      contador3= contador3+1}
    
    if (cv_r>=li4 & cv_r<=ls4){
      contador4= contador4+1}
    
    if (cv_r>=li5 & cv_r<=ls5){
      contador5= contador5+1}
  }
  
  tabla_cv[j,1]=mean(long3)
  tabla_cv[j,3]=mean(long4)
  tabla_cv[j,5]=mean(long5)
  tabla_cv[j,2]=contador3/nsim
  tabla_cv[j,4]=contador4/nsim
  tabla_cv[j,6]=contador5/nsim
}

##### Intervalo bootrap para el coeficiente de variaci??n ###############
cv_boot=0

for(m in 1:length(n)){
  cober6=0
  long6=0
  contador6=0
  for(l in 1:nsim){
    x<-dist(n[m],media,sigma)
    for(j in 1:nsim2){
      mues_boot<-sample(x,n[m],replace=T) ## Muestras con reemplazo
      cv_boot[j]= sd(mues_boot)/mean(mues_boot) }
    li6=quantile(cv_boot, (alpha/2))
    ls6=quantile(cv_boot, 1-(alpha/2))
    
    long6[l]=ls6-li6
    
    if(cv_r>=li6 & cv_r<=ls6){
      contador6=contador6+1}   
  }
  tabla_cv[m,7]=mean(long6)
  tabla_cv[m,8]=contador6/nsim
}
####### Graficas ########
par(mfrow=c(1,2))
plot(tabla_sd[,2],main="Cobertura de los intervalos para sd
     en la distribucion Normal",col="red",type="o",
     xaxt='n',ylim=c(0,1),pch=5,xlab="Tama??o muestra",
     ylab="Proporcion de aceptados")
axis(1,1:length(n),n)
lines(tabla_sd[,4],col="blue",type="o")
lines(tabla_sd[,6],type="b")
abline(h=1-alpha,col="green")
legend(5,0.7,c("alpha","estimador 1","estimador 2","Bootstrap"),
       col=c("green","red","blue","black"),lty=c(1,1),cex=0.8)

plot(tabla_sd[,1],main="Longitud de los intervalos para sd
     en la distribucion Normal",col="red",type="o",
     xaxt='n',ylim=c(0,max(tabla_sd[,1])),pch=5,xlab="Tama??o muestra",
     ylab="Longitud")
axis(1,1:length(n),n)
lines(tabla_sd[,3],col="blue",type="o")
lines(tabla_sd[,5],type="b")
legend(5,90,c("estimador 1","estimador 2","Bootstrap"),
       col=c("red","blue","black"),lty=c(1,1),cex=0.8 )

par(mfrow=c(1,2))
plot(tabla_cv[,2],main="Cobertura de los intervalos para CV
     en la distribucion Normal",col="red",type="o",
     xaxt='n',ylim=c(0,1),pch=5,xlab="Tama??o muestra",
     ylab="Proporcion de aceptados")
axis(1,1:length(n),n)
lines(tabla_cv[,4],col="blue",type="o")
lines(tabla_cv[,6],type="b")
lines(tabla_cv[,8], col="purple", type="o")
abline(h=1-alpha,col="green")
legend(5,0.85,c("alpha","estimador 1","estimador 2","estimador 3","Bootstrap"),
       col=c("green","red","blue","purple","black"),lty=c(1,1),cex=0.8)

plot(tabla_cv[,1],main="Longitud de los intervalos para CV
     en la distribucion Normal",col="red",type="o",
     xaxt='n',ylim=c(0,max(tabla_cv)),pch=5,xlab="Tama??o muestra",
     ylab="Longitud")
axis(1,1:length(n),n)
lines(tabla_cv[,3],col="blue",type="o")
lines(tabla_cv[,5],type="b")
lines(tabla_cv[,7], col="purple", type="o")
legend(5,0.85,c("estimador 1","estimador 2","estimador 3","Bootstrap"),
       col=c("red","blue","purple","black"),lty=c(1,1),cex=0.8) 

################ distribucion Gamma #######################
tabla_cv<-matrix(nrow=11,ncol=8)               ## Matriz que almacena los 
colnames(tabla_cv)<-c("L.I 1","C.I 1", "L.I 2" #valores de la longitud media 
                      ,"C.I 2","L.I 3","C.I 3",#y la cobertura media de cada
                      "L.I 4", "C.I 4")        #intervalo para cada tama??o 
rownames(tabla_cv)<-c("5","10","20","30",      #de muestra para la desviacion
                      "40","50","60",
                      "70","80","90","100")
tabla_sd<-matrix(nrow=11,ncol=6)               ## Matriz que almacena los 
colnames(tabla_sd)<-c("L.I 1","C.I 1", "L.I 2" #valores de la longitud media 
                      ,"C.I 2","L.I 3","C.I 3")#y la cobertura media de cada 
rownames(tabla_sd)<-c("5","10","20","30","40"  #intervalo para cada tama??o 
                      ,"50","60", "70","80",   #de muestra para la desviacion
                      "90","100")

alpha=0.05
k=100               ## Parametro de forma de la gamma
lambda_ga=0.5       ## Parametro de escala de la gamma
sigma=sqrt(400) 
cv_g=1/sqrt(100) 
nsim=5000           ## numero de simulaciones
nsim2=100          ## Numero de remuestras
n<-c(5,seq(10,100,10)) ## Tama??o de muestras

for(l in 1:length(n)){
  a=0      
  b=0
  long1=0   ## Vector de longitudes para el estimador 1
  long2=0   ## Vector de longitudes para el estimador 2
  contador1=0
  contador2=0
  p=1/(2*sqrt(n[l]-4)) ##Proporcion para el promedio recortado
  for(m in 1:nsim){
    x<-rgamma(n[l],shape=k,scale=lambda_ga)  ## Distribucion a evaluar
    
    ##Calcular ro4 #######
    
    for(j in 1:n[l]){    
      b= b+((x[j]-mean(x))^2)  ## Denominador de ro
    }
    for(i in 1:n[l]){
      a=a+(((x[i]-mean(x, trim=p))^4)/(b^2)) #### ro4/n
    }
    r=n[l]*a  ## Ro 4
    
    ##Calcular c
    c=n[l]/(n[l]+qnorm((alpha/2),0,1))
    
    ## Calcular se
    se=c*(sqrt(((r*(n[l]-3))/n[l])/(n[l]-1)))
    
    ##Limites de los intervalos 
    
    #### Intervalo 1
    
    li1=sqrt(exp(log(c*var(x))+(qnorm((alpha/2),0,1)*se))) ## Inferior
    ls1=sqrt(exp(log(c*var(x))+(qnorm((1-(alpha/2)),0,1)*se))) ## Superior
    
    ## Intervalo 2
    
    li2<-sqrt(((n[l]-1)*var(x))/qchisq(1-(alpha/2),n[l]-1)) ## Inferior
    ls2<-sqrt(((n[l]-1)*var(x))/qchisq((alpha/2),n[l]-1)) ## Superior
    
    ## Calculo de la longitud
    
    long1[m]<- ls1-li1 ##Longitud intervalo 1
    long2[m]<- ls2-li2 ## Longitud intervalo 2
    
    if (sigma>=li1 & sigma<=ls1){
      contador1= contador1+1}   
    
    if (sigma>=li2 & sigma<=ls2){
      contador2= contador2+1 }
  } 
  
  ## Almacenamos todos los valores para cada n en un matriz
  
  tabla_sd[l,1]=mean(long1)
  tabla_sd[l,3]=mean(long2)
  tabla_sd[l,2]=contador1/nsim
  tabla_sd[l,4]=contador2/nsim
}

##### Intervalo bootrap para la desviaci??n estandar 

sd_boot=0  ## Vector para la distribcion de la desviaci??n

for(m in 1:length(n)){
  long7=0  ## vector de longitudes
  contador7=0 
  for(l in 1:nsim){
    x<-rgamma(n[m],shape=k,scale=lambda_ga) ## Muestra de la distribucon a evaluar
    for(j in 1:nsim2){
      mues_boot<-sample(x,n,replace=T) ## Muestras con reemplazo
      sd_boot[j]= sd(mues_boot)}
    li7=quantile(sd_boot, (alpha/2))  #limite inferior
    ls7=quantile(sd_boot, 1-(alpha/2)) #limite superior
    
    long7[l]=ls7-li7  ## Calculo de la longitud
    if(sigma>=li7 & sigma<=ls7){
      contador7=contador7+1}   
  }
  tabla_sd[m,5]=mean(long7)  ## Longitud promedio
  tabla_sd[m,6]=contador7/nsim ## Cobertura
}

tabla_sd
############ Intervalos de confianza para CV #######################

for(j in 1:length(n)){
  long3=0  ## Vector de longitudes para el intervalo 1
  long4=0  ## Vector de longitudes para el intervalo 2 
  long5=0  ## Vector de longitudes para el intervalo 3
  contador3=0
  contador4=0
  contador5=0
  
  for(i in 1:nsim){ 
    x<-rgamma(n[j],shape=k,scale=lambda_ga) ## distribucion a evaluar
    
    cv= sd(x)/mean(x)  ## Coeficiente de varianci??n estiamado
    
    ## Inverlo 1
    li3=cv+(qnorm((alpha/2),0,1)*sqrt(1/(n[j]-1))*(cv^2)*(0.5+(cv^2)))
    ls3=cv+(qnorm(1-(alpha/2),0,1)*sqrt(1/(n[j]-1))*(cv^2)*(0.5+(cv^2)))
    
    ## Intervalo 2
    li4=((sqrt((n[j]-1))*cv)/(sqrt(qchisq(1-(alpha/2),n[j]-1))))
    ls4=((sqrt((n[j]-1))*cv)/(sqrt(qchisq((alpha/2),n[j]-1))))
    
    ## Intervalo 3
    li5=cv+(qnorm((alpha/2),0,1)*(sqrt((cv^4)+0.5*(cv^2))/sqrt(n[j])))
    ls5=cv+(qnorm(1-(alpha/2),0,1)*(sqrt((cv^4)+0.5*(cv^2))/sqrt(n[j])))
    
    ## Calculo de la Longitud
    
    long3[i]=ls3-li3
    long4[i]=ls4-li4
    long5[i]=ls5-li5
    
    # Calculo de la Cobertura para cada intervalo 
    if (cv_g>=li3 & cv_g<=ls3){
      contador3= contador3+1}
    
    if (cv_g>=li4 & cv_g<=ls4){
      contador4= contador4+1}
    
    if (cv_g>=li5 & cv_g<=ls5){
      contador5= contador5+1}
  }
  
  tabla_cv[j,1]=mean(long3)
  tabla_cv[j,3]=mean(long4)
  tabla_cv[j,5]=mean(long5)
  tabla_cv[j,2]=contador3/nsim
  tabla_cv[j,4]=contador4/nsim
  tabla_cv[j,6]=contador5/nsim
}

##### Intervalo bootrap para el coeficiente de variaci??n ###############
cv_boot=0

for(m in 1:length(n)){
  cober6=0
  long6=0
  contador6=0
  for(l in 1:nsim){
    x<-rgamma(n[m],shape=k,scale=lambda_ga)
    for(j in 1:nsim2){
      mues_boot<-sample(x,n,replace=T) ## Muestras con reemplazo
      cv_boot[j]= sd(mues_boot)/mean(mues_boot) }
    li6=quantile(cv_boot, (alpha/2))
    ls6=quantile(cv_boot, 1-(alpha/2))
    
    long6[l]=ls6-li6
    
    if(cv_g>=li6 & cv_g<=ls6){
      contador6=contador6+1}   
  }
  tabla_cv[m,7]=mean(long6)
  tabla_cv[m,8]=contador6/nsim
}

####### Graficas ########
par(mfrow=c(1,2))
plot(tabla_sd[,2],main="Cobertura de los intervalos para sd
     en la distribucion Gamma",col="red",type="o",
     xaxt='n',ylim=c(0.6,1),pch=5,xlab="Tama??o de muestra",
     ylab="Proporcion de aceptados")
axis(1,1:length(n),n)
lines(tabla_sd[,4],col="blue",type="o")
lines(tabla_sd[,6],type="b")
abline(h=1-alpha,col="green")
legend(5,0.85,c("alpha","estimador 1","estimador 2","Bootstrap"),
       col=c("green","red","blue","black"),lty=c(1,1),cex=0.8)

plot(tabla_sd[,1],main="Longitud de los intervalos para sd
     en la distribucion Gamma",col="red",type="o",
     xaxt='n',ylim=c(0,max(tabla_sd[,1])),pch=5,xlab="Tama??o de muestra",
     ylab="Proporcion de aceptados")
axis(1,1:length(n),n)
lines(tabla_sd[,3],col="blue",type="o")
lines(tabla_sd[,5],type="b")
legend(5,15,c("estimador 1","estimador 2","Bootstrap"),
       col=c("red","blue","black"),lty=c(1,1),cex=0.8)

par(mfrow=c(1,2))
plot(tabla_cv[,2],main="Cobertura de los intervalos para CV
     en la distribucion Gamma",col="red",type="o",
     xaxt='n',ylim=c(0,1),pch=5,xlab="Tama??o de muestra",
     ylab="Proporcion de aceptados")
axis(1,1:length(n),n)
lines(tabla_cv[,4],col="blue",type="o")
lines(tabla_cv[,6],type="b")
lines(tabla_cv[,8], col="purple", type="o")
abline(h=1-alpha,col="green")
legend(5,0.85,c("alpha","estimador 1","estimador 2","estimador 3","Bootstrap"),
       col=c("green","red","blue","purple","black"),lty=c(1,1),cex=0.8)

plot(tabla_cv[,1],main="Longitud de los intervalos para CV
     en la distribucion Gamma",col="red",type="o",
     xaxt='n',ylim=c(0,max(tabla_cv)),pch=5,xlab="Tama??o de muestra",
     ylab="Proporcion de aceptados")
axis(1,1:length(n),n)
lines(tabla_cv[,3],col="blue",type="o")
lines(tabla_cv[,5],type="b")
lines(tabla_cv[,7], col="purple", type="o")
legend(5,0.85,c("estimador 1","estimador 2","estimador 3","Bootstrap"),
       col=c("red","blue","purple","black"),lty=c(1,1),cex=0.8) 

########################     distribucion Uniforme   ##################################
cv_r=sigma/media    ## Coeficiente de variaci??n
alpha=0.05          ## Trabajamos con un nivel de confianza del 0.05
nsim=5000           ## numero de simulaciones
nsim2=100          ## Numero de remuestras
n<-c(5,seq(10,100,10)) ## Tama??o de muestras

tabla_sd<-matrix(nrow=11,ncol=6)               ## Matriz que almacena los 
colnames(tabla_sd)<-c("L.I 1","C.I 1", "L.I 2" #valores de la longitud media 
                      ,"C.I 2","L.I 3","C.I 3")#y la cobertura media de cada 
rownames(tabla_sd)<-c("5","10","20","30","40"  #intervalo para cada tama??o 
                      ,"50","60", "70","80",   #de muestra para la desviacion
                      "90","100")

tabla_cv<-matrix(nrow=11,ncol=8)               ## Matriz que almacena los 
colnames(tabla_cv)<-c("L.I 1","C.I 1", "L.I 2" #valores de la longitud media 
                      ,"C.I 2","L.I 3","C.I 3",#y la cobertura media de cada
                      "L.I 4", "C.I 4")        #intervalo para cada tama??o 
rownames(tabla_cv)<-c("5","10","20","30",      #de muestra para la desviacion
                      "40","50","60",
                      "70","80","90","100")

dist<-function(n,a,b){    ## Ditribucion a evaluar 
  x<-runif(n,a,b)
}

###################### Intevalos para la desviaci??n #########################

for(l in 1:length(n)){
  a=0      
  b=0
  long1=0   ## Vector de longitudes para el estimador 1
  long2=0   ## Vector de longitudes para el estimador 2
  contador1=0
  contador2=0
  p=1/(2*sqrt(n[l]-4)) ##Proporcion para el promedio recortado
  for(m in 1:nsim){
    x<-dist(n[l],c,d)   ## Distribucion a evaluar
    
    ##Calcular ro4 #######
    
    for(j in 1:n[l]){    
      b= b+((x[j]-mean(x))^2)  ## Denominador de ro
    }
    for(i in 1:n[l]){
      a=a+(((x[i]-mean(x, trim=p))^4)/(b^2)) #### ro4/n
    }
    r=n[l]*a  ## Ro 4
    
    ##Calcular c
    c=n[l]/(n[l]+qnorm((alpha/2),0,1))
    
    ## Calcular se
    se=c*(sqrt(((r*(n[l]-3))/n[l])/(n[l]-1)))
    
    ##Limites de los intervalos 
    
    #### Intervalo 1
    
    li1=sqrt(exp(log(c*var(x))+(qnorm((alpha/2),0,1)*se))) ## Inferior
    ls1=sqrt(exp(log(c*var(x))+(qnorm((1-(alpha/2)),0,1)*se))) ## Superior
    
    ## Intervalo 2
    
    li2<-sqrt(((n[l]-1)*var(x))/qchisq(1-(alpha/2),n[l]-1)) ## Inferior
    ls2<-sqrt(((n[l]-1)*var(x))/qchisq((alpha/2),n[l]-1)) ## Superior
    
    ## Calculo de la longitud
    
    long1[m]<- ls1-li1 ##Longitud intervalo 1
    long2[m]<- ls2-li2 ## Longitud intervalo 2
    
    if (sigma>=li1 & sigma<=ls1){
      contador1= contador1+1}   
    
    if (sigma>=li2 & sigma<=ls2){
      contador2= contador2+1 }
  } 
  
  ## Almacenamos todos los valores para cada n en un matriz
  
  tabla_sd[l,1]=mean(long1)
  tabla_sd[l,3]=mean(long2)
  tabla_sd[l,2]=contador1/nsim
  tabla_sd[l,4]=contador2/nsim
}

##### Intervalo bootrap para la desviaci??n estandar 

sd_boot=0  ## Vector para la distribcion de la desviaci??n

for(m in 1:length(n)){
  long7=0  ## vector de longitudes
  contador7=0 
  for(l in 1:nsim){
    x<-dist(n[m],c,d) ## Muestra de la distribucon a evaluar
    for(j in 1:nsim2){
      mues_boot<-sample(x,n[m],replace=T) ## Muestras con reemplazo
      sd_boot[j]= sd(mues_boot)}
    li7=quantile(sd_boot, (alpha/2))  #limite inferior
    ls7=quantile(sd_boot, 1-(alpha/2)) #limite superior
    
    long7[l]=ls7-li7  ## Calculo de la longitud
    
    if(sigma>=li7 & sigma<=ls7){
      contador7=contador7+1}   
  }
  tabla_sd[m,5]=mean(long7)  ## Longitud promedio
  tabla_sd[m,6]=contador7/nsim ## Cobertura
}

tabla_sd
############ Intervalos de confianza para CV #######################

for(j in 1:length(n)){
  long3=0  ## Vector de longitudes para el intervalo 1
  long4=0  ## Vector de longitudes para el intervalo 2 
  long5=0  ## Vector de longitudes para el intervalo 3
  contador3=0
  contador4=0
  contador5=0
  
  for(i in 1:nsim){ 
    x<-dist(n[j],c,d) ## distribucion a evaluar
    
    cv= sd(x)/mean(x)  ## Coeficiente de varianci??n estiamado
    
    ## Inverlo 1
    li3=cv+(qnorm((alpha/2),0,1)*sqrt(1/(n[j]-1))*(cv^2)*(0.5+(cv^2)))
    ls3=cv+(qnorm(1-(alpha/2),0,1)*sqrt(1/(n[j]-1))*(cv^2)*(0.5+(cv^2)))
    
    ## Intervalo 2
    li4=((sqrt((n[j]-1))*cv)/(sqrt(qchisq(1-(alpha/2),n[j]-1))))
    ls4=((sqrt((n[j]-1))*cv)/(sqrt(qchisq((alpha/2),n[j]-1))))
    
    ## Intervalo 3
    li5=cv+(qnorm((alpha/2),0,1)*(sqrt((cv^4)+0.5*(cv^2))/sqrt(n[j])))
    ls5=cv+(qnorm(1-(alpha/2),0,1)*(sqrt((cv^4)+0.5*(cv^2))/sqrt(n[j])))
    
    ## Calculo de la Longitud
    
    long3[i]=ls3-li3
    long4[i]=ls4-li4
    long5[i]=ls5-li5
    
    # Calculo de la Cobertura para cada intervalo 
    if (cv_r>=li3 & cv_r<=ls3){
      contador3= contador3+1}
    
    if (cv_r>=li4 & cv_r<=ls4){
      contador4= contador4+1}
    
    if (cv_r>=li5 & cv_r<=ls5){
      contador5= contador5+1}
  }
  
  tabla_cv[j,1]=mean(long3)
  tabla_cv[j,3]=mean(long4)
  tabla_cv[j,5]=mean(long5)
  tabla_cv[j,2]=contador3/nsim
  tabla_cv[j,4]=contador4/nsim
  tabla_cv[j,6]=contador5/nsim
}

##### Intervalo bootrap para el coeficiente de variaci??n ###############
cv_boot=0

for(m in 1:length(n)){
  cober6=0
  long6=0
  contador6=0
  for(l in 1:nsim){
    x<-dist(n[m],c,d)
    for(j in 1:nsim2){
      mues_boot<-sample(x,n[m],replace=T) ## Muestras con reemplazo
      cv_boot[j]= sd(mues_boot)/mean(mues_boot) }
    li6=quantile(cv_boot, (alpha/2))
    ls6=quantile(cv_boot, 1-(alpha/2))
    
    long6[l]=ls6-li6
    
    if(cv_r>=li6 & cv_r<=ls6){
      contador6=contador6+1}   
  }
  tabla_cv[m,7]=mean(long6)
  tabla_cv[m,8]=contador6/nsim
}
####### Graficas ########
par(mfrow=c(1,2))
plot(tabla_sd[,2],main="Cobertura de los intervalos para sd
     en la distribucion uniforme",col="red",type="o",
     xaxt='n',ylim=c(0,1),pch=5,xlab="Tama??o de muestra",
     ylab="Proporcion de aceptados")
axis(1,1:length(n),n)
lines(tabla_sd[,4],col="blue",type="o")
lines(tabla_sd[,6],type="b")
abline(h=1-alpha,col="green")
legend(5,0.7,c("alpha","estimador 1","estimador 2","Bootstrap"),
       col=c("green","red","blue","black"),lty=c(1,1),cex=0.8)

plot(tabla_sd[,1],main="Longitud de los intervalos para sd
     en la distribucion uniforme",col="red",type="o",
     xaxt='n',ylim=c(0,max(tabla_sd[,1])),pch=5,xlab="Tama??o de muestra",
     ylab="Longitud")
axis(1,1:length(n),n)
lines(tabla_sd[,3],col="blue",type="o")
lines(tabla_sd[,5],type="b")
legend(5,550,c("estimador 1","estimador 2","Bootstrap"),
       col=c("red","blue","black"),lty=c(1,1),cex=0.8 )

par(mfrow=c(1,2))
plot(tabla_cv[,2],main="Cobertura de los intervalos para CV
     en la distribucion uniforme",col="red",type="o",
     xaxt='n',ylim=c(0,1),pch=5,xlab="Tama??o de muestra",
     ylab="Proporcion de aceptados")
axis(1,1:length(n),n)
lines(tabla_cv[,4],col="blue",type="o")
lines(tabla_cv[,6],type="b")
lines(tabla_cv[,8], col="purple", type="o")
abline(h=1-alpha,col="green")
legend(5,250,c("alpha","estimador 1","estimador 2","estimador 3","Bootstrap"),
       col=c("green","red","blue","purple","black"),lty=c(1,1),cex=0.8)

plot(tabla_cv[,1],main="Longitud de los intervalos para CV
     en la distribuci??n uniforme",col="red",type="o",
     xaxt='n',ylim=c(0,max(tabla_cv)),pch=5,xlab="Tama??o de muestra",
     ylab="Longitud")
axis(1,1:length(n),n)
lines(tabla_cv[,3],col="blue",type="o")
lines(tabla_cv[,5],type="b")
lines(tabla_cv[,7], col="purple", type="o")
legend(5,0.85,c("estimador 1","estimador 2","estimador 3","Bootstrap"),
       col=c("red","blue","purple","black"),lty=c(1,1),cex=0.8)


