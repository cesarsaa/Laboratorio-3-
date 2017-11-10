
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
c=0
mis.intervalos <- matrix(rep(0, 10000), nrow = 2) ## matriz de ceros

for(i in 1:5000) {
  mis.intervalos[,i] <- calcula.el.intervalo(i)
  if (mis.intervalos[1, i] <= mu && mis.intervalos[2, i] >= mu){
    c= c+1
  }
  else{
    c=c
  }
}

Porcentaje <- c/5000 #Proporcion de intervalos que atrapan al parametro

#---Gráfico---#


plot(1:5000, type = "n",
     xlim = range(mis.intervalos),
     ylab = "Muestreos")

abline(v = mu, lty = 2, col=("red"))

for(i in 1:5000) {
  segments(mis.intervalos[1,i], i, mis.intervalos[2,i], i,
           col = ifelse(mis.intervalos[1, i] < mu & mis.intervalos[2, i] > mu, "gray", "red"))  
}

#---Para n=30---#
B <- 5000 ## número de experimentos
n <- 30 ## tamaño de muestra
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
c=0
mis.intervalos <- matrix(rep(0, 10000), nrow = 2) ## matriz de ceros

for(i in 1:5000) {
  mis.intervalos[,i] <- calcula.el.intervalo(i)
  if (mis.intervalos[1, i] <= mu && mis.intervalos[2, i] >= mu){
    c= c+1
  }
  else{
    c=c
  }
}

Porcentaje <- c/5000 #Proporcion de intervalos que atrapan al parametro

#---Para n=50---#

B <- 5000 ## número de experimentos
n <- 50 ## tamaño de muestra
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
c=0
mis.intervalos <- matrix(rep(0, 10000), nrow = 2) ## matriz de ceros

for(i in 1:5000) {
  mis.intervalos[,i] <- calcula.el.intervalo(i)
  if (mis.intervalos[1, i] <= mu && mis.intervalos[2, i] >= mu){
    c= c+1
  }
  else{
    c=c
  }
}

Porcentaje <- c/5000 #Proporcion de intervalos que atrapan al parametro

#---Para n=100---#

B <- 5000 ## número de experimentos
n <- 100 ## tamaño de muestra
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
c=0
mis.intervalos <- matrix(rep(0, 10000), nrow = 2) ## matriz de ceros

for(i in 1:5000) {
  mis.intervalos[,i] <- calcula.el.intervalo(i)
  if (mis.intervalos[1, i] <= mu && mis.intervalos[2, i] >= mu){
    c= c+1
  }
  else{
    c=c
  }
}

Porcentaje <- c/5000 #Proporcion de intervalos que atrapan al parametro
#--------------- Punto 4 ---------------#
  


#--------------- Punto 7 ---------------#

#----Parametros de las distribuciones----#


################   Parametros de las distribuciones #######################

k=100               ## Parametro de forma de la gamma
lambda_ga=0.5       ## Parametro de escala de la gamma
media=200           ## Media de la normal y parametro de locacion de laplace
sigma=sqrt(400)     ## Desviacion estandar de la normal
c=200 		    ## Parametro uniforme
d=600 		    ## Parametro uniforme

########################      distribucion normal    ##################################
cv_r=sigma/media    ## Coeficiente de variaciÛn
alpha=0.05          ## Trabajamos con un nivel de confianza del 0.05
nsim=5000           ## numero de simulaciones
nsim2=100          ## Numero de remuestras
n<-c(5,seq(10,100,10)) ## TamaÒo de muestras

tabla_sd<-matrix(nrow=11,ncol=6)               ## Matriz que almacena los 
colnames(tabla_sd)<-c("L.I 1","C.I 1", "L.I 2" #valores de la longitud media 
                      ,"C.I 2","L.I 3","C.I 3")#y la cobertura media de cada 
rownames(tabla_sd)<-c("5","10","20","30","40"  #intervalo para cada tamaÒo 
                      ,"50","60", "70","80",   #de muestra para la desviacion
                      "90","100")

tabla_cv<-matrix(nrow=11,ncol=8)               ## Matriz que almacena los 
colnames(tabla_cv)<-c("L.I 1","C.I 1", "L.I 2" #valores de la longitud media 
                      ,"C.I 2","L.I 3","C.I 3",#y la cobertura media de cada
                      "L.I 4", "C.I 4")        #intervalo para cada tamaÒo 
rownames(tabla_cv)<-c("5","10","20","30",      #de muestra para la desviacion
                      "40","50","60",
                      "70","80","90","100")

dist<-function(n,a,b){    ## Ditribucion a evaluar 
  x<-rnorm(n,a,b)
}

###################### Intevalos para la desviaciÛn #########################

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

##### Intervalo bootrap para la desviaciÛn estandar 

sd_boot=0  ## Vector para la distribcion de la desviaciÛn

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
    
    cv= sd(x)/mean(x)  ## Coeficiente de varianciÛn estiamado
    
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

##### Intervalo bootrap para el coeficiente de variaciÛn ###############
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
plot(tabla_sd[,2],main="Cobertura de los intervalos para sd
     en la distribucion Normal",col="red",type="o",
     xaxt='n',ylim=c(0,1),pch=5,xlab="Tamaño muestra",
     ylab="Proporcion de aceptados")
axis(1,1:length(n),n)
lines(tabla_sd[,4],col="blue",type="o")
lines(tabla_sd[,6],type="b")
abline(h=1-alpha,col="green")
legend(5,0.7,c("alpha","estimador 1","estimador 2","Bootstrap"),
       col=c("green","red","blue","black"),lty=c(1,1),cex=0.8)


plot(tabla_sd[,1],main="Longitud de los intervalos para sd
     en la distribucion Normal",col="red",type="o",
     xaxt='n',ylim=c(0,max(tabla_sd[,1])),pch=5,xlab="Tamaño muestra",
     ylab="Longitud")
axis(1,1:length(n),n)
lines(tabla_sd[,3],col="blue",type="o")
lines(tabla_sd[,5],type="b")
legend(5,40,c("estimador 1","estimador 2","Bootstrap"),
       col=c("red","blue","black"),lty=c(1,1),cex=0.8 )

plot(tabla_cv[,2],main="Cobertura de los intervalos para CV
     en la distribucion Normal",col="red",type="o",
     xaxt='n',ylim=c(0,1),pch=5,xlab="Tamaño muestra",
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
     xaxt='n',ylim=c(0,max(tabla_cv)),pch=5,xlab="Tamaño muestra",
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
                      "L.I 4", "C.I 4")        #intervalo para cada tamaÒo 
rownames(tabla_cv)<-c("5","10","20","30",      #de muestra para la desviacion
                      "40","50","60",
                      "70","80","90","100")
tabla_sd<-matrix(nrow=11,ncol=6)               ## Matriz que almacena los 
colnames(tabla_sd)<-c("L.I 1","C.I 1", "L.I 2" #valores de la longitud media 
                      ,"C.I 2","L.I 3","C.I 3")#y la cobertura media de cada 
rownames(tabla_sd)<-c("5","10","20","30","40"  #intervalo para cada tamaÒo 
                      ,"50","60", "70","80",   #de muestra para la desviacion
                      "90","100")

alpha=0.05
k=100               ## Parametro de forma de la gamma
lambda_ga=0.5       ## Parametro de escala de la gamma
sigma=sqrt(400) 
cv_g=1/sqrt(100) 
nsim=5000           ## numero de simulaciones
nsim2=100          ## Numero de remuestras
n<-c(5,seq(10,100,10)) ## TamaÒo de muestras

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

##### Intervalo bootrap para la desviaciÛn estandar 

sd_boot=0  ## Vector para la distribcion de la desviaciÛn

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
    
    cv= sd(x)/mean(x)  ## Coeficiente de varianciÛn estiamado
    
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

##### Intervalo bootrap para el coeficiente de variaciÛn ###############
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

plot(tabla_sd[,2],main="Cobertura de los intervalos para sd
     en la distribucion Gamma",col="red",type="o",
     xaxt='n',ylim=c(0.6,1),pch=5,xlab="Tamaño de muestra",
     ylab="Proporcion de aceptados")
axis(1,1:length(n),n)
lines(tabla_sd[,4],col="blue",type="o")
lines(tabla_sd[,6],type="b")
abline(h=1-alpha,col="green")
legend(5,0.85,c("alpha","estimador 1","estimador 2","Bootstrap"),
       col=c("green","red","blue","black"),lty=c(1,1),cex=0.8)


plot(tabla_sd[,1],main="Longitud de los intervalos para sd
     en la distribucion Gamma",col="red",type="o",
     xaxt='n',ylim=c(0,max(tabla_sd[,1])),pch=5,xlab="Tamaño de muestra",
     ylab="Proporcion de aceptados")
axis(1,1:length(n),n)
lines(tabla_sd[,3],col="blue",type="o")
lines(tabla_sd[,5],type="b")
legend(5,15,c("estimador 1","estimador 2","Bootstrap"),
       col=c("red","blue","black"),lty=c(1,1),cex=0.8)

plot(tabla_cv[,2],main="Cobertura de los intervalos para CV
     en la distribucion Gamma",col="red",type="o",
     xaxt='n',ylim=c(0,1),pch=5,xlab="Tamaño de muestra",
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
     xaxt='n',ylim=c(0,max(tabla_cv)),pch=5,xlab="Tamaño de muestra",
     ylab="Proporcion de aceptados")
axis(1,1:length(n),n)
lines(tabla_cv[,3],col="blue",type="o")
lines(tabla_cv[,5],type="b")
lines(tabla_cv[,7], col="purple", type="o")
legend(5,0.85,c("estimador 1","estimador 2","estimador 3","Bootstrap"),
       col=c("red","blue","purple","black"),lty=c(1,1),cex=0.8) 

########################     distribucion Uniforme   ##################################
cv_r=sigma/media    ## Coeficiente de variaciÛn
alpha=0.05          ## Trabajamos con un nivel de confianza del 0.05
nsim=5000           ## numero de simulaciones
nsim2=100          ## Numero de remuestras
n<-c(5,seq(10,100,10)) ## TamaÒo de muestras

tabla_sd<-matrix(nrow=11,ncol=6)               ## Matriz que almacena los 
colnames(tabla_sd)<-c("L.I 1","C.I 1", "L.I 2" #valores de la longitud media 
                      ,"C.I 2","L.I 3","C.I 3")#y la cobertura media de cada 
rownames(tabla_sd)<-c("5","10","20","30","40"  #intervalo para cada tamaÒo 
                      ,"50","60", "70","80",   #de muestra para la desviacion
                      "90","100")

tabla_cv<-matrix(nrow=11,ncol=8)               ## Matriz que almacena los 
colnames(tabla_cv)<-c("L.I 1","C.I 1", "L.I 2" #valores de la longitud media 
                      ,"C.I 2","L.I 3","C.I 3",#y la cobertura media de cada
                      "L.I 4", "C.I 4")        #intervalo para cada tamaÒo 
rownames(tabla_cv)<-c("5","10","20","30",      #de muestra para la desviacion
                      "40","50","60",
                      "70","80","90","100")

dist<-function(n,a,b){    ## Ditribucion a evaluar 
  x<-runif(n,a,b)
}

###################### Intevalos para la desviaciÛn #########################

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

##### Intervalo bootrap para la desviaciÛn estandar 

sd_boot=0  ## Vector para la distribcion de la desviaciÛn

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
    
    cv= sd(x)/mean(x)  ## Coeficiente de varianciÛn estiamado
    
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

##### Intervalo bootrap para el coeficiente de variaciÛn ###############
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
plot(tabla_sd[,2],main="Cobertura de los intervalos para sd
     en la distribucion uniforme",col="red",type="o",
     xaxt='n',ylim=c(0,1),pch=5,xlab="Tamaño de muestra",
     ylab="Proporcion de aceptados")
axis(1,1:length(n),n)
lines(tabla_sd[,4],col="blue",type="o")
lines(tabla_sd[,6],type="b")
abline(h=1-alpha,col="green")
legend(5,0.7,c("alpha","estimador 1","estimador 2","Bootstrap"),
       col=c("green","red","blue","black"),lty=c(1,1),cex=0.8)


plot(tabla_sd[,1],main="Longitud de los intervalos para sd
     en la distribucion uniforme",col="red",type="o",
     xaxt='n',ylim=c(0,max(tabla_sd[,1])),pch=5,xlab="Tamaño de muestra",
     ylab="Longitud")
axis(1,1:length(n),n)
lines(tabla_sd[,3],col="blue",type="o")
lines(tabla_sd[,5],type="b")
legend(5,250,c("estimador 1","estimador 2","Bootstrap"),
       col=c("red","blue","black"),lty=c(1,1),cex=0.8 )

plot(tabla_cv[,2],main="Cobertura de los intervalos para CV
     en la distribucion uniforme",col="red",type="o",
     xaxt='n',ylim=c(0,1),pch=5,xlab="Tamaño de muestra",
     ylab="Proporcion de aceptados")
axis(1,1:length(n),n)
lines(tabla_cv[,4],col="blue",type="o")
lines(tabla_cv[,6],type="b")
lines(tabla_cv[,8], col="purple", type="o")
abline(h=1-alpha,col="green")
legend(5,250,c("alpha""estimador 1","estimador 2","estimador 3","Bootstrap"),
       col=c("green","red","blue","purple","black"),lty=c(1,1),cex=0.8)


plot(tabla_cv[,1],main="Longitud de los intervalos para CV
     en la distribuciÛn uniforme",col="red",type="o",
     xaxt='n',ylim=c(0,max(tabla_cv)),pch=5,xlab="Tamaño de muestra",
     ylab="Longitud")
axis(1,1:length(n),n)
lines(tabla_cv[,3],col="blue",type="o")
lines(tabla_cv[,5],type="b")
lines(tabla_cv[,7], col="purple", type="o")
legend(5,0.85,c("estimador 1","estimador 2","estimador 3","Bootstrap"),
       col=c("red","blue","purple","black"),lty=c(1,1),cex=0.8)


