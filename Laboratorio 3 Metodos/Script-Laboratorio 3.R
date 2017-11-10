
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

#---Para n=50---#

#---Para n=100---#

#--------------- Punto 4 ---------------#
  


#--------------- Punto 7 ---------------#

################   Parametros de las distribuciones #######################

k=100               ## Parametro de forma de la gamma
lambda_ga=0.5       ## Parametro de escala de la gamma
media=200           ## Media de la normal y Uniforme
sigma=sqrt(400)     ## Desviacion estandar de la normal
a=200               ## Parametro Uniforme 
b=200               ## Parametro Uniforme 

########################      Datos     ##################################
cv_r=sigma/media    ## Coeficiente de variacion
alpha=0.05          ## Trabajamos con un nivel de confianza del 0.05
nsim=5000           ## numero de simulaciones
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


####### Graficas ########

plot(tabla_sd[,2],main="Cobertura de los intervalos para sd
     en la distribuciÛn de Normal",col="red",type="o",
     xaxt='n',ylim=c(0,1),pch=5,xlab="TamaÒo de muestra",
     ylab="ProporciÛn de aceptados")
axis(1,1:length(n),n)
lines(tabla_sd[,4],col="blue",type="o")
lines(tabla_sd[,6],type="b")
abline(h=1-alpha,col="green")
legend(5,0.7,c("estimador 1","estimador 2"),
       col=c("red","blue"),lty=c(1,1),cex=0.8)

plot(tabla_sd[,1],main="Longitud de los intervalos para sd
     en la distribuciÛn de Normal",col="red",type="o",
     xaxt='n',ylim=c(0,max(tabla_sd[,1])),pch=5,xlab="TamaÒo de muestra",
     ylab="Longitud")
axis(1,1:length(n),n)
lines(tabla_sd[,3],col="blue",type="o")
lines(tabla_sd[,5],type="b")
legend(5,90,c("estimador 1","estimador 2"),
       col=c("red","blue"),lty=c(1,1),cex=0.8 )

plot(tabla_cv[,2],main="Cobertura de los intervalos para CV
     en la distribuciÛnde Normal",col="red",type="o",
     xaxt='n',ylim=c(0,1),pch=5,xlab="TamaÒo de muestra",
     ylab="ProporciÛn de aceptados")
axis(1,1:length(n),n)
lines(tabla_cv[,4],col="blue",type="o")
lines(tabla_cv[,6],type="b")
lines(tabla_cv[,8], col="purple", type="o")
abline(h=1-alpha,col="green")
legend(5,0.4,c("estimador 1","estimador 2","estimador 3"),
       col=c("red","blue","purple"),lty=c(1,1),cex=0.8)

plot(tabla_cv[,1],main="Longitud de los intervalos para CV
     en la distribuciÛn de Normal",col="red",type="o",
     xaxt='n',ylim=c(0,max(tabla_cv)),pch=5,xlab="TamaÒo de muestra",
     ylab="Longitud")
axis(1,1:length(n),n)
lines(tabla_cv[,3],col="blue",type="o")
lines(tabla_cv[,5],type="b")
lines(tabla_cv[,7], col="purple", type="o")
legend(5,0.85,c("estimador 1","estimador 2","estimador 3"),
       col=c("red","blue","purple"),lty=c(1,1),cex=0.8) 

#---------------------------------------#



