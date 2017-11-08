x<-c(12,10,15,8,19,14,12,21,16,11,8,15)
y<-c(11,11,17,9,21,13,16,25,20,18,10,17)
n<-c(1,2,3,4,5,6,7,8,9,10,11,12)
x11()
plot(n, x, type = "b",pch = 19, ylab = "Unidades Vendidas",xlab = "Número de negocio", ylim=c(min(x)-1,max(y)+1),main = "Gráfica de unidades vendidas antes y despues de la publicidad")
lines(n, y, type = "b", col = 2, pch = 19)
legend(x=9.3,y=25.8,c(x="Antes",y="Despues") , 
       col = c(1, 2), lwd = c(2, 2))

