
# Laboratorio 3 Metodos Estadisticos 

# Diana C. Arias Sinisterra
# Kevin S. Garcia Chica
# Cesar A. Saavedra Vanegas

#--------------- Punto 1 ---------------#
n = 10
media <- 5
varianza <- 1
alpha <- 0.05
cuantil <- qnorm(1-alpha/2)
x <- rnorm(n,media,varianza)

lim_inf <- media - cuantil * sqrt(varianza)/sqrt(n)
lim_sup <- media + cuantil * sqrt(varianza)/sqrt(n)

r=5000
muchasnormales=replicate(r,x)

para30 <- muchasnormales[1:30]
para50 <- muchasnormales[1:50]
para100 <- muchasnormales[1:100]

hist(muchasnormales, xlim = c(lim_inf,lim_sup))
hist(para30, xlim = c(lim_inf,lim_sup))
hist(para50, xlim = c(lim_inf,lim_sup))
hist(para100, xlim = c(lim_inf,lim_sup))


#--------------- Punto 4 ---------------#
  


#--------------- Punto 7 ---------------#