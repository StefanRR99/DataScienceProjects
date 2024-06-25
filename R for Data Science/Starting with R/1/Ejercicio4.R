#Ejercicio 4
#Apartado A
#Creación del vector que almacena los 100 valores equiespaciados
x <- seq(-2, 4, length.out = 100)

#Obtención de los valores de la función normal, con los parametros ajustados segun los requerimientos del ejericio
f <- dnorm(x, mean = 1, sd = 2)

#Creación del gráfico.
plot(x, f)


#Apartado B
#Creación del vector que almacena los 50 valores
x2 <- c(0:49)

#Obtención de los valores de la función binomial, con los parametros ajustados segun los requerimientos del ejericio
f2 <- dbinom(x2, size = 50, prob = 0.5)

#Creación del gráfico.
plot(x2, f2)

