#Ejercicio 5

#Cargar los datos
ruta <- file.choose()
datos <- read_excel(ruta)

install.packages("BSDA")
library("BSDA")

#Apartado A
#Variables del ejercicio 1T3
media_muestral <- 15000
desviacion <- 800
muestra <- 30

#Guardar el resultado 
help("zsum.test")
resultado <- zsum.test(mean.x = media_muestral, sigma.x = desviacion, n.x = muestra, conf.level = 0.95)

#Extraer el vector IC y redondear 
intervalo_confianza <- resultado$conf.int[1:2]
intervalo_confianza <- round(intervalo_confianza, 0)

print(intervalo_confianza)

#Apartado B
#Creación de la muestra - (por defecto con reposición)
muestra_PA_SIS <- sample(datos$`PA_SISTÓLICA (mm Hg)`, 50, replace = TRUE)

#Cálculo, extracción, redondeo e impresión del IC
resultado2 <- zsum.test(mean.x = mean(muestra_PA_SIS), sigma.x = 15.8, n.x = length(muestra_PA_SIS), conf.level = 0.95)
print(mean(muestra_PA_SIS))
intervalo_confianza2 <- round(resultado2$conf.int[1:2], 1) #Redondeo a 1, ya que son los mismos decimales que la media muestral.
print(intervalo_confianza2)


#Apartado C - desviación desconocida, uso de desviación muestral (s)
#Creación de la muestra
muestra_PA_DIAS <- sample(datos$`PA_DIASTÓLICA (mm Hg)`, 45, replace = TRUE)

#Cálculo, extracción, redondeo e impresión del IC, con la función tsum.test()
help("tsum.test")
resultado3 <- tsum.test(mean.x = mean(muestra_PA_DIAS), s.x = sd(muestra_PA_DIAS), n.x = length(muestra_PA_DIAS), conf.level = 0.99)
print(mean(muestra_PA_DIAS))
intervalo_confianza3 <- round(resultado3$conf.int[1:2], 2)
print(intervalo_confianza3)


#Apartado D
#install.packages("epitools")
library(epitools)

#Número de pacientes que sufren trombocitopenia
pacientes <- sum(datos$`PLAQUETAS (1000 cél/uL)` < 150)

#Cálculo, extracción, redondeo e impresión del IC, con la función binom.approx()
help("binom.approx")
resultado4 <- binom.approx(x = pacientes, n = length(datos$`PLAQUETAS (1000 cél/uL)`), conf.level = 0.99)
intervalo_confianza4 <- round(c(resultado4$lower, resultado4$upper), 4)
print(intervalo_confianza4)

#¿Realmente sería válido este IC calculado?
#No creo que sea válido o fiable este IC, ya que aunque los resultados del IC parezcan correctos a primera vista, la proporción muestral de los pacientes que sufren de trombocitopenia es muy baja (11/300 = 0.0367) y el tamaño de la muestra (o población en este caso) tambien es muy bajo.
#El intérvalo de confianza se ha calculado con una confianza del 99% y cuadra a primera vista con la proporción pero no creo que se pueda extrapolar este resultado a un caso real.


#Apartado E
#install.packages("EnvStats")
library(EnvStats)

#Creación de la muestra y cálculo de la varianza y desv.
muestra_Cbrazo <- sample(datos$`CIRC_BRAZO (cm)`, 100, replace = TRUE)
var_Cbrazo <- var(muestra_Cbrazo)
desv_Cbrazo <- sd(muestra_Cbrazo)
media_Cbrazo <- mean(muestra_Cbrazo)

#Cálculo, extracción, redondeo e impresión del IC, con la función varTest()
help("varTest")
resultado5 <- varTest(muestra_Cbrazo, conf.level = 0.98)
intervalo_confianza5 <- round(resultado5$conf.int[1:2], 3) 
print(intervalo_confianza5)

#¿Qué condiciones son primordiales que se cumplan para poder calcular el IC de σ^2 y σ? ¿Se cumplen en este caso?
#Para calcular los intervalos de confianza, como en todos los casos, la muestra debe ser aleatoria, en este caso se cumple.
#Ademas, tambien es necesario que la variable siga una distribucion normal, lo podemos confirmar mediante un gráfico.
hist((muestra_Cbrazo))
#No es un gráfico muy exacto pero podemos extrapolar a partir de el que si que sigue una distribución normal.

