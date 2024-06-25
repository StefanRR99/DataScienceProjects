#Ejercicio 7 - REPASAR BIEN Y RESPONDER A LAS PREGUNTAS
#Mirar la teoria
library("BSDA")

#Cargar los datos
ruta <- file.choose()
datos <- read_excel(ruta)

#Apartado A
#Varialbes del ejercicio 1T4
media_muestral <- 58.3
desviacion_pobl <- 9.5
muestra <- 40
nivel_significancia <- 0.05
mu0 <- 60 

#Cálculo mediante la función zsum.test()
help("zsum.test")
resultado <- zsum.test(mean.x = media_muestral, sigma.x = desviacion_pobl, n.x = muestra, mu = mu0, alternative = "two.sided", conf.level = 1 - nivel_significancia)

#Obtención del P-Valor
P_valor <- round(resultado$p.value, 3)

#Comparación de P-valor con el nivel de significancia.
print(P_valor) 
print(P_valor > nivel_significancia)
#Verdadero - Aceptamos H0


#Apartado B
#Creación de la muestra
muestra_col_hdl <- sample(datos$`COL_HDL (mg/dL)`, 100, replace = TRUE)

#Cálculo mediante la función zsum.test()
#H0 >= 50, H1 < 50
resultado2 <- zsum.test(mean.x = mean(muestra_col_hdl), sigma.x = 16, n.x = length(muestra_col_hdl), mu = 50, alternative = "greater", conf.level = 0.95)

#Obtención del P-Valor
P_valor2 <- round(resultado2$p.value, 4)
#Comparación de P-valor con el nivel de significancia.
print(P_valor2)
print(P_valor2 > 0.05)
#FALSO - Rechazamos H0

#¿Que puedes concluir a partir del P-valor obtenido? #Sabiendo que un nivel de HDL mayor o igual a 50 mg/dL se considera saludable, ¿se podr´ıa afirmar que la poblacion tiene, en promedio, niveles saludables de HDL?
#Como el Pvalor es muy pequeño y rechazamos la hipotesis nula, podemos concluir que la poblacion disponible en los datos no tiene un nivel saludable de HDL.


#Apartado C
#Creación de los subconjuntos de datos masculinos y femeninos y de las muestras
datos_masc <- subset(datos, datos$`GÉNERO (1=MASCULINO)` == 1)
datos_fem <- subset(datos, datos$`GÉNERO (1=MASCULINO)` == 0)
muestra_glob_rojos_masc <- sample(datos_masc$`GLOB_ROJOS (106 cél/uL)`, 50, replace = TRUE)
muestra_glob_rojos_fem <- sample(datos_fem$`GLOB_ROJOS (106 cél/uL)`, 50, replace = TRUE)

#H0 <= 4.7 / 4.2, H1 > 4.7, 4.2

#HOMBRES
help("tsum.test")
resultado_hombres <- tsum.test(mean.x = mean(muestra_glob_rojos_masc), s.x = sd(muestra_glob_rojos_masc), n.x = length(muestra_glob_rojos_masc), mu = 4.7, alternative = "less", conf.level = 0.99)
PValor_hombres <- round(resultado_hombres$p.value, 4)
print(PValor_hombres)
print(PValor_hombres > 0.01)
#Verdadero - Aceptamos H0

#MUJERES
resultado_mujeres <- tsum.test(mean.x = mean(muestra_glob_rojos_fem), s.x = sd(muestra_glob_rojos_fem), n.x = length(muestra_glob_rojos_fem), mu = 4.2, alternative = "less", conf.level = 0.99)
PValor_mujeres <- round(resultado_mujeres$p.value, 4)
print(PValor_mujeres)
print(PValor_mujeres > 0.01)
#Verdadero - Aceptamos H0

#CONCLUSIONES
#A partir de los datos, podemos aceptar la hipotesis nula, pero esto no quiere decir que la hipotesis nula sea la correcta.
#Aun asi, según los Pvalores, en el caso de las mujeres, la hipotesis se puede asegurar con mas confianza que en el caso de los hombres.


#Apartado D 
help("prop.test")

#Datos de la población de generos
generos <- datos$`GÉNERO (1=MASCULINO)`

#Cálculo del resultado
#H0 = 0.5, H1 != 0.5
resultado_D <- prop.test(x = sum(generos), n = length(datos$`GÉNERO (1=MASCULINO)`), p = 0.5, alternative = "two.sided", conf.level = 0.98, correct = FALSE)
Pvalor4 <- round(resultado_D$p.value, 4)
print(Pvalor4)
print(Pvalor4 > 0.02)
#Verdadero, haceptamos H0


#Apartado E
#Creación de la muestra
alturas <- sample(datos$`ALTURA (cm)`, 75, replace = TRUE)

library("EnvStats")
help("EnvStats")

#Variables
desv_poblE <- 10
var_poblE <- desv_poblE^2

#Calculo
resultado_E <- varTest(alturas, alternative = "less", conf.level = 0.95, sigma.squared = var_poblE)

Pvalor5 <- round(resultado_E$p.value, 4)
print(Pvalor5)
print(Pvalor5 > 0.05)
#Verdadero, aceptamos H0

