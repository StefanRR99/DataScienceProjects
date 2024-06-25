#Ejercicio 2

#Cargar los datos
ruta <- file.choose()
datos <- read_excel(ruta)

#Apartado A
#Creación de los histogramas
#freq = FALSE, frecuencias relativas
hist(datos$EDAD, main = "Histograma de la Edad", xlab = "Edad (años)", ylab = "Frecuencia Absoluta", col = "darkseagreen1")
hist(datos$`ALTURA (cm)`, freq = FALSE, main = "Histograma de la Altura", xlab = "Altura (cm)", ylab = "Frecuencia Relativa", col = "firebrick1")

#Apartado B
qqnorm(datos$EDAD, col = "dodgerblue3", main = "Q-Q Plot de EDAD",
       xlab = "Cuantiles teóricos", ylab = "Cuantiles muestrales")
qqline(datos$EDAD, col = "lightblue", lwd = 2)

qqnorm(datos$`ALTURA (cm)`, col = "magenta", main = "Q-Q Plot de ALTURA",
       xlab = "Cuantiles teóricos", ylab = "Cuantiles muestrales")
qqline(datos$`ALTURA (cm)`, col = "lavenderblush", lwd = 2)

#Apartado C
#Para guardar los gráficos generados -> Plots -> Export -> Save as Image/PDF
#También existen metodos para exportar los gráficos mediante código.

#Apartado D
#Según se puede observar en los gráficos generados, la variable edad no sigue una distribución normal.
#Se observa que las diferentes edades aparecen un numero similar de veces en el conjunto de datos, aproximadamente 20 veces cada edad, segun el gráfico del histograma.
#En cambio, la variable altura si que sigue una distribución normal, con la media de altura cerca de los 170 cm.
#Esto se puede confirmar mirando los dos tipos de graficos generados, el histograma y el Q-Q Plot.