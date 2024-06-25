#Ejercicio 5

#Cargar los datos
ruta <- file.choose()
datos <- read_excel(ruta)

#Apartado A
#Creación del vector
x <- datos$EDAD[101:135]

#Representación del histograma
hist(x, xlab = "Edad", ylab = "Frecuencia Relativa",  main = "Distribución de la
población EDAD (N=35)", freq = FALSE)

#Apartado B
#Instalar y cargar paquetes
install.packages("DescTools")
library("DescTools")

#Creación de la matriz
#help("CombSet")
muestras <- CombSet(x, m = 2, repl = TRUE, ord = TRUE)

#Apartado C
#Creación del vector
mediasMuestrales <- rowMeans(muestras)

#Apartado D
#Creación del histograma
hist(mediasMuestrales, main = "Distribución muestral de la media para la población EDAD
     (N=35 y n=2)", xlab = bquote("Media muestral (" * bar(x) * ")"), ylab = "Frecuencia Absoluta")
     
#Apartado E
#Muestra = 3
muestras3 <- CombSet(x, m = 3, repl = TRUE, ord = TRUE)
mediasMuestrales3 <- rowMeans(muestras3)
hist(mediasMuestrales3, main = "Distribución muestral de la media para la población EDAD
     (N=35 y n=3)", xlab = bquote("Media muestral (" * bar(x) * ")"), ylab = "Frecuencia Absoluta")

#Muestra = 4
muestras4 <- CombSet(x, m = 4, repl = TRUE, ord = TRUE)
mediasMuestrales4 <- rowMeans(muestras4)
hist(mediasMuestrales4, main = "Distribución muestral de la media para la población EDAD
     (N=35 y n=4)", xlab = bquote("Media muestral (" * bar(x) * ")"), ylab = "Frecuencia Absoluta")

#Apartado F
#¿La población sigue una distribución normal? 
#Si, al observar los 2 útlimos gráficos, se puede ver como siguie una distribución normal (la forma de la campana de Gauss)

#¿Qué efecto tiene el tamaño de la muestra n en la normalidad de la distribuciones muestrales?
#A menor tamaño de la muestra tendremos menos cantidad de datos, se puede ver al comparar el gráfico con n = 2 y n = 4, el primer gráfico parece que se acerca a la distribución normal, pero no es suficiente ya que es poco exacto, pero el último gráfico confirma la distribución normal.
#Esto se puede ver también en el tamaño de las matrices, que crece al escoger un tamaño de muestra mayor, la primera disponia de 1225 filas, la segunda 128625 y la tercera 6002500, pesando unos 48 MB.
#Por lo tanto se puede decir que a mayor tamaño de muestra -> mayor cantidad de datos -> más precisión.

#¿Este fenómeno se debe a alguna razón matemática? En caso afirmativo, explica brevemente esta razón con tus palabras. 
#Tras investigar un poco me he topado con el fenómeno de la "ley de los grandes números", que explica que cuanto mas aumenta el tamaño de una muestra, la media de la muestra tiende a acercarse a la media de la población.
