#Apartado A
#Creación de los vectores de secuencias

a1 <- c(seq(from = 0, to = 20, by = 1))
a2 <- c(seq(from = 1, by = 5, length.out = 20))
a3 <- c(seq(from = 0, to = 1, length.out = 30), seq(from = 2, to = 3, length.out = 15))

#Apartado B

#Creacion del vector
vector <- c(3, -1, 4, 2)

#Creación del resto de vectores con la función rep
b1 <- rep(vector, 3)
b2 <- rep(vector, each = 4)
b3 <- rep(vector, each = 3, length.out = 15)

#Apartado C
#Creación de la función
f <- function(x) {
  3 * cos(x)
}

#Creación del vector secuencia y el vector en el que se almacenarán los resultados (del mismo tamaño que la secuencia)
secuencia <- seq((-3 * pi) / 2, (3 * pi) / 2, by = pi / 64)
resultados <- numeric(length(secuencia))

#Calculo de la función para cada valor de la secuencia
for (i in 1:length(secuencia)) {
  resultados[i] <- f(secuencia[i])
}

#Asignacion de los valores maximos y minimos
max <- max(resultados)
min <- min(resultados)

#Impresión del gráfico por pantalla
plot(secuencia, resultados)


#Apartado D
#Creacion del vector con las temperaturas medias mensuales.
temperaturas <- c(14.9, 14.1, 14.5, 15.8, 18.8, 22.5, 25.7, 26.7, 25.4, 22.7, 19.1, 16.4)

#D1 - Calculo de la media
media = sum(temperaturas) / length(temperaturas)
media2 = mean(temperaturas)

#D2 - Ordenación
ascendente <- sort(temperaturas)
descendente <- sort(temperaturas, decreasing = TRUE)

#Obtención de los indices 
indices_ascendente <- sort(temperaturas, index.return = TRUE)
indices_descendente <- sort(temperaturas, index.return = TRUE, decreasing = TRUE)

#Al generar los indices se observa que el metodo sort almacena los dos vectores, valores e indices en una lista.
print(indices_ascendente)


#Apartado E
#Creación de la variable de iteraciones del producto y calcuó de pi según la función
n <- 10000
resultado <- 2 * prod(seq(2, 2 * n, by = 2) / (seq(2, 2 * n, by = 2) - 1) * seq(2, 2 * n, by = 2) / (seq(2, 2 * n, by = 2) + 1))


#Apartado F
#Creación del vector de datos
datos <- c(256, 186,
           251, 320, 338, 297, 317, 295, 240,
           365, 363, 431, 409, 478, 390, 339,
           506, 501, 524, 547, 589, 549, 418,
           600, 502, 601, 479, 539, 531, 306,
           320)
#Aplicacion de la funcion cumsum para obtener los casos diarios acumulados
casos_acumulados <- cumsum(datos)

#Graficacion de los datos
barplot(casos_acumulados, names.arg = 1:31)


#Apartado G
help(rev)
#La función rev() se utiliza para invertir los elementos de un vector.
#Puede ser útil para la creacion de gráficos en el que los datos a mostrar deben ser en orden descendiente (siempre y cuando el vector original este ordenado crecientemente).
#Un ejemplo de su uso, es el de la detección de palabras palindromas, a pesar de que no se pueda utilizar la funcion en cadenas de texto, podemos simular una cadena en un vector.

palabra <- c('H', 'o', 'l', 'a')
inversion <- rev(palabra)

#Por ejemplo, esta lógica se puede añadir a una función que devuelva un booleano en caso de que la palabra sea palindroma (es decir que se lea igual de derecha a izquierda que de izquierda a derecha).