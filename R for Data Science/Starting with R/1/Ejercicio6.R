#Ejercicio 6
#Instalación e importación del paquete expm
install.packages("expm")
library("expm")

#Apartado A
#Creación del vector de datos y de las matrices
datos <- c(3.1, 5.3, 2, 3.5,-1.4, 2.7, 4.1, -5.3, 2.3, 3.7, -1, 0)
A <- matrix (datos, nrow = 3, ncol = 4, byrow = TRUE)
B <- matrix (datos, nrow = 6, ncol = 2, byrow = FALSE)

print(dim(A))
print(dim(B))

#Apartado B1
A[2, 3] <- 5
A[2,] <- 0
A[,3] <- -2
print(A)

#Apartado B2 - dos maneras de hacerlo, la primera en caso de no conocer el tamaño de la matriz
A[c(1, 1, nrow(A), nrow(A)), c(1, ncol(A), 1, ncol(A))] <- 10
A[c(1, 1, 3, 3), c(1, 4, 1, 4)] <- 10
print(A)

#Apartado B3
D <- A[, -2]
print(D)
diag(D) <- 3
print(D)
D <- ifelse(row(D) != col(D), 0, D)
print(D)

#Apartado C1
#Creación de los vectores de datos y de la matriz final por filas
datos <- c(2017, 2018, 2019)
Andalucıa <- c(23699, 24091, 23826)
Islas_Baleares <- c(32163, 34007, 32179)
Canarias <- c(22790, 23048, 24565)
C_Valenciana <- c(24034, 25207, 26176)
C_Madrid <- c(32451, 33055, 35587)
R_Murcia <- c(23574, 24801, 24448)

matriz <- rbind (datos , Andalucıa, Islas_Baleares, Canarias, C_Valenciana, C_Madrid, R_Murcia)

#Apartado C2
#Calculo del promedio por año sin incluir la primera fila (la he definido como las etiquetas de los años)
media_anual <- colMeans(matriz[-1, ])
matriz <- rbind(matriz, media_anual)

#Calculo del promedio por comunidad autonoma
media_ccaa <- rowMeans(matriz)
matriz <- cbind(matriz, media_ccaa)

#Apartado C3
#Al trabajar con vectores nombrados, las etiquetas de las filas ya estaban añadidas. 
#Eliminación de la primera fila ya que los años estaban definidos en un vector, y asignación de titulos a las columnas.
matriz <- matriz[-1, , drop = FALSE]
colnames(matriz) <- c("2017", "2018", "2019", "Media por CCAA")
print(matriz)


#Apartado D
#Creacion de las matrices
A <- matrix(c( 1, -1, 0, 3,
               2, 0, 1, 0,
               3, 5, 0, 2,
              -2, -3, -1, 0), nrow = 4, byrow = TRUE)

B <- matrix(c(1, 0, 0, 1,
              2, -1, -3, 0,
              0, 0, 0, 1,
              -1, 0, 0, 0), nrow = 4, byrow = TRUE)

C <- matrix(c(1, 0, 0, 0,
              0, 2, 0, 0,
              0, 0, 3, 0,
              0, 0, 0, 4), nrow = 4, byrow = TRUE)

#Instanciación de las matrices necesarias para el calculo
B_transpuesta <- t(B)
C_inversa <- solve(C)
I <- diag(4)

#Calculo y visualización 
R <- 0.5 * (A + B_transpuesta) - 3 * C_inversa * C * I
print(fractions(R))
