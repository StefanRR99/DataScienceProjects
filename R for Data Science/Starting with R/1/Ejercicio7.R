#Apartado A
#Instanciación del vector de datos y el array
datos <- round(seq(255, 0, length.out = 3 * 4 * 5))
A <- array(datos, dim=c(3,4,5))
print(A)

#Apartado B
#El primer vector extrar los valores de las filas 3 y 1, el segundo de las columnas 2 y 3 y el tercer vector extrae de las matrices 5, 4 y 3
B <- A[c(3, 1), c(2, 3), c(5, 4, 3)]
print(B)

#Apartado C
#Instanciación de C
C <- A
C[c(1, 3), 2, c(1, 3, 5)] <- -1
#Definición de la diagonal principal a 1
C <- lapply(1:5, function(i) `diag<-`(C[,,i], 1))
print(C)

#Apartado D
#Cambio de nombres en las columnas y las filas del array A
filas <- c("Fila 1", "Fila 2", "Fila 3") 
columnas <- c("Columna 1", "Columna 2", "Columna 3", "Columna 4") 
dimnames(A) <- list(filas, columnas)
print(A)