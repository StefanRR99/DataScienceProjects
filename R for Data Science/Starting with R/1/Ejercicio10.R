#Ejercicio 10
#Apartado A
#Creación de los objetos
secuencia <- seq(-3, 3, length.out = 8)

vector_logico <- c(FALSE,TRUE,FALSE,FALSE,FALSE,TRUE)
matriz <- matrix(vector_logico, nrow = 2, ncol = 4, byrow = TRUE)

caracteres <- "Hello World"

factor <- factor(c("Malo","Regular","Bueno",
                  "Bueno","Excelente","Malo","Malo","Regular"), 
                 levels = c("Malo", "Regular", "Bueno", "Excelente"), ordered = TRUE)

#Creación de la lista y cambio de nombres
lista <- list(secuencia, matriz, caracteres, factor)
names(lista) <- c("Secuencia", "Matriz", "Frase", "Escala Malo-Excelente")

#Apartado B - Invertir las esquinas y eliminar las columnas 2 y 3
lista$Matriz <- !lista$Matriz[, c(-2, -3)]
print(lista$Matriz)

#Apartado C
#Instalar y cargar paquete stringr
#install.packages("stringr")
library(stringr)

#Convertir Hello World en \\HELLO*world! 
print(lista$Frase)
lista$Frase <- paste("\\", lista$Frase, sep = "")
lista$Frase <- gsub(" ", "*", lista$Frase)
lista$Frase <- paste0(lista$Frase, "!")
lista$Frase <- gsub("Hello", toupper("Hello"), lista$Frase)
lista$Frase <- gsub("W", tolower("W"), lista$Frase)
print(lista$Frase)

#Apartado D 
#Creacion del dataframe (a partir de los datos del entorno) y eliminación de la columna peso
datosPacientes <- datos[order(datos$Altura, decreasing = TRUE), ]
datosPacientes <- datosPacientes[, c("Nombre", "Consume_Drogas", "Altura", "IMC", "Categoria_IMC")]
lista$datosPacientes <- datosPacientes
print(lista$datosPacientes)

#Eliminar de la lista vector caracter
lista$Frase <- NULL
print(lista)

#Eliminar los 3 primeros elementos del factor
lista$`Escala Malo-Excelente` <- lista$`Escala Malo-Excelente`[-c(1:3)]
print(lista$`Escala Malo-Excelente`)

#Apartado E
lista$parametros <- parametros <- c(mean(lista$datosPacientes$Altura), sd(lista$datosPacientes$Altura), sum(lista$datosPacientes$Altura >= 1.70) / nrow(lista$datosPacientes))
names(lista$parametros) <- c("Media Muestral", "Desviación Típica", "Proporción > 1,70")
print(lista)

