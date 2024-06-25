#Ejercicio 3
#Creación del vector progresion.

progresion <- c(2*3^seq(0, 9, by= 1))
print(progresion)
class(progresion)

#Creación del vector indices_condicion.
indices_condicion <- which(progresion%%9 == 0 & progresion < 100)

#Impresión de los indices del vector que cumplen las condiciones.
print(indices_condicion)

#Impresión de los números del vector a partir de los indices que cumplen con las condiciones.
print(progresion[indices_condicion]) 