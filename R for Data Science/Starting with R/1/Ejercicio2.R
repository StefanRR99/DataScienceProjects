#Ejercicio 2
#Creación del vector numeros.
numeros <- c(1:30)

#Creación del vector condicionado a partir del anterior.
numeros_condicion <- numeros[(numeros < 10 & numeros%%2 == 0) | (numeros > 20 & numeros%%2 == 1)]

print(numeros_condicion) 