#Ejercicio 9
#Apartado A
#Creación de los vectores o factores correspondientes.
Nombre <- c("Juan", "Pedro", "Lorena", "Luis", "Gema", "Ana", "Javier", "Paula")
Consume_Drogas <- factor(c("S", "N", "*", "N", "S", "*", "*", "N"), levels = c('S', 'N', '*'), ordered = TRUE)
Peso <- c(85, 70, 58, 91, 60, 63, 100, 55)
Altura <- c(168, 178, 157, 185, 170, 165, 195, 175)

datos <- data.frame(Nombre, Consume_Drogas, Peso, Altura)

#Apartado B
#Inserción de Miguel al principio
Miguel <- data.frame(Nombre = "Miguel", Consume_Drogas = "N", Peso = 110, Altura = 180)
datos <- rbind(Miguel, datos)

#Inserción de Olga entre Lorena y Luis (quinta posición despues de insertar a Miguel)
Olga <- data.frame(Nombre = "Olga", Consume_Drogas = "*", Peso = 61, Altura = 172)
datos <- rbind(datos[1:4, ], Olga, datos[5:nrow(datos), ])
print(datos)
rownames(datos) <- NULL  #Anotamos esta linea para poner bien los numeros de los registros, ya que Luis tenia asignado el 51 
print(datos)

#Apartado C
#Pasamos altura de cm a m
datos$Altura <- datos$Altura / 100

#Calculamos el IMC
datos$IMC <- round(datos$Peso / (datos$Altura ^ 2), digits = 1)

#Calculo de la categoria IMC. Primero instanciación del vector datos_categoria y mediante un bucle for le añadimos la categoria que cumpla con las condiciones.
datos_categoria <- c()
for (i in 1:nrow(datos)) {
  if(datos$IMC[i] < 18.5){
    datos_categoria <- append(datos_categoria, "Infrapeso")
  }else if(datos$IMC[i] >= 18.5 & datos$IMC[i] < 25){
    datos_categoria <- append(datos_categoria, "Saludable")
  }else if(datos$IMC[i] >= 25 & datos$IMC[i] < 30){
    datos_categoria <- append(datos_categoria, "Sobrepeso")
  }else if(datos$IMC[i] >= 30 & datos$IMC[i] < 40){
    datos_categoria <- append(datos_categoria, "Obeso")
  }else if(datos$IMC[i] >= 40){
    datos_categoria <- append(datos_categoria, "Obesidad extrema")
  }
}

#Añadir el factor de la categoria IMC a los datos
Categoria_IMC <- factor(datos_categoria, levels = c("Infrapeso", "Saludable", "Sobrepeso", "Obeso", "Obesidad extrema"), ordered = TRUE)
datos <- cbind(datos, Categoria_IMC)
print(datos)

#Apartado D
#Cambiar el formato del nombre de las filas, paste() para concatenar cadenas
rownames(datos) <- paste("P", 1:nrow(datos), "-", substr(datos$Nombre, 1, 1))
print(datos)

#Apartado E 
#Ordenar de manera descendiente segun el factor Consume_Drogas y ordenar crecientemente el IMC (- delante para anular decreasing = TRUE)
datosPacientesOrd <- datos[order(datos$Consume_Drogas, -datos$IMC, decreasing = TRUE), ]
print(datosPacientesOrd)

#Apartado F 
#F1
datosNC <- datosPacientesOrd[datosPacientesOrd$Consume_Drogas == "*", ]
datosNC <- datosNC[order(datosNC$Nombre), ]
datosNC <- datosNC[, c("Nombre", "Peso", "Altura", "IMC", "Categoria_IMC")]
print(datosNC)

#F2
datosDrogas <- datosPacientesOrd[datosPacientesOrd$Consume_Drogas != "*", ]
datosDrogas <- datosDrogas[order(datosDrogas$Nombre), ]
datosDrogas <- datosDrogas[substring(datosDrogas$Nombre, 1, 1) >= "A" & substring(datosDrogas$Nombre, 1, 1) <= "N", ]
datosDrogas <- datosDrogas[, c("Nombre", "Consume_Drogas", "IMC", "Categoria_IMC")]
print(datosDrogas)
