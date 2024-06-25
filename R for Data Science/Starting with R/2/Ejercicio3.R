#Ejercicio 3

#Cargar los datos
ruta <- file.choose()
datos <- read_excel(ruta)

#Apartado A

muestra_A <- sample(datos$`GÉNERO (1=MASCULINO)`, 15, replace = FALSE)

#Apartado B

muestra_B <- sample(datos$`PULSO (BPM)`, 30, replace = TRUE)

#Apartado C

muestra_C <- sample(datos$`PESO (kg)`, 50, replace = TRUE) 
