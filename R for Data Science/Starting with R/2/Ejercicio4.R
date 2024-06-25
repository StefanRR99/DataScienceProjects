#Ejercicio 4

#Cargar los datos
ruta <- file.choose()
datos <- read_excel(ruta)

#Apartado A
#Creación de los datos
muestra_colesterol <- c(83, 99, 52, 112, 85, 109, 87, 77, 130, 98,
                        100, 82, 70, 140, 143, 116, 134, 142, 86, 119)

#Cálculo de las variables, con redondeo a 1 decimal
media_muestral <- round(mean(muestra_colesterol), 1)
varianza_muestral <- round(var(muestra_colesterol), 1)
desviacion_tipica_muestral <- round(sd(muestra_colesterol), 1)

#Impresión de los resultados
print(paste("Media muestral = " , media_muestral))
print(paste("Varianza muestral = " , varianza_muestral))
print(paste("Desviación típica muestral = " , desviacion_tipica_muestral))

#Apartado B
#Cálculo de la proporción muestral y redondeo a 3 decimales.
proporcion_muestralLDL <- length(which(muestra_colesterol < 100)) / length(muestra_colesterol)
proporcion_muestralLDL <- round(proporcion_muestralLDL, 3)

#Apartado C
#Cálculo de la media poblacional y redondeo a 1 decimal.
media_pobl <- round(mean(datos$`COL_LDL (mg/dL)`), 1)

#Apartado D
#Para calcular la varianza poblaccional, solo hay que multiplicar por (n/(n-1))
varianza_poblacional <- (length(muestra_colesterol) / (length(muestra_colesterol) - 1)) * varianza_muestral
varianza_poblacional <- round(varianza_poblacional, 1)

#Apartado E
#Para calcular la desviación típica poblacional unicamente hay que hacer la raiz cuadrada de la varianza poblacional.
desviacion_tipica_poblacional <- round(sqrt(varianza_poblacional), 1)

#Apartado F
#Cálculo de la proporción poblacional y redondeo a 3 decimales.
proporcion_poblacionalLDL <- length(which(datos$`COL_LDL (mg/dL)` > 130 & datos$`COL_LDL (mg/dL)` < 159)) / length(datos$`COL_LDL (mg/dL)`)
proporcion_poblacionalLDL <- round(proporcion_poblacionalLDL, 3)
