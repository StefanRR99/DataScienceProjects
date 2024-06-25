#Ejercicio 8
#Apartado A
alcohol <- c("N", "S", "S", "N", "S", "N", "N", "S", "S", "S")
alcoholFactor <- factor(alcohol, levels = c("S", "N"), labels = c("Sí", "No"))

#Apartado B
fuma <- c(0, 1, 0, 1, 0, 0, 0, 1, 1, 1)
fumaFactor <- factor(fuma, levels = c(0, 1), labels = c("No Fuma", "Fuma"))
fumaFactor

#Apartado C
deporte <- c("POCO", "Nada", "Bastante", "NORMAL", "Normal", "Poco", "Normal", "BASTANTE", "Poco", "NADA")
deporteFactor <- factor(deporte)
deporteFactor
#El problema que ha ocurrido es que no se ha seguido un convenio o un protocolo para la encuesta ya que algunos han respondido en mayuscula otros en minuscula, y R al distinguir mayusculas de minusculas, cada opción descrita la detecta como un nuevo nivel.

#Apartado D
#Se deben modificar en el vector las mayusculas a minusculas, con la primera letra siempre en mayuscula.
deporte <- c("Poco", "Nada", "Bastante", "Normal", "Normal", "Poco", "Normal", "Bastante", "Poco", "Nada")
deporteFactor <- factor(deporte, levels = c("Normal", "Bastante", "Nada", "Poco"))
deporteFactor
plot(deporteFactor)
#Se muestra un gráfico sin una ordenación lógica.

#Apartado E
deporteFactorOrd <- factor(deporte, levels = c("Nada", "Poco", "Normal", "Bastante"), ordered = TRUE)
deporteFactorOrd
plot(deporteFactorOrd)
#Al ordenar el nuevo factor, ya se pueden obtener conclusiones fiables a partir del gráfico.

#Apartado F - Obtener los indices (lo mismo que el numero de encuestado) según diferentes preguntas.
#Resultado 1 - no beben ni fuman, y al menos, les gusta el deporte de forma normal.
indices1 <- which(alcoholFactor == "No" & fumaFactor == "No Fuma" & deporteFactorOrd >= "Normal")
print(indices1)

#Resultado 2 -  fuman, beben y, al menos, les gusta un poco el deporte
#indices2 <- which(alcoholFactor == "Sí" & fumaFactor == "Fuma" & deporteFactorOrd %in% c("Poco", "Normal", "Bastante"))
indices2 <- which(alcoholFactor == "Sí" & fumaFactor == "Fuma" & deporteFactorOrd >= "Poco")
print(indices2)

#Apartado G
str(alcohol)
str(alcoholFactor)
#Tras ejecutar estos comandos, observamos que el vector devuelve el listado de valores
#Mientras que el factor devuelve mucha mas información: el numero de niveles, las etiquetas asignadas y los valores ordenados.
#Asimismo, en el apartado anterior, se puede observar como los factores de etiquetas pueden tener un orden, util para realizar consultas con operadores < y >.
