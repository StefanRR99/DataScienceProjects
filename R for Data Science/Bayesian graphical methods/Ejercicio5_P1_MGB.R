#Práctica 1 MGB



#EJERCICIO 5
nodos <- c("E","S","F","O","C", "M")
niveles <- list(E=c("joven","adulto","anciano"),
                S=c("M","F"),
                F=c("secundaria","universidad"),
                O=c("empleado","autónomo"),
                C=c("pequeña","grande"),
                M=c("coche","tren","otro"))
arcos <- matrix(c("E","F",
                  "S","F",
                  "F","O",
                  "F","C",
                  "O","M",
                  "C","M"), byrow=TRUE, ncol = 2)

#Implementacion de la ecuacion (6).
calcularParametros <- function(nodos, niveles, arcos) {
  parametros = 0
  for (nodo in nodos) {
    padres = arcos[arcos[,2] == nodo, 1]
    num_categorias_nodo = length(niveles[[nodo]]) - 1
    producto_categorias_padres = 1
    for (padre in padres) {
      producto_categorias_padres = producto_categorias_padres * length(niveles[[padre]])
    }
    parametros = parametros + (num_categorias_nodo * producto_categorias_padres)
  }
  return(parametros)
}

numeroParam = calcularParametros(nodos, niveles, arcos)
print(paste("Parámetros de las distribuciones locales de la BN: ", numeroParam))

#BN burglary network
nodos2 <- c("R","T","A","J","M")

niveles2 <- list(R=c("V","F"),
                T=c("V","F"),
                A=c("V","F"),
                J=c("V","F"),
                M=c("V","F"))

arcos2 <- matrix(c("R","A",
                  "T","A",
                  "A","J",
                  "A","M"), byrow=TRUE, ncol = 2)

numeroParam2 = calcularParametros(nodos2, niveles2, arcos2)
print(paste("Parámetros de las distribuciones locales de la BN: ", numeroParam2))







