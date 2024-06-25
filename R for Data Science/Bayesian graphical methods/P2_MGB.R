#Practica 2 MGB



#EJERCICIO 1
distGlobal <- read.delim(file.choose(), header = TRUE, sep = "\t", row.names = NULL, dec = ".")
View(distGlobal)
# tipo data.frame 
print(class(distGlobal))
# todas tipo character y la probabilidad numeric
print(sapply(distGlobal, class))


#EJERCICIO 2
# Función EsSubconjunto para verificar si A es subconjunto estricto de B
EsSubconjunto <- function(A, B) {
  A <- as.character(A)
  B <- as.character(B)
  
  if (length(A) == 0) {
    return(TRUE)
  }
  if (setequal(A, B)) {
    return(FALSE)
  }

  return(setequal(A,intersect(A,B)))
}

# Test
print(EsSubconjunto(c("a", "b", "c"), c("a", "b", "c", "d"))) # TRUE
print(EsSubconjunto(c("A", "b", "c"), c("a", "b", "c", "d"))) # FALSE
print(EsSubconjunto(c("a", "b", "c", "d"), c("a", "b", "c", "d"))) # FALSE
print(EsSubconjunto(character(0), c("a", "b", "c", "d"))) # TRUE


#EJERCICIO 3
# Función EsCondIndependiente
EsCondIndependiente <- function(P, X, Y, Z) {
  library("dplyr")
  library("rlist")
  
  # 1-Asumimos independencia condicional
  independiente <- TRUE
  
  # 2-Índice de la columna X
  indiceX <- match(X, colnames(P))
  
  # 3-Niveles de X
  nivelesX <- levels(as.factor(P[[indiceX]]))
  #print(nivelesX)

  # 4-Nueva lista de niveles
  niveles <- list()
  niveles <- list.append(list(nivelesX))
  
  # 5-Índice de las columnas Y y Z
  indicesY <- match(Y, colnames(P))
  indicesZ <- match(Z, colnames(P))
  
  # 6-Niveles de Y y Z
  for (i in indicesY) {
    niveles <- list.append(niveles, levels(as.factor(P[[i]])))
  }
  
  for (i in indicesZ) {
    niveles <- list.append(niveles, levels(as.factor(P[[i]])))
  }

  # 7-Combinaciones
  combinaciones <- expand.grid(niveles, stringsAsFactors = FALSE)

  # 8-Verificar si se niega independiente
  for (i in 1:nrow(combinaciones)) {
    
    if (length(Z) == 0) {
      probXY <- 0
      probX <- 0
      probY <- 0
      
      vectorXY <- as.character(combinaciones[i, ])
      vectorX <- as.character(vectorXY[1])
      vectorY <- as.character(vectorXY[2:length(vectorXY)])
      
      for(j in 1:nrow(P)){
        filaP <- P[j, ]
        prob <- as.numeric(filaP[length(filaP)])

        if(EsSubconjunto((vectorXY), (filaP))){
          probXY <- probXY + prob
        }

        if(EsSubconjunto((vectorX), (filaP))){
          probX <- probX + prob
        }
        
        if(EsSubconjunto((vectorY), (filaP))){
          probY <- probY + prob
        }
      }
      if(!near(probXY, probX * probY)){
        independiente <- FALSE
      }
    }else{
      probXYZ <- 0
      probZ <- 0
      probXZ <- 0
      probYZ <- 0
      
      vectorXYZ <- as.character(combinaciones[i, ])
      vectorZ <- as.character(vectorXYZ[(2 + length(Y)) : length(vectorXYZ)])
      vectorXZ <- as.character(list.append(vectorXYZ[1], vectorZ))
      vectorYZ <- as.character(vectorXYZ[-1])
      
      for(j in 1:nrow(P)){
        filaP <- P[j, ]
        prob <- as.numeric(filaP[length(filaP)])
        
        if(EsSubconjunto((vectorXYZ), (filaP))){
          probXYZ <- probXYZ + prob
        }
        
        if(EsSubconjunto((vectorZ), (filaP))){
          probZ <- probZ + prob
        }
        
        if(EsSubconjunto((vectorXZ), (filaP))){
          probXZ <- probXZ + prob
        }
        
        if(EsSubconjunto((vectorYZ), (filaP))){
          probYZ <- probYZ + prob
        }
        
      }
      if(!near(probXYZ * probZ, probXZ * probYZ)){
        independiente <- FALSE
      }
    }
  }
  return(independiente)
}

# Test con resultado TRUE
print(EsCondIndependiente(distGlobal, "D", c("I"), character(0))) # (D ⊥ I)
print(EsCondIndependiente(distGlobal, "D", c("S"), character(0))) # (D ⊥ S)
print(EsCondIndependiente(distGlobal, "G", c("S"), c("I", "D"))) # (G ⊥ S | I, D)
print(EsCondIndependiente(distGlobal, "S", c("D"), c("I"))) # (S ⊥ D | I)
print(EsCondIndependiente(distGlobal, "I", c("L"), c("G", "S"))) # (I ⊥ L | G, S)
print(EsCondIndependiente(distGlobal, "D", c("L", "S"), c("I", "G"))) # (D ⊥ L, S | I, G)

# Test con resultado FALSE
print(EsCondIndependiente(distGlobal, "G", c("L"), character(0))) # (G ⊥ L)
print(EsCondIndependiente(distGlobal, "G", c("S"), character(0))) # (G ⊥ S)
print(EsCondIndependiente(distGlobal, "S", c("L"), character(0))) # (S ⊥ L)
print(EsCondIndependiente(distGlobal, "S", c("L", "D", "G"), character(0))) # (S ⊥ L, D, G)
print(EsCondIndependiente(distGlobal, "I", c("S"), c("G", "L"))) # (I ⊥ S | G, L)
print(EsCondIndependiente(distGlobal, "I", c("G"), c("S", "L"))) # (I ⊥ G | S, L)
# Nuevos test para FALSE 
print(EsCondIndependiente(distGlobal, "I", c("G", "S"), c("L")))
print(EsCondIndependiente(distGlobal, "I", c("G", "L"), c("S")))
print(EsCondIndependiente(distGlobal, "I", c("L", "S"), c("D", "G")))


#EJERCICIO 4
library(DescTools) 
library(dplyr)

IMapaMinimal <- function(P, Xord) {
  n <- length(Xord) 
  vectorArcos <- character(0) 
  
  for (i in 2:n) {
    U <- Xord[1:(i - 1)] 
    for (j in (i - 1): 0) {
      matrizUprima <- CombSet(U, j)
      for (k in 1:nrow(matrizUprima)) {
        Uprima <- as.character(matrizUprima[k, ])
        Dif <- setdiff(U, Uprima)
        if (EsSubconjunto(Uprima, U)) {
          if (EsCondIndependiente(P, Xord[i], Dif, Uprima)) {
            U <- Uprima
            break
          }
        }
      }
    }
    for (padre in U) {
      relacion <- c(padre,Xord[i])
      vectorArcos <- append(vectorArcos,relacion)
    }
  }
  
  arcos <- matrix(vectorArcos, ncol = 2, byrow = TRUE)
  G <- list(variables = Xord, arcos = arcos)
  
  return(G)
}

# Validar
G1 <- IMapaMinimal(distGlobal, c("D","I","S","G","L"))
G2 <- IMapaMinimal(distGlobal, c("L","S","G","I","D"))
G3 <- IMapaMinimal(distGlobal, c("L","D","S","I","G"))

library(bnlearn)
library(Rgraphviz)

dag1 <- empty.graph(nodes = G1$variables)
arcs(dag1) <- G1$arcos
graphviz.plot(dag1, layout = "dot", shape = "circle", main = "DAG 1")


dag2 <- empty.graph(nodes = G2$variables)
arcs(dag2) <- G2$arcos
graphviz.plot(dag2, layout = "dot", shape = "circle", main = "DAG 2")


dag3 <- empty.graph(nodes = G3$variables)
arcs(dag3) <- G3$arcos
graphviz.plot(dag3, layout = "dot", shape = "circle", main = "DAG 3")
