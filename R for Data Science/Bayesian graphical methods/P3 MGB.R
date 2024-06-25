#Práctica 3 MGB




#EJERCICIO 1
#Creación de las CPTs
#Dificultad
cptD <- array(c(0.6, 0.4), 
              dim = 2, 
              dimnames = list(c("d0","d1")))

#Inteligencia
cptI <- array(c(0.7,0.3), 
              dim = 2, 
              dimnames = list(c("i0","i1")))

#Nota
cptG <- array(c(0.3,0.4,0.3,
                0.05,0.25,0.7,
                0.9,0.08,0.02,
                0.5,0.3,0.2),
              dim = c(3, 2, 2), 
              dimnames = list(G = c("g1","g2","g3"),
                              D = c("d0","d1"),
                              I = c("i0","i1")))

#SAT
cptS <- array(c(0.95,0.05,0.2,0.8),
              dim = c(2, 2), 
              dimnames = list(S = c("s0", "s1"),
                              I = c("i0","i1")))

#Carta
cptL <- array(c(0.1,0.9,0.4,0.6,0.99,0.01),
              dim = c(2, 3), 
              dimnames = list(L = c("l0", "l1"),
                              G = c("g1","g2","g3")))

#Creación de la lista
cpts <- list()
cpts$D <- cptD
cpts$I <- cptI
cpts$G <- cptG
cpts$S <- cptS
cpts$L <- cptL

print(cpts)



#EJERCICIO 2
#Función EsSubconjunto
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

#Función CalcularProbabilidad
CalcularProbabilidad <- function(x,Y,cpt){
  prob <- NA
  indicex <- match(x,dimnames(cpt)[[1]])
  
  if(!is.na(indicex)&(length(dim(cpt))-1)==length(Y)){
    if(length(Y)==0){
      prob <- cpt[indicex]
    }else if(length(Y)==1){
      indicey <- match(Y,colnames(cpt))
      if(!is.na(indicey)){
        prob <- cpt[indicex,indicey]
      }
    }else if(length(Y)==2){
      cptFiltrada <- cpt[indicex,,]
      fil <- match(Y[1],rownames(cptFiltrada))
      col <- match(Y[2],colnames(cptFiltrada))
      if(is.na(fil)){
        fil <- match(Y[2],rownames(cptFiltrada))
        col <- match(Y[1],colnames(cptFiltrada))
      }
      if(!is.na(fil)&!is.na(col)){
        prob <- cptFiltrada[fil,col]
      }
    }
  }
  return(unname(prob))
}


library("rlist")

#Función ObtenerPadres
ObtenerPadres <- function(X,arcos){
  n <- length(X)
  padres <- list()
  for(i in 1:n){
    padresXi <- c()
    for(j in 1:nrow(arcos)){
      if(X[i]==arcos[j,2]){
        padresXi <- c(padresXi, arcos[j,1])
      }
    }
    padres <- list.append(padres,padresXi)
  }
  names(padres) <- X
  return(padres)
}

#Función ObtenerNivelesPadres
ObtenerNivelesPadres <- function(padresX, muestra){
  nivelesPadres <- character(0)
  varMuestra <- toupper(substr(muestra, 1, 1))
  indicesPadres <- match(padresX,varMuestra)
  indicesPadres <- sort(indicesPadres)
  nivelesPadres <- muestra[indicesPadres]
  return(nivelesPadres)
}


#APARTADO A
#La función CalcularProbabilidad() obtiene la probabilidad condicionada a partir de una tabla de probabilidades conjuntas.
#Los argumentos de entrada de la función son:
# x - la variable de la que se quiere obtener la probabilidad.
# Y - vector de variables (cero, una o más) con respecto a las cuales se condiciona la probabilidad.
# cpt - la tabla con la probabilidad conjunta.

x <- "d1"
Y <- character(0)
cpt <- cpts[["D"]]
CalcularProbabilidad(x, Y, cpt)

x <- "s1"
Y <- "i0" 
cpt <- cpts[["S"]]
CalcularProbabilidad(x, Y, cpt)

x <- "g3"
Y <- c("d0","i1")
cpt <- cpts[["G"]]
CalcularProbabilidad(x, Y, cpt)

#¿Por qué crees que la función devuelve NA (Not Available) al probar con estos parámetros de entrada?
x <- "d5" 
Y <- character(0) 
cpt <- cpts[["D"]]
CalcularProbabilidad(x, Y, cpt)

x <- "s1"
Y <- "i0"
cpt <- cpts[["G"]]
CalcularProbabilidad(x, Y, cpt)

x <- "g2"
Y <- "d0" 
cpt <- cpts[["G"]]
CalcularProbabilidad(x, Y, cpt)

#Obtenemos NA como resultado debido a que se están introduciendo valores de elementos que no se encuentran en la tabla
#  de probabilidades conjuntas (los dos primeros casos) o faltan datos para obtener la probabilidad (el tercer caso).
#1- No existe d5 en D
#2- No existe s1 en G
#3- Falta concretar además de g2 y d0, en Y, si está condicionado por i0 o i1


#El mayor número de padres que puede tener la cpt introducida es 2, debido a que el elemento que cuenta con mayor número de
# padres es G (Nota), y cuenta con dos.


#APARTADO B
X <- c("D", "I", "G", "S", "L")

arcos <- matrix(c("D", "G",
                  "I", "G",
                  "I", "S",
                  "G", "L"), byrow=TRUE, ncol = 2)

padres <- ObtenerPadres(X, arcos)
print(padres)

#APARTADO C
#El cometido de la función ObtenerNivelesPadres() es devolver los niveles correspondientes a las variables padres de un 
# elemento para una muestra dada.
padresX <- padres[["D"]]
muestra <- c("d0","i1","g3","s0","l1")
ObtenerNivelesPadres(padresX,muestra)

padresX <- padres[["G"]]
muestra <- c("d0","i1","g3","s0","l1")
ObtenerNivelesPadres(padresX,muestra)       

padresX <- padres[["S"]]
muestra <- c("d1","i0","g2","s1")
ObtenerNivelesPadres(padresX,muestra)

padresX <- padres[["L"]]
muestra <- c("g2","i0","s1","d1") 
ObtenerNivelesPadres(padresX,muestra)



#EJERCICIO 3
AlgoritmoLW <- function(cpts, arcos, e){
  w <- 1
  X <- names(cpts)
  n <- length(X)
  padres <- ObtenerPadres(X, arcos)
  muestra <- character(0)
  
  for (i in 1:n) {
    cpt <- cpts[[X[i]]]
    nivelesX <- dimnames(cpt)[[1]]
    u <- ObtenerNivelesPadres(padres[[X[i]]], muestra)
    interseccion <- intersect(nivelesX, e)
    
    if (length(interseccion) == 0) {
      probX <- numeric(0)
      for (j in 1:length(nivelesX)) {
        prob <- CalcularProbabilidad(nivelesX[j], u, cpt)
        probX <- c(probX, prob)
      }
      
      xi <- sample(nivelesX, size = 1, prob = probX)
      muestra <- c(muestra, xi)
      
    } else {
      xi <- interseccion
      muestra <- c(muestra, xi)
      w <- w * CalcularProbabilidad(xi, u, cpt)
    }
  }
  
  muestraPonderada <- list(muestra = muestra, peso = w)
  
  return(muestraPonderada)
  
}

e <- c("d1")
AlgoritmoLW(cpts, arcos, e)

e <- c("i1")
AlgoritmoLW(cpts, arcos, e)

e <- c("d0","i0")
AlgoritmoLW(cpts, arcos, e)

e <- c("l0","s0")
AlgoritmoLW(cpts, arcos, e)

e <- c("d0","g3","s1")
AlgoritmoLW(cpts, arcos, e)

e <- c("d1","i0","g2","s0")
AlgoritmoLW(cpts, arcos, e)

e <- c("i1","g1")
AlgoritmoLW(cpts, arcos, e)



#EJERCICIO 4
InferenciaAproxLW <- function(y, e, M) {
  pesosEvento <- 0
  pesosSumatorio <- 0
  for (m in 1:M) {
    muestraPonderada <- AlgoritmoLW(cpts, arcos, e)
    
    xm <- muestraPonderada$muestra
    wm <- muestraPonderada$peso
    contiene <- 0
    
    if (EsSubconjunto(y, xm)) {
      contiene <- 1
    }
    
    pesosEvento <- pesosEvento + (wm * contiene)
    pesosSumatorio <- pesosSumatorio + wm
    
  }
  
  inferenciaAprox <- pesosEvento / pesosSumatorio
  
  return(inferenciaAprox)
}

#Test de la función
y <- c("g3")
e <- c("l0", "s0")
M <- 1000
print(InferenciaAproxLW(y,e,M))
M <- 5000
print(InferenciaAproxLW(y,e,M))
M <- 10000
print(InferenciaAproxLW(y,e,M))
#Devuelve un valor cercano a 0.733

y <- c("l1")
e <- c("d1", "i0")
M <- 1000
print(InferenciaAproxLW(y,e,M))
#Devuelve un valor cercano a 0.202

y <- c("g3","l1")
e <- c("d1", "i0")
M <- 1000
print(InferenciaAproxLW(y,e,M))
#Devuelve un valor cercano a 0.007



#EJERCICIO 5
#APARTADO A
library("bnlearn")

dag <- model2network("[D][I][G|D:I][S|I][L|G]")
print(dag)

bn <- custom.fit(dag, cpts)
print(bn)

#Cálculo de inferencias aproximadas del ejercicio 4
#y <- c("g3")
#e <- c("l0", "s0")
#M <- 1000
cpquery(bn, event = (G == "g3"),
        evidence = list(L = "l0", S = "s0"),
        method = "lw",
        n = 1000)
cpquery(bn, event = (G == "g3"),
        evidence = list(L = "l0", S = "s0"),
        method = "lw",
        n = 5000)
cpquery(bn, event = (G == "g3"),
        evidence = list(L = "l0", S = "s0"),
        method = "lw",
        n = 10000)
#Devuelve un valor cercano a 0.733

#y <- c("l1")
#e <- c("d1", "i0")
#M <- 1000
cpquery(bn, event = (L == "l1"),
        evidence = list(D = "d1", I = "i0"),
        method = "lw",
        n = 1000)
#Devuelve un valor cercano a 0.202

#y <- c("g3","l1")
#e <- c("d1", "i0")
#M <- 1000
cpquery(bn, event = (G == "g3" & L == "l1"),
        evidence = list(D = "d1", I = "i0"),
        method = "lw",
        n = 1000)
#Devuelve un valor cercano a 0.007


#APARTADO B
library("gRain")
library("RBGL")
junction <- compile(as.grain(bn))
#Cálculo de inferencias exactas del ejercicio 4
#y <- c("g3")
#e <- c("l0", "s0")
#M <- 1000
evidencia <- setEvidence(junction, evidence=list(L = "l0", S = "s0"))
querygrain(evidencia, nodes = c("G"), type = "joint")["g3"]
#Devuelve 0.733

#y <- c("l1")
#e <- c("d1", "i0")
#M <- 1000
evidencia <- setEvidence(junction, evidence=list(D = "d1", I = "i0"))
querygrain(evidencia, nodes = c("L"), type = "joint")["l1"]
#Devuelve 0.202

#y <- c("g3","l1")
#e <- c("d1", "i0")
#M <- 1000
evidencia <- setEvidence(junction, evidence=list(D = "d1", I = "i0"))
querygrain(evidencia, nodes = c("G","L"), type = "joint")["g3","l1"]
#Devuelve 0.007




