#Práctica 1 MGB



#EJERCICIO 3

#Apartado A
library(readxl)
ruta <- file.choose()
encuesta <- read_excel(ruta)
View(encuesta)

edad <- factor(encuesta$E,levels=c("joven","adulto","anciano"))
sexo <- factor(encuesta$S, levels = c("M", "F"))
formacion <- factor(encuesta$F, levels = c("secundaria", "universidad"))
ocupacion <- factor(encuesta$O, levels = c("empleado", "autonomo"))
ciudad <- factor(encuesta$C, levels = c("pequeña", "grande"))
medio <- factor(encuesta$M, levels = c("coche", "tren", "otro"))

cptE <- array(c(length(which(encuesta$E == "joven")) / length(encuesta$E),
                 length(which(encuesta$E == "adulto")) / length(encuesta$E),
                 length(which(encuesta$E == "anciano")) / length(encuesta$E)),
               dim = 3,
               dimnames = list(E = levels(edad)))
print(cptE)

cptS <- array(c(length(which(encuesta$S == "M")) / length(encuesta$S),
                length(which(encuesta$S == "F")) / length(encuesta$S)),
              dim = 2,
              dimnames = list(S = levels(sexo)))
print(cptS)

cptF <- array(c(
    (length(which(encuesta$F == "secundaria" & encuesta$E == "joven" & encuesta$S == "M")) / length(encuesta$F)) / (length(which(encuesta$E == "joven" & encuesta$S == "M")) / length(encuesta$F)),
    (length(which(encuesta$F == "universidad" & encuesta$E == "joven" & encuesta$S == "M")) / length(encuesta$F)) / (length(which(encuesta$E == "joven" & encuesta$S == "M")) / length(encuesta$F)),
    (length(which(encuesta$F == "secundaria" & encuesta$E == "adulto" & encuesta$S == "M")) / length(encuesta$F)) / (length(which(encuesta$E == "adulto" & encuesta$S == "M")) / length(encuesta$F)),
    (length(which(encuesta$F == "universidad" & encuesta$E == "adulto" & encuesta$S == "M")) / length(encuesta$F)) / (length(which(encuesta$E == "adulto" & encuesta$S == "M")) / length(encuesta$F)),
    (length(which(encuesta$F == "secundaria" & encuesta$E == "anciano" & encuesta$S == "M")) / length(encuesta$F)) / (length(which(encuesta$E == "anciano" & encuesta$S == "M")) / length(encuesta$F)),
    (length(which(encuesta$F == "universidad" & encuesta$E == "anciano" & encuesta$S == "M")) / length(encuesta$F)) / (length(which(encuesta$E == "anciano" & encuesta$S == "M")) / length(encuesta$F)),
    
    (length(which(encuesta$F == "secundaria" & encuesta$E == "joven" & encuesta$S == "F")) / length(encuesta$F)) / (length(which(encuesta$E == "joven" & encuesta$S == "F")) / length(encuesta$F)),
    (length(which(encuesta$F == "universidad" & encuesta$E == "joven" & encuesta$S == "F")) / length(encuesta$F)) / (length(which(encuesta$E == "joven" & encuesta$S == "F")) / length(encuesta$F)),
    (length(which(encuesta$F == "secundaria" & encuesta$E == "adulto" & encuesta$S == "F")) / length(encuesta$F)) / (length(which(encuesta$E == "adulto" & encuesta$S == "F")) / length(encuesta$F)),
    (length(which(encuesta$F == "universidad" & encuesta$E == "adulto" & encuesta$S == "F")) / length(encuesta$F)) / (length(which(encuesta$E == "adulto" & encuesta$S == "F")) / length(encuesta$F)),
    (length(which(encuesta$F == "secundaria" & encuesta$E == "anciano" & encuesta$S == "F")) / length(encuesta$F)) / (length(which(encuesta$E == "anciano" & encuesta$S == "F")) / length(encuesta$F)),
    (length(which(encuesta$F == "universidad" & encuesta$E == "anciano" & encuesta$S == "F")) / length(encuesta$F)) / (length(which(encuesta$E == "anciano" & encuesta$S == "F")) / length(encuesta$F))
  ), dim = c(2, 3, 2),
      dimnames = list(F = levels(formacion),
                      E = levels(edad),
                      S = levels(sexo)
                        ))
print(cptF)

cptO <- array(c(
  (length(which(encuesta$F == "secundaria" & encuesta$O == "empleado")) / length(encuesta$F)) / (length(which(encuesta$F == "secundaria" )) / length(encuesta$F)),
  (length(which(encuesta$F == "secundaria" & encuesta$O == "autónomo")) / length(encuesta$F)) / (length(which(encuesta$F == "secundaria")) / length(encuesta$F)),
  (length(which(encuesta$F == "universidad" & encuesta$O == "empleado")) / length(encuesta$F)) / (length(which(encuesta$F == "universidad")) / length(encuesta$F)),
  (length(which(encuesta$F == "universidad" & encuesta$O == "autónomo")) / length(encuesta$F)) / (length(which(encuesta$F == "universidad")) / length(encuesta$F))
  ),
  dim = c(2, 2),
  dimnames = list(O = levels(ocupacion),
                  F = levels(formacion)
                  )
)

print(cptO)

cptC <- array(c(
  (length(which(encuesta$F == "secundaria" & encuesta$C == "pequeña")) / length(encuesta$F)) / (length(which(encuesta$F == "secundaria" )) / length(encuesta$F)),
  (length(which(encuesta$F == "secundaria" & encuesta$C == "grande")) / length(encuesta$F)) / (length(which(encuesta$F == "secundaria")) / length(encuesta$F)),
  (length(which(encuesta$F == "universidad" & encuesta$C == "pequeña")) / length(encuesta$F)) / (length(which(encuesta$F == "universidad")) / length(encuesta$F)),
  (length(which(encuesta$F == "universidad" & encuesta$C == "grande")) / length(encuesta$F)) / (length(which(encuesta$F == "universidad")) / length(encuesta$F))
  ),
  dim = c(2, 2),
  dimnames = list(C = levels(ciudad),
                  F = levels(formacion)
                  )
)

print(cptC)

cptM <- array(c(
  (length(which(encuesta$M == "coche" & encuesta$O == "empleado" & encuesta$C == "pequeña")) / length(encuesta$F)) / (length(which(encuesta$O == "empleado" & encuesta$C == "pequeña")) / length(encuesta$F)),
  (length(which(encuesta$M == "tren" & encuesta$O == "empleado" & encuesta$C == "pequeña")) / length(encuesta$F)) / (length(which(encuesta$O == "empleado" & encuesta$C == "pequeña")) / length(encuesta$F)),
  (length(which(encuesta$M == "otro" & encuesta$O == "empleado" & encuesta$C == "pequeña")) / length(encuesta$F)) / (length(which(encuesta$O == "empleado" & encuesta$C == "pequeña")) / length(encuesta$F)),
  
  (length(which(encuesta$M == "coche" & encuesta$O == "empleado" & encuesta$C == "grande")) / length(encuesta$F)) / (length(which(encuesta$O == "empleado" & encuesta$C == "grande")) / length(encuesta$F)),
  (length(which(encuesta$M == "tren" & encuesta$O == "empleado" & encuesta$C == "grande")) / length(encuesta$F)) / (length(which(encuesta$O == "empleado" & encuesta$C == "grande")) / length(encuesta$F)),
  (length(which(encuesta$M == "otro" & encuesta$O == "empleado" & encuesta$C == "grande")) / length(encuesta$F)) / (length(which(encuesta$O == "empleado" & encuesta$C == "grande")) / length(encuesta$F)),
  
  (length(which(encuesta$M == "coche" & encuesta$O == "autónomo" & encuesta$C == "pequeña")) / length(encuesta$F)) / (length(which(encuesta$O == "autónomo" & encuesta$C == "pequeña")) / length(encuesta$F)),
  (length(which(encuesta$M == "tren" & encuesta$O == "autónomo" & encuesta$C == "pequeña")) / length(encuesta$F)) / (length(which(encuesta$O == "autónomo" & encuesta$C == "pequeña")) / length(encuesta$F)),
  (length(which(encuesta$M == "otro" & encuesta$O == "autónomo" & encuesta$C == "pequeña")) / length(encuesta$F)) / (length(which(encuesta$O == "autónomo" & encuesta$C == "pequeña")) / length(encuesta$F)),
  
  (length(which(encuesta$M == "coche" & encuesta$O == "autónomo" & encuesta$C == "grande")) / length(encuesta$F)) / (length(which(encuesta$O == "autónomo" & encuesta$C == "grande")) / length(encuesta$F)),
  (length(which(encuesta$M == "tren" & encuesta$O == "autónomo" & encuesta$C == "grande")) / length(encuesta$F)) / (length(which(encuesta$O == "autónomo" & encuesta$C == "grande")) / length(encuesta$F)),
  (length(which(encuesta$M == "otro" & encuesta$O == "autónomo" & encuesta$C == "grande")) / length(encuesta$F)) / (length(which(encuesta$O == "autónomo" & encuesta$C == "grande")) / length(encuesta$F))
  ),
  dim = c(3, 2, 2),
  dimnames = list(M = levels(medio),
                  C = levels(ciudad),
                  O = levels(ocupacion)
                  )
)
print(cptM)

listadoCPT <- list(E = cptE, S = cptS, F = cptF, O = cptO, C = cptC, M = cptM)


#Apartado B
library(bnlearn)

#Creación del grafo
variables <- c("E", "S", "F", "O", "C", "M")
dag <- empty.graph(variables)

dag <- set.arc(dag, from = "E", to = "F")
dag <- set.arc(dag, from = "S", to = "F")
dag <- set.arc(dag, from = "F", to = "C")
dag <- set.arc(dag, from = "F", to = "O")
dag <- set.arc(dag, from = "O", to = "M")
dag <- set.arc(dag, from = "C", to = "M")

#Creación de la BN
bn <-  custom.fit(dag, listadoCPT)
print(bn)
#Son idénticas a las del ejercicio 1
#bn$F

#¿Qué función de bnlearn permite calcular el número de parámetros de las distribuciones locales?
nparams(bn)

#¿Y el número total de nodos y arcos?
num_nodos <- length(nodes(dag))
num_arcos <- nrow(arcs(dag))


#Apartado C
otraBn <- bn.fit(dag, encuesta)
#Da error porque parece que los datos de la encuesta los trata como caracteres

encuesta <- as.data.frame(encuesta)
library(dplyr)
#help("mutate_if")
encuesta <- encuesta %>% mutate_if(is.character, as.factor)
otraBn <- bn.fit(dag, encuesta)
print(otraBn)
modelstring(otraBn)
