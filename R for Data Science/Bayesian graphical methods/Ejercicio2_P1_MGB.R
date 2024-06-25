#Práctica 1 MGB



#EJERCICIO 2

#Apartado A
#install.packages("bnlearn")
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

print(dag)

#Podemos confirmar que el dag es correcto ya que aparecen todos los nodos y los nodos de los que depende (o las condiciones): [E][S][F|E:S][O|F][C|F][M|O:C]


#Apartado B

#dag <- set.arc(dag, from = "C", to = "S")
#Muestra error, ya que esa relacion causaria crear un ciclo en el grafo y el Grafo Dirigido Acíclico por definición no puede tener ciclos.

#Funciones que obtienen nodos, arcos, padres e hijos del grafo definido.
nodes(dag)
arcs(dag)
parents(dag, "F")
children(dag, "F")

#Factorizacion de la distribucion global de la BN
factorizacion <- modelstring(dag)
print(factorizacion)
#Los datos obtenidos son idénticos al ejercicio 1


#Apartado C
otroDag <- empty.graph(variables)

arcos <- matrix(c("E", "F",
                  "S", "F",
                  "F", "C",
                  "F", "O",
                  "O", "M",
                  "C", "M"), byrow = TRUE, ncol = 2)

arcs(otroDag) <- arcos

all.equal(dag, otroDag)

print(otroDag)


#Apartado D
ultimoDag <- model2network("[E][S][F|E:S][O|F][C|F][M|O:C]")

print(ultimoDag)

all.equal(dag, otroDag, ultimoDag)


