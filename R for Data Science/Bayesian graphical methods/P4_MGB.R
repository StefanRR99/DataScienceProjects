#Practica 4 MGB


#Ejercicio 3 P4
library("Rgraphviz")
library("bnlearn")

#Apartado A
variables <- c("A", "S", "T", "L", "B", "E", "X", "D")
dag <- empty.graph(variables)

dag <- set.arc(dag, from = "A", to = "T")
dag <- set.arc(dag, from = "S", to = "L")
dag <- set.arc(dag, from = "S", to = "B")
dag <- set.arc(dag, from = "T", to = "E")
dag <- set.arc(dag, from = "L", to = "E")
dag <- set.arc(dag, from = "B", to = "D")
dag <- set.arc(dag, from = "E", to = "X")
dag <- set.arc(dag, from = "E", to = "D")

graphviz.plot(dag, layout = "dot", shape = "ellipse")

tres_a <- moral(dag)
graphviz.plot(tres_a, layout = "dot", shape = "ellipse", main = "DAG Moral")

#Apartado B
variables2 <- c("A", "S", "T", "L", "B", "E", "D")
dag2 <- empty.graph(variables2)

dag2 <- set.arc(dag2, from = "A", to = "T")
dag2 <- set.arc(dag2, from = "S", to = "L")
dag2 <- set.arc(dag2, from = "S", to = "B")
dag2 <- set.arc(dag2, from = "T", to = "E")
dag2 <- set.arc(dag2, from = "L", to = "E")
dag2 <- set.arc(dag2, from = "B", to = "D")
dag2 <- set.arc(dag2, from = "E", to = "D")
tres_b <- moral(dag2)
graphviz.plot(tres_b, layout = "dot", shape = "ellipse", main = "DAG moralizado (T, D, S).")


#Apartado C - una vez obtenido el Moralizado (T, D, S), lo escribimos a mano para calcular la D-separación.
variables3 <- c("A", "S", "T", "L", "B", "E", "D")
dag3 <- empty.graph(variables3)

dag3 <- set.edge(dag3, from = "A", to = "T")
dag3 <- set.edge(dag3, from = "S", to = "L")
dag3 <- set.edge(dag3, from = "S", to = "B")
dag3 <- set.edge(dag3, from = "T", to = "E")
dag3 <- set.edge(dag3, from = "L", to = "E")
dag3 <- set.edge(dag3, from = "B", to = "D")
dag3 <- set.edge(dag3, from = "E", to = "D")
dag3 <- set.edge(dag3, from = "T", to = "L")
dag3 <- set.edge(dag3, from = "E", to = "B")

resultado_dseparacion <- dsep(dag3, "T", "D", "S")
print(resultado_dseparacion)
