#Práctica 1 MGB



#EJERCICIO 4

#Instalación del paquete
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Rgraphviz")

#Apartado A
#dag
variables <- c("E", "S", "F", "O", "C", "M")
dag <- empty.graph(variables)

dag <- set.arc(dag, from = "E", to = "F")
dag <- set.arc(dag, from = "S", to = "F")
dag <- set.arc(dag, from = "F", to = "C")
dag <- set.arc(dag, from = "F", to = "O")
dag <- set.arc(dag, from = "O", to = "M")
dag <- set.arc(dag, from = "C", to = "M")

#Crear 5 dag distintos
graphviz.plot(dag, layout = "dot", shape = "ellipse", main = "DAG con layout = 'dot' y shape = 'ellipse'")
graphviz.plot(dag, layout = "fdp", shape = "circle", main = "DAG con layout = 'fdp' y shape = 'circle'")
graphviz.plot(dag, layout = "twopi", shape = "rectangle", main = "DAG con layout = 'twopi' y shape = 'rectangle'")
graphviz.plot(dag, layout = "circo", shape = "circle", main = "DAG con layout = 'circo' y shape = 'circle'")
graphviz.plot(dag, layout = "neato", shape = "ellipse", main = "DAG con layout = 'neato' y shape = 'ellipse'")

#Apartado B
#Listado de personalizacion del grafico dag
esteticaDag <- list(
  nodes = nodes(dag),  
  arcs = as.matrix(arcs(dag)),  
  col = "red", 
  fill = "purple",  
  textCol = "white", 
  lwd = 2,  
  lty = "dashed"  
)
graphviz.plot(dag, layout = "dot", shape = "ellipse", main = "DAG Personalizado", highlight = esteticaDag)


#Apartado C
library("Rgraphviz")
plotDag <- graphviz.plot(dag)

edgeRenderInfo(plotDag) <- list(
  col = c("E~F" = "aquamarine", "F~C" = "aquamarine", "C~M" = "aquamarine",
          "S~F" = "bisque3", "F~O" = "bisque3", "O~M" = "bisque3"),
  lwd = c("E~F" = 3, "F~C" = 3, "C~M" = 3,
          "S~F" = 2, "F~O" = 2, "O~M" = 2),
  lty = c("E~F" = "dashed", "F~C" = "dashed", "C~M" = "dashed",
          "S~F" = "solid", "F~O" = "solid", "O~M" = "solid")
)
nodeRenderInfo(plotDag) <- list(
  col = c("E" = "coral1", "S" = "coral1",
          "F" = "chocolate3",
          "O" = "chartreuse", "C" = "chartreuse",
          "M" = "deeppink"),
  fill = c("E" = "darkslategray1", "S" = "darkslategray1",
           "F" = "#F0FFF0",
           "O" = "#EEEEE0", "C" = "#EEEEE0",
           "M" = "lemonchiffon2"),
  textCol = c("E" = "#FFFFE0", "S" = "#FFFFE0",
              "F" = "#9AFF9A",
              "O" = "purple", "C" = "purple",
              "M" = "#FFFAFA")
)
renderGraph(plotDag)


#Apartado D
#Creación de la BN - igual que en el ejercicio 3
library(readxl)
ruta <- file.choose()
encuesta <- read_excel(ruta)
library(dplyr)
encuesta <- as.data.frame(encuesta)
help("mutate_if")
encuesta <- encuesta %>%
  mutate_if(is.character, as.factor)
bn <- bn.fit(dag, encuesta)

#Representación visual de las CPTs
bn.fit.barchart(bn$E, main = "CPT de Edad (E)",
                xlab = "P(E)", ylab = "")
bn.fit.dotplot(bn$E, main = "CPT de Edad (E)",
               xlab = "P(E)", ylab = "")

bn.fit.barchart(bn$S, main = "CPT de Sexo (S)",
                xlab = "P(S)", ylab = "")
bn.fit.dotplot(bn$S, main = "CPT de Sexo (S)",
               xlab = "P(S)", ylab = "")

bn.fit.barchart(bn$F, main = "CPT de Formación (F)",
                xlab = "P(F)", ylab = "")
bn.fit.dotplot(bn$F, main = "CPT de Formación (F)",
               xlab = "P(F)", ylab = "")

bn.fit.barchart(bn$O, main = "CPT de Ocupación (O)",
                xlab = "P(O)", ylab = "")
bn.fit.dotplot(bn$O, main = "CPT de Ocupación (O)",
               xlab = "P(O)", ylab = "")

bn.fit.barchart(bn$C, main = "CPT de Ciudad (C)",
                xlab = "P(C)", ylab = "")
bn.fit.dotplot(bn$C, main = "CPT de Ciudad (C)",
               xlab = "P(C)", ylab = "")

bn.fit.barchart(bn$M, main = "CPT de Medio (M)",
                xlab = "P(M)", ylab = "")
bn.fit.dotplot(bn$M, main = "CPT de Medio (M)",
               xlab = "P(M)", ylab = "")




