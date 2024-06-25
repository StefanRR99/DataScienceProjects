#Práctica 1 MGB



#EJERCICIO 1

#Apartado A

#Nodo E - Hijo F
#Nodo S - Hijo F
#Nodo F - Padres (E, S), Hijos (C, O)
#Nodo O - Padre F, Hijo M
#Nodo C - Padre F, Hijo M
#Nodo M - Padres (C, O)

#      S
#      |
#      v
# E -> F -> C
#      |
#      v
#      O

#Apartado B - Contestada en el PDF

#Apartado C
#Cargar los datos
library(readxl)
ruta <- file.choose()
encuesta <- read_excel(ruta)
View(encuesta)

#Apartado D
edad <- factor(encuesta$E,levels=c("joven","adulto","anciano"))
sexo <- factor(encuesta$S, levels = c("M", "F"))
formacion <- factor(encuesta$F, levels = c("secundaria", "universidad"))
ocupacion <- factor(encuesta$O, levels = c("empleado", "autonomo"))
ciudad <- factor(encuesta$C, levels = c("pequeña", "grande"))
medio <- factor(encuesta$M, levels = c("coche", "tren", "otro"))

E <- c(length(which(encuesta$E == "joven")) / length(encuesta$E),
      length(which(encuesta$E == "adulto")) / length(encuesta$E),
      length(which(encuesta$E == "anciano")) / length(encuesta$E))
names(E) <- c("joven", "adulto", "anciano")

S <- c(length(which(encuesta$S == "M")) / length(encuesta$S),
        length(which(encuesta$S == "F")) / length(encuesta$S))
names(S) <- c("M", "F")

F <- matrix(c(
  (length(which(encuesta$F == "secundaria" & encuesta$E == "joven" & encuesta$S == "M")) / length(encuesta$F)) / (length(which(encuesta$E == "joven" & encuesta$S == "M")) / length(encuesta$F)),
  (length(which(encuesta$F == "secundaria" & encuesta$E == "joven" & encuesta$S == "F")) / length(encuesta$F)) / (length(which(encuesta$E == "joven" & encuesta$S == "F")) / length(encuesta$F)),
  (length(which(encuesta$F == "secundaria" & encuesta$E == "adulto" & encuesta$S == "M")) / length(encuesta$F)) / (length(which(encuesta$E == "adulto" & encuesta$S == "M")) / length(encuesta$F)),
  (length(which(encuesta$F == "secundaria" & encuesta$E == "adulto" & encuesta$S == "F")) / length(encuesta$F)) / (length(which(encuesta$E == "adulto" & encuesta$S == "F")) / length(encuesta$F)),
  (length(which(encuesta$F == "secundaria" & encuesta$E == "anciano" & encuesta$S == "M")) / length(encuesta$F)) / (length(which(encuesta$E == "anciano" & encuesta$S == "M")) / length(encuesta$F)),
  (length(which(encuesta$F == "secundaria" & encuesta$E == "anciano" & encuesta$S == "F")) / length(encuesta$F)) / (length(which(encuesta$E == "anciano" & encuesta$S == "F")) / length(encuesta$F)),
  
  (length(which(encuesta$F == "universidad" & encuesta$E == "joven" & encuesta$S == "M")) / length(encuesta$F)) / (length(which(encuesta$E == "joven" & encuesta$S == "M")) / length(encuesta$F)),
  (length(which(encuesta$F == "universidad" & encuesta$E == "joven" & encuesta$S == "F")) / length(encuesta$F)) / (length(which(encuesta$E == "joven" & encuesta$S == "F")) / length(encuesta$F)),
  (length(which(encuesta$F == "universidad" & encuesta$E == "adulto" & encuesta$S == "M")) / length(encuesta$F)) / (length(which(encuesta$E == "adulto" & encuesta$S == "M")) / length(encuesta$F)),
  (length(which(encuesta$F == "universidad" & encuesta$E == "adulto" & encuesta$S == "F")) / length(encuesta$F)) / (length(which(encuesta$E == "adulto" & encuesta$S == "F")) / length(encuesta$F)),
  (length(which(encuesta$F == "universidad" & encuesta$E == "anciano" & encuesta$S == "M")) / length(encuesta$F)) / (length(which(encuesta$E == "anciano" & encuesta$S == "M")) / length(encuesta$F)),
  (length(which(encuesta$F == "universidad" & encuesta$E == "anciano" & encuesta$S == "F")) / length(encuesta$F)) / (length(which(encuesta$E == "anciano" & encuesta$S == "F")) / length(encuesta$F))
  ),
  ncol = 2, nrow = 6,
  dimnames = list(c("joven & M", "joven & F", "adulto & M", "adulto & F", "anciano & M", "anciano & F"), c("secundaria", "universidad"))
)


O <- matrix(c(
  (length(which(encuesta$F == "secundaria" & encuesta$O == "empleado")) / length(encuesta$F)) / (length(which(encuesta$F == "secundaria" )) / length(encuesta$F)),
  (length(which(encuesta$F == "universidad" & encuesta$O == "empleado")) / length(encuesta$F)) / (length(which(encuesta$F == "universidad")) / length(encuesta$F)),
  (length(which(encuesta$F == "secundaria" & encuesta$O == "autónomo")) / length(encuesta$F)) / (length(which(encuesta$F == "secundaria")) / length(encuesta$F)),
  (length(which(encuesta$F == "universidad" & encuesta$O == "autónomo")) / length(encuesta$F)) / (length(which(encuesta$F == "universidad")) / length(encuesta$F))
  ),
  ncol = 2, nrow = 2,
  dimnames = list(c("secundaria", "universidad"), c("empleado", "autónomo"))
)

C <- matrix(c(
  (length(which(encuesta$F == "secundaria" & encuesta$C == "pequeña")) / length(encuesta$F)) / (length(which(encuesta$F == "secundaria" )) / length(encuesta$F)),
  (length(which(encuesta$F == "universidad" & encuesta$C == "pequeña")) / length(encuesta$F)) / (length(which(encuesta$F == "universidad")) / length(encuesta$F)),
  (length(which(encuesta$F == "secundaria" & encuesta$C == "grande")) / length(encuesta$F)) / (length(which(encuesta$F == "secundaria")) / length(encuesta$F)),
  (length(which(encuesta$F == "universidad" & encuesta$C == "grande")) / length(encuesta$F)) / (length(which(encuesta$F == "universidad")) / length(encuesta$F))
  ),
  ncol = 2, nrow = 2,
  dimnames = list(c("secundaria", "universidad"), c("pequeña", "grande"))
)


M <- matrix(c(
  (length(which(encuesta$M == "coche" & encuesta$O == "empleado" & encuesta$C == "pequeña")) / length(encuesta$F)) / (length(which(encuesta$O == "empleado" & encuesta$C == "pequeña")) / length(encuesta$F)),
  (length(which(encuesta$M == "coche" & encuesta$O == "empleado" & encuesta$C == "grande")) / length(encuesta$F)) / (length(which(encuesta$O == "empleado" & encuesta$C == "grande")) / length(encuesta$F)),
  (length(which(encuesta$M == "coche" & encuesta$O == "autónomo" & encuesta$C == "pequeña")) / length(encuesta$F)) / (length(which(encuesta$O == "autónomo" & encuesta$C == "pequeña")) / length(encuesta$F)),
  (length(which(encuesta$M == "coche" & encuesta$O == "autónomo" & encuesta$C == "grande")) / length(encuesta$F)) / (length(which(encuesta$O == "autónomo" & encuesta$C == "grande")) / length(encuesta$F)),

  (length(which(encuesta$M == "tren" & encuesta$O == "empleado" & encuesta$C == "pequeña")) / length(encuesta$F)) / (length(which(encuesta$O == "empleado" & encuesta$C == "pequeña")) / length(encuesta$F)),
  (length(which(encuesta$M == "tren" & encuesta$O == "empleado" & encuesta$C == "grande")) / length(encuesta$F)) / (length(which(encuesta$O == "empleado" & encuesta$C == "grande")) / length(encuesta$F)),
  (length(which(encuesta$M == "tren" & encuesta$O == "autónomo" & encuesta$C == "pequeña")) / length(encuesta$F)) / (length(which(encuesta$O == "autónomo" & encuesta$C == "pequeña")) / length(encuesta$F)),
  (length(which(encuesta$M == "tren" & encuesta$O == "autónomo" & encuesta$C == "grande")) / length(encuesta$F)) / (length(which(encuesta$O == "autónomo" & encuesta$C == "grande")) / length(encuesta$F)),

  (length(which(encuesta$M == "otro" & encuesta$O == "empleado" & encuesta$C == "pequeña")) / length(encuesta$F)) / (length(which(encuesta$O == "empleado" & encuesta$C == "pequeña")) / length(encuesta$F)),
  (length(which(encuesta$M == "otro" & encuesta$O == "empleado" & encuesta$C == "grande")) / length(encuesta$F)) / (length(which(encuesta$O == "empleado" & encuesta$C == "grande")) / length(encuesta$F)),
  (length(which(encuesta$M == "otro" & encuesta$O == "autónomo" & encuesta$C == "pequeña")) / length(encuesta$F)) / (length(which(encuesta$O == "autónomo" & encuesta$C == "pequeña")) / length(encuesta$F)),
  (length(which(encuesta$M == "otro" & encuesta$O == "autónomo" & encuesta$C == "grande")) / length(encuesta$F)) / (length(which(encuesta$O == "autónomo" & encuesta$C == "grande")) / length(encuesta$F))

  ),
  ncol = 3, nrow = 4,
  dimnames = list(c("empleado & pequeña", "empleado & grande", "autónomo & pequeña", "autónomo & grande"), c("coche", "tren", "otro"))
)

#Apartado E
#P(E = joven, S = M, F = universidad, O = autónomo, C = grande, M = coche).
resultado <- as.numeric(E[1] * S[1] * F[1, 2] * O[2, 2] * C[2, 2] * M[4, 1])

#P(E = anciano, S = F, F = secundaria, O = empleado, C = grande, M = otro).
resultado2 <- as.numeric(E[3] * S[2] * F[6, 1] * O[1, 1] * C[1, 2] * M[2, 3])


#P(E = adulto, S = M, F = universidad, O = autónomo, C = pequeña, M = tren)
resultado3 <- as.numeric(E[2] * S[1] * F[3, 2] * O[2, 2] * C[2, 1] * M[3, 2])


#Apartado F

#F1
#(E ⊥ S)
#(S ⊥ E)
#(C ⊥ E, S, O | F)
#(O ⊥ E, S, C | F)
#(M ⊥ E, S, F | O, C)


#F2
#P(E,S,F,O,C,M) = P(E) * P(S) * P(F∣E,S) * P(O∣F) * P(C∣F) * P(M∣O,C)

