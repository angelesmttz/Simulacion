ciclos<-7
tmax<-1

library(testit)

knapsack <- function(cap, peso, valor) {
  n <- length(peso)
  pt <- sum(peso) 
  assert(n == length(valor))
  vt <- sum(valor) 
  if (pt < cap) { 
    return(vt)
  } else {
    filas <- cap + 1 
    cols <- n + 1 
    tabla <- matrix(rep(-Inf, filas * cols),
                    nrow = filas, ncol = cols) 
    for (fila in 1:filas) {
      tabla[fila, 1] <- 0 
    }
    rownames(tabla) <- 0:cap 
    colnames(tabla) <- c(0, valor) 
    for (objeto in 1:n) { 
      for (acum in 1:(cap+1)) { # consideramos cada fila de la tabla
        anterior <- acum - peso[objeto]
        if (anterior > 0) { # si conocemos una combinacion con ese peso
          tabla[acum, objeto + 1] <- max(tabla[acum, objeto], tabla[anterior, objeto] + valor[objeto])
        }
      }
    }
    return(max(tabla))
  }
}

factible <- function(seleccion, pesos, capacidad) {
  return(sum(seleccion * pesos) <= capacidad)
}

objetivo <- function(seleccion, valores) {
  return(sum(seleccion * valores))
}

normalizar <- function(data) {
  menor <- min(data)
  mayor <- max(data)
  rango <- mayor - menor
  data <- data - menor # > 0
  return(data / rango) # entre 0 y 1
}

generador.pesos <- function(cuantos, min, max) {
  return(sort(round(normalizar(rnorm(cuantos)) * (max - min) + min)))
}

generador.valores <- function(pesos, min, max) {
  n <- length(pesos)
  valores <- double()
  for (i in 1:n) {
    media <- pesos[n]
    desv <- runif(1)
    valores <- c(valores, rnorm(1, media, desv))
  }
  valores <- normalizar(valores) * (max - min) + min
  return(valores)
}

poblacion.inicial <- function(n, tam) {
  pobl <- matrix(rep(FALSE, tam * n), nrow = tam, ncol = n)
  for (i in 1:tam) {
    pobl[i,] <- round(runif(n))
  }
  return(as.data.frame(pobl))
}

mutacion <- function(sol, n) {
  pos <- sample(1:n, 1)
  mut <- sol
  mut[pos] <- (!sol[pos]) * 1
  return(mut)
}

reproduccion <- function(x, y, n) {
  pos <- sample(2:(n-1), 1)
  xy <- c(x[1:pos], y[(pos+1):n])
  yx <- c(y[1:pos], x[(pos+1):n])
  return(c(xy, yx))
}

para.mut<-function(i){
  if (runif(1) < pm) {
    return(unlist(mutacion(p[i,],n)))
  }
}

para.rep<-function(i){
  padres <- sample(1:tam, 2,replace=TRUE)
  hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
  p.hijo<- hijos[1:n] # primer hijo
  s.hijo<- hijos[(n+1):(2*n)] # segundo hijo
  son<-rbind(p.hijo,s.hijo)
  return(son)
}

para.obj<-function(i){
  obj <- objetivo(p[i,], valores)
  fact <-factible(p[i,], pesos, capacidad)
  datos<-cbind(obj,fact)
  return(datos)
}

n <- 50
pesos <- generador.pesos(n, 15, 80)
valores <- generador.valores(pesos, 10, 500)
capacidad <- round(sum(pesos) * 0.65)
optimo <- knapsack(capacidad, pesos, valores)
pm <- 0.05
rep <- 50
resultados<-data.frame()
for (replicas in 1:ciclos){
  for (init in seq(200,500,100)){
    
    source('~/GitHub/Simulacion/Simulacion/P10/Cod_P10_T10.R')
    t.paralel<-cbind("paralelo",replicas, init,t)
    
    source('~/GitHub/Simulacion/Simulacion/P10/Cod_P10.R')
    t.original<-cbind("original",replicas,init,t)
    
    resultados<-rbind(resultados,t.paralel,t.original)
    
    
  }
  print(replicas)
}

save.image(file="Resultados_Tarea.RData")

library(ggplot2)

colnames(resultados)<-c("Tipo","Replica","Init","Tiempo")
resultados$Tiempo<-as.numeric(levels(resultados$Tiempo))[resultados$Tiempo]
resultados$Tipo <- as.factor(resultados$Tipo)
resultados$Init <- as.factor(resultados$Init)

png("Variacion_pob.png")
ggplot(data=resultados, aes(x = Init, y= Tiempo,fill = Tipo)) +
  geom_boxplot(position=position_dodge(1))+
  ylab("Tiempo (s)") +
  xlab("Población")
dev.off()

