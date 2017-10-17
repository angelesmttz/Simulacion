ciclos<-3
tmax<-50
init<-200

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
  source('~/GitHub/Simulacion/Simulacion/P10/Cod_P10_T10.R')
  paralel<-cbind("paralelo",replicas,mejores,c(1:tmax))
  
  source('~/GitHub/Simulacion/Simulacion/P10/Cod_P10.R')
  original<-cbind("original",replicas,mejores,c(1:tmax))
  
  resultados<-rbind(resultados,paralel,original)
}

save.image(file="Optimos.RData")
load("~/GitHub/Simulacion/Simulacion/P10/Optimos.RData")

colnames(resultados)<-c("Tipo","Replica","Valores","Paso")
resultados$Replica<- as.factor(resultados$Replica)
resultados$Tipo<- as.factor(resultados$Tipo)
resultados$Valores <- as.numeric(levels(resultados$Valores))[resultados$Valores]
resultados$Paso <- as.numeric(levels(resultados$Paso))[resultados$Paso]
resultados<-resultados[resultados$Replica == "3",]


library(ggplot2)
png("Grafico_optimo.png")
ggplot()+
  geom_line(data=resultados,aes(x=resultados$Paso,y=resultados$Valores,color=Tipo))+
  geom_hline(yintercept=optimo, linetype="dashed", color = "black",size=0.8)+
  xlab("Paso")+  ylab("Valores")+
  theme(axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        axis.title.x = element_text(size=1),
        axis.title.y = element_text(size=14),
        legend.text=element_text(size=14))
dev.off()


  