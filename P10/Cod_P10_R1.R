init<-50
tmax<-50

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

para.rep.m<-function(i){
  padres <- sample(1:tam, 2, prob=elites,replace=TRUE)
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


suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

for (replicas in 1:5){
p <- poblacion.inicial(n, init)
tam <- dim(p)[1]
assert(tam == init)
mejores <- double()


for (iter in 1:tmax) {
  p$obj <- NULL
  p$fact <- NULL
  
  p<-rbind(p,foreach(i=1:tam,.combine = rbind)%dopar% para.mut(i))
  elites<-foreach(i=1:tam,.combine = c)%dopar% objetivo(p[i,], valores)
  elites <- elites / sum(elites)
  p<-rbind(p,foreach(i=1:rep,combine=rbind)%dopar% para.rep.m(i))
  
  tam <- dim(p)[1]
  obj <- double()
  fact <- integer()
  
  p<- data.frame(sapply(p, function(x) as.numeric(as.character(x))))
  p<-cbind(p,foreach(i=1:tam,.combine=rbind)%dopar% para.obj(i))
  
  mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
  
  p <- p[mantener,]
  tam <- dim(p)[1]
  assert(tam == init)
  factibles <- p[p$fact == TRUE,]
  mejor <- max(factibles$obj)
  mejores <- c(mejores, mejor)
  
}
  datos<-cbind(replicas,mejores,c(1:tmax))
  resultados<-rbind(resultados,datos)
  print(replicas)
}

stopImplicitCluster()

#save.image(file="Reto1.RData")

para.rep<-function(i){
  padres <- sample(1:tam, 2,replace=TRUE)
  hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
  p.hijo<- hijos[1:n] # primer hijo
  s.hijo<- hijos[(n+1):(2*n)] # segundo hijo
  son<-rbind(p.hijo,s.hijo)
  return(son)
}

source('~/GitHub/Simulacion/Simulacion/P10/Cod_P10_T10.R')
tarea<-cbind(mejores,c(1:tmax))
tarea<-as.data.frame(tarea)
colnames(tarea)<-c("Valores","Paso")
colnames(resultados)<-c("Replica","Valores","Paso")
resultados$Replica<-as.factor(resultados$Replica)


library(ggplot2)
png("Seleccion_ruleta_100.png")

ggplot()+
  geom_line(data=resultados,aes(x=resultados$Paso,y=resultados$Valores,color=resultados$Replica),size=0.6,linetype="F1")+
  geom_hline(yintercept=optimo, linetype="dashed", color = "darkgreen",size=0.6)+
  geom_line(data=tarea,aes(x=tarea$Paso,y=tarea$Valores,color="black"),size=0.6)+
  xlab("Paso")+   ylab("Valores")+
  scale_color_manual(name = "Selección", values = c("deepskyblue","deepskyblue1",
          "darkgoldenrod2","firebrick3","darkorchid","black"),
          labels =c("Ruleta1", "Ruleta2","Ruleta3","Ruleta4","Ruleta5","Original")) +
    guides(color = guide_legend(order = 1))
    dev.off()


save.image(file="R1_100.RData")
