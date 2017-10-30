binario <- function(d, l) {
  b <-  rep(FALSE, l)
  while (l > 0 | d > 0) {
    b[l] <- (d %% 2 == 1)
    l <- l - 1
    d <- bitwShiftR(d, 1)
  }
  return(b)
}

decimal <- function(bits, l) {
  valor <- 0
  for (pos in 1:l) {
    valor <- valor + 2^(l - pos) * bits[pos]
  }
  return(valor)
}

prueba<-function(i){
  w <- neuronas[i,]
  deseada <- correcto[i]
  resultado <- sum(w * pixeles) >= 0
  return(resultado)
}

setwd("~/GitHub/Simulacion/Simulacion/P12/Codigo")
modelos <- read.csv("digitos.modelo", sep=" ", header=FALSE, stringsAsFactors=F)
r <- 5
c <- 3
dim <- r * c
tasa <- 0.15
tranqui <- 0.993
tope <- 9
digitos <- 0:tope
pasos.pb<-300
k <- length(digitos)
n <- floor(log(k-1, 2)) + 1
neuronas <- matrix(runif(n * dim), nrow=n, ncol=dim)
test<-pasos.pb/length(digitos)
combinaciones<-rep(digitos,test)
combinaciones<-sample(combinaciones)

var.n<-c(0.995,0.5)
var.g<-c(0.92,0.8)
var.b<-c(0.002,0.2,0.4)
resultados<-data.frame()

library(reshape2)
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

for (negro in var.n){
  for (gris in var.g){
    for (blanco in var.b){

modelos[modelos=='n'] <- negro
modelos[modelos=='g'] <- gris
modelos[modelos=='b'] <- blanco

contadores <- matrix(rep(0, k*(k+1)), nrow=k, ncol=(k+1))
rownames(contadores) <- 0:tope
colnames(contadores) <- c(0:tope, NA)

for (t in 1:5000) { # entrenamiento
  d <- sample(0:tope, 1)
  pixeles <- runif(dim) < modelos[d + 1,]
  correcto <- binario(d, n)
  neuronas<-foreach(i=1:n,.combine=rbind)%dopar% perceptron(i)
  tasa <- tranqui * tasa
}

for (t in 1:pasos.pb) { # prueba
  d <- combinaciones[t]
  pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
  correcto <- binario(d, n)
  salida<-foreach(i=1:n,.combine=c)%dopar%prueba(i)
  r <- min(decimal(salida, n), k) # todos los no-existentes van al final
  contadores[d+1, r+1] <- contadores[d+1, r+1] + 1
}

res<-melt(contadores)
res<-cbind(res,negro,gris,blanco)
resultados<-rbind(resultados,res)

    }
  }
}

stopImplicitCluster()
colnames(resultados)<-c("Correcto","Resultado","Valor","Negro","Gris","Blanco")
resultados<-resultados[resultados$Valor!=0,]
save.image(file="Reto1.RData")

load("~/GitHub/Simulacion/Simulacion/P12/Codigo/Reto1.RData")
resultados[is.na(resultados)]<-tope+1
tam<-dim(resultados)[1]
resultados$Juicio<- rep("Incorrecto",tam)


for (f in 1:tam){
  fila<-resultados[f,]
    if (fila$Correcto==fila$Resultado){resultados[f,]$Juicio="Correcto"}
}

resultados$Correcto<-as.factor(resultados$Correcto)
resultados$Juicio<-as.factor(resultados$Juicio)

library(ggplot2)
ggplot()+
geom_boxplot(data=resultados,aes(x=Correcto,y=Valor*100/test,fill=Juicio))
  