inicio <- -6
final <- -inicio
paso <- 0.25
x <- seq(inicio, final, paso)
f <- function(x) { return(1 / (exp(x) + exp(-x))) }

suppressMessages(library(distr))
g <- function(x) { return((2 / pi) * f(x)) }
generador  <- r(AbscontDistribution(d = g)) # creamos un generador
e<- data.frame()
res<-data.frame()

for (qty in seq (10,1000,100)){
  muestra <- generador(qty) # sacamos una muestra
  desde <- 3
  hasta <- 7
  pedazo <- 5000
  cuantos <- 500
  parte <- function() {
    valores <- generador(pedazo)
    return(sum(valores >= desde & valores <= hasta))
  }
  suppressMessages(library(doParallel))
  registerDoParallel(makeCluster(detectCores() - 1))
  montecarlo <- foreach(i = 1:cuantos, .combine=c) %dopar% parte()
  stopImplicitCluster()
  integral <- sum(montecarlo) / (cuantos * pedazo)
  int<-(pi / 2) * integral
  e<-cbind(int,qty)
  res<-rbind(res,e)
  
  colnames(res)<-c("Area","Muestra")
  

}