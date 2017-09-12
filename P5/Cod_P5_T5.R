library(ggplot2)
inicio <- -6
final <- -inicio
paso <- 0.25
ciclos<-50
varcuan<-seq(10,100,10)
x <- seq(inicio, final, paso)
f <- function(x) { return(1 / (exp(x) + exp(-x))) }

suppressMessages(library(distr))
g <- function(x) { return((2 / pi) * f(x)) }
generador  <- r(AbscontDistribution(d = g)) # creamos un generador

wa<-0.0488341111
desde <- 3
hasta <- 7
pedazo <- 50000
res<-data.frame()

parte <- function() {
  valores <- generador(pedazo)
  return(sum(valores >= desde & valores <= hasta))
}
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

for (cuantos in varcuan){
  n<-cuantos*pedazo
  
  for (replica in 1:ciclos){
    
t<-system.time({

montecarlo <- foreach(i = 1:cuantos, .combine=c) %dopar% parte()
integral <- sum(montecarlo) / n
int<-(pi / 2) * integral
})
error<-abs(int-wa)
e<-cbind(int,error, cuantos,t[3])
res<-rbind(res,e)

  }
}

stopImplicitCluster()


colnames(res)<-c("valor","error","cuantos","tiempo")

png("Variacion_error.png")
res$cuantos<-as.factor(res$cuantos)
ggplot(data=res,aes(x=res$cuantos,y=res$error))+
  geom_boxplot(fill="cadetblue2")+
  scale_y_continuous(name="Error",labels=scales::comma) +
  scale_x_discrete(name="Muestras")

png("Variacion_tiempo.png")
ggplot(data=res,aes(x=res$cuantos,y=res$tiempo))+
  geom_boxplot(fill="cadetblue2")+
  scale_y_continuous(name="Tiempo",labels=scales::comma) +
  scale_x_discrete(name="Muestras")
graphics.off()

rm(list=ls())
gc()
