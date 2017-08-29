library(ggplot2)
source('~/GitHub/Simulacion/Simulacion/P3/Obt_primos.R')

primo <- function(n) {
  if (n == 1 || n == 2) {
    return(TRUE)
  }
  if (n %% 2 == 0) {
    return(FALSE)
  }
  for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
    if ((n %% i) == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

desde <- 100
hasta <-  300
original <- desde:hasta
invertido <- hasta:desde
prgr<-tail(primos,hasta-desde)
prpq<-primos[hasta-desde]

replicas <- 30
suppressMessages(library(doParallel))
resultados<-data.frame()

for (t in 1:(detectCores()-1)){
  registerDoParallel(makeCluster(t))
  ot <-  numeric()
  it <-  numeric()
  at <-  numeric()
  gt<-numeric()
  pt<-numeric()
  
  for (r in 1:replicas) {
    ot <- c(ot, system.time(foreach(n = original, .combine=c) %dopar% primo(n))[3]) 
    it <- c(it, system.time(foreach(n = invertido, .combine=c) %dopar% primo(n))[3])
    at <- c(at, system.time(foreach(n = sample(original), .combine=c) %dopar% primo(n))[3])
    gt <- c(gt, system.time(foreach(n = prgr, .combine=c) %dopar% primo(n))[3])
    pt <- c(pt, system.time(foreach(n = prpq, .combine=c) %dopar% primo(n))[3])
    
  
}
  
  stopImplicitCluster()
  ot <-  matrix(ot,byrow=TRUE)
  it <-  matrix(it,byrow=TRUE)
  at <-  matrix(at,byrow=TRUE)
  gt<-matrix(gt,byrow=TRUE)
  pt<-matrix(pt,byrow=TRUE)
  

  ot<-cbind(ot,1,t)
  it<-cbind(it,2,t)
  at<-cbind(at,3,t)
  gt<-cbind(gt,4,t)
  pt<-cbind(pt,5,t)
  
  resultados<-rbind(resultados,ot,it,at,gt,pt)
  resultados<-data.frame(resultados)
  
  
}
colnames(resultados)<-c("tiempo","Orden","Nucleos")
resultados$Nucleos <- factor(resultados$Nucleos,
                labels = c("Uno", "Dos", "Tres"))
resultados$Orden <- factor(resultados$Orden,
                             labels = c("Original", "Invertido", "Aleatorio","Primos grandes","Primos pequeños"))

png("Variacion_en_nucleos.png")
ggplot() +
  geom_boxplot(data=resultados, aes(x = Nucleos, y=tiempo,fill =Orden))+
  scale_y_continuous(name="Tiempo de ejecucion") +
  scale_x_discrete(name="Nucleos")
dev.off()