library(reshape2)
library(ggplot2)

f <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}
repeticiones<-15

#Gráfico base
x <- seq(-6, 5, 0.25)
y <-  x
z <- outer(x, y, f)
colnames(z) <- y
rownames(z) <- x
d <- melt(z)
names(d) <- c("x", "y", "Valor")

base<-ggplot(d, aes(x, y)) + 
  geom_raster(aes(fill=Valor)) + 
  scale_fill_gradient(low="yellow", high="lightgoldenrodyellow")


tmax<-40
low <- -2
high <- 3
step <- 0.25

replicas <-function(r){
  
  curr.x <-runif(1, low, high)
  curr.y<-runif(1, low, high)
  best <- c(curr.x,curr.y)
  datos<-data.frame()
  resultados<-data.frame()

for (paso in 1:tmax) {

  delta.x <- runif(1,0,step)
  delta.y <- runif(1,0,step)
  left <- curr.x - delta.x
  right <- curr.x + delta.x
  
  up<-curr.y + delta.y
  down<-curr.y-delta.y
  
  if (f(left,curr.y) > f(right,curr.y)) {
    best.x<- c(left,curr.y)
  } else {
    best.x<- c(right,curr.y)
  }
  
  if (f(curr.x,down) > f(curr.x,up)) {
    best.y<-c(curr.x,down)
  } else {
    best.y<-c(curr.x,up)
  }
  
  
  if (f(best.x[1],best.x[2]) > f(best.y[1],best.y[2])) {
    curr.x<- best.x[1]
    curr.y<- best.x[2]
  } else {
    curr.x<- best.y[1]
    curr.y<- best.y[2]
  }
  
  
  if (f(curr.x,curr.y) > f(best[1],best[2])) {
    best[1]<-curr.x
    best[2]<-curr.y
  }
  datos<-cbind(curr.x,curr.y,f(curr.x,curr.y),r,paso)
  resultados<-rbind(resultados,datos)
    
}
  return(resultados)
}


suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
trayecto<- foreach(r = 1:repeticiones, .combine=rbind) %dopar% replicas(r)
stopImplicitCluster()

colnames(trayecto)=c("x","y","z","Replica","Paso")
trayecto$Replica<- as.factor(trayecto$Replica)

for (i in 1:tmax){

g<-trayecto[trayecto$Paso==i,]

base+ geom_point(data=g,aes(g$x,g$y,color=g$Replica))+
  labs(color="Réplica")+
  ggtitle(paste("Paso",i))+
  theme(plot.title = element_text(hjust = 0.5))
ggsave(paste("P7_paso_",i,".png"))

}
