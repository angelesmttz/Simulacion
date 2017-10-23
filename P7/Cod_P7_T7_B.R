f <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}

low <- -2
high <- 3
step <- 0.25
replicas <- 10
tmax<-50
maxWA<-0.0666822
resultados<-data.frame()

paralelo <-function(w){
  
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
    
    
    v1<-cbind(left,curr.y,f(left,curr.y))
    v2<-cbind(right,curr.y,f(right,curr.y))
    v3<-cbind(curr.x,down,f(curr.x,down))
    v4<-cbind(curr.x,up,f(curr.x,down))
    
    
 
    
    if (f(curr.x,curr.y) > f(best[1],best[2])) {
      best[1]<-curr.x
      best[2]<-curr.y
      
    }
    
    datos<-cbind(curr.x,curr.y,f(curr.x,curr.y),w,paso)
    resultados<-rbind(resultados,datos)  
  }
  return(resultados)
}


suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
trayecto<- foreach(w = 1:replicas, .combine=rbind) %dopar% paralelo(w)

stopImplicitCluster()


library(ggplot2)

colnames(trayecto)<-c("x","y","Valores","Replica","Paso")
trayecto$Replica<- as.factor(trayecto$Replica)

png("Replicas.png")
ggplot(data=trayecto, aes(x = Paso, y= trayecto$Valores,group=trayecto$Replica)) +
  geom_point(size=1.5,color=trayecto$Replica)+
  scale_y_continuous(name="Resultado de la función")+
  theme(legend.position = "none")+
  geom_hline(yintercept=maxWA, linetype="dashed", color = "black",size=0.5)+
  theme_minimal()

graphics.off()