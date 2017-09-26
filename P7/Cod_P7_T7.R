library(ggplot2)
f <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}
low <- -2
high <- 3
step <- 0.25
replicas <- 10
tmax<-35
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
    
    datos<-cbind(curr.x,curr.y,f(curr.x,curr.y),w,paso)
    resultados<-rbind(resultados,datos)  
  }
  return(resultados)
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
trayecto<- foreach(w = 1:replicas, .combine=rbind) %dopar% paralelo(w)
stopImplicitCluster()

colnames(trayecto)<-c("x","y","Valores","Replica","Paso")
trayecto$Replica<- as.factor(trayecto$Replica)

png("Replicas.png")
ggplot() +
  geom_line(data=trayecto, aes(x = Paso, y= trayecto$Valores,color=trayecto$Replica),size=0.6)+
  scale_y_continuous(name="Resultado de la función")+
  geom_hline(yintercept=maxWA, linetype="dashed", color = "black",size=0.5)+
  theme_minimal()+
  guides(color=FALSE)
  
graphics.off()

