npos<- function(r) {
  pos <- rep(0, dimension)
  for (t in 1:duracion) {
    cambiar <- sample(1:dimension, 1)
    cambio<-1
    if (runif(1) < 0.5) {
      cambio <- -1
    }
    
    pos[cambiar] <- pos[cambiar] + cambio
  }
  
  if(all(pos==origen)){
    return(TRUE)}else{return(FALSE)}
}
repetir<-200
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

datos <-  data.frame()
for(replica in 1:15){
  for (duracion in seq(100,400,100)){
      for (dimension in 1:8){
        origen<-rep(0,dimension)
        ti<-Sys.time()
        regreso <- foreach(r=1:repetir,.combine=c)%dopar% npos(r)
        tf<-Sys.time()
        t<-difftime(tf,ti,units="secs")
        
        res<-cbind(replica,duracion,dimension,t)
        datos <- rbind(datos, res)
      }
  }
  }

stopImplicitCluster()
save.image(file="Resultados_Reto_1.RData")

colnames(datos)<-c("Replica","Pasos","Dimension","Tiempo")
datos$Dimension<-as.factor(datos$Dimension)
datos$Pasos<-as.factor(datos$Pasos)

library(ggplot2)
ggplot(data=datos, aes(x = Pasos, y= Tiempo,fill=Dimension)) +
  geom_boxplot(position = position_dodge(1))+
  theme_bw()
ggsave("Variacion_tiempo.png")

