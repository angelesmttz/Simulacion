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

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

datos <-  data.frame()
for (replica in 1:7){
for (duracion in seq(100,400,100)){
for(repetir in seq(100,400,100)){
for (dimension in 1:8) {
  origen<-rep(0,dimension)
  
  regreso <- foreach(r=1:repetir,.combine=c)%dopar% npos(r)
  res<-cbind(replica,duracion,repetir,dimension,sum(regreso))
  datos <- rbind(datos, res)
}
}
}
}
stopImplicitCluster()
save.image(file="Resultados_Tarea_1.RData")

colnames(datos)<-c("Replica", "Pasos","Repeticiones","Dimension","Regresos")
datos$Repeticiones<-as.factor(datos$Repeticiones)
datos$Dimension<-as.factor(datos$Dimension)
datos$Pasos<-as.factor(datos$Pasos)

library(ggplot2)
ggplot(data=datos, aes(x = Pasos, y= Regresos,color=Dimension)) +
  geom_boxplot(position = position_dodge(1))+facet_grid(Repeticiones~.)+
  theme_bw()+ ylab("Regresos al origen")
ggsave("Variacion_dimensiones.png")

