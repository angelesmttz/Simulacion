repetir <- 300
duracion <- 200
eucl <-  FALSE
ciclos<-20
resultados <-  data.frame()

for (replicas in 1:ciclos){
for (dimension in 1:8) {
  origen<-rep(0,dimension)
  regreso<-rep(FALSE,repetir)
  
  for (r in 1:repetir){
      pos <- rep(0, dimension)
      for (t in 1:duracion) {
      cambiar <- sample(1:dimension, 1)
      cambio <- 1
      if (runif(1) < 0.5) {
      cambio <- -1}
      pos[cambiar] <- pos[cambiar] + cambio}
      
      if(all(pos==origen)){
        regreso[r]<-TRUE} 
            }
    
  datos <- cbind(replicas, dimension, sum(regreso))
    resultados<-rbind(resultados,datos)
}
}

colnames(resultados)<-c("Replica","Dimension","Regreso")
resultados$Dimension<-as.factor(resultados$Dimension)

library(ggplot2)

ggplot(data=resultados,aes(x=resultados$Dimension,y=resultados$Regreso))+
  geom_boxplot(fill="blue")+
  xlab("Dimensión")+ylab("Regreso al origen")
