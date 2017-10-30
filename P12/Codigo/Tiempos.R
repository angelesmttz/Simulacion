resultados<-data.frame()
pruebas<-data.frame()

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))


for (replica in 1:10){
  for (t.pruebas in seq(300,900,300)){
    
  source('~/GitHub/Simulacion/Simulacion/P12/Codigo/P12.R')
  secuencial<-cbind(replica,"Original",t.pruebas,tiempo)
  
  source('~/GitHub/Simulacion/Simulacion/P12/Codigo/P12_T12.R')
  paralelo<-cbind(replica,"Paralelo",t.pruebas,tiempo)

  resultados<-rbind(resultados,secuencial,paralelo)
  pruebas<-rbind(pruebas,pruebas.sec,pruebas.par)
  }
  print(replica)
}

stopImplicitCluster()

save.image(file="Tarea_P12.RData")
colnames(resultados)<-c("Replica","Tipo","Pruebas","Tiempo")
resultados$Tiempo<-as.numeric(levels(resultados$Tiempo))[resultados$Tiempo]
resultados$Tipo<-as.factor(resultados$Tipo)
resultados$Pruebas<-as.factor(resultados$Pruebas)

library(ggplot2)
ggplot()+
  geom_boxplot(data=resultados,aes(x=Pruebas,y=Tiempo,fill=Tipo))