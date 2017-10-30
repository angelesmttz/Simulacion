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
  geom_boxplot(data=resultados,aes(x=Pruebas,y=Tiempo,fill=Tipo))+
  ylab("Tiempo(s)")+xlab("Número de pruebas")
ggsave("Variacion_tiempo.png")


colnames(pruebas)<-c("Numero","Resultado","Correcto","Juicio","Tipo","Pruebas")
pruebas$Juicio<-as.factor(pruebas$Juicio)
pruebas$Tipo<-as.factor(pruebas$Tipo)
pruebas$Pruebas<-as.factor(pruebas$Pruebas)
pruebas$Correcto<-as.factor(pruebas$Correcto)
pruebas$Numero<-rep(1,dim(pruebas)[1])

ggplot(data=pruebas,aes(x=Correcto))+
  geom_histogram(fill=Juicio)+facet_grid(Pruebas~Tipo)