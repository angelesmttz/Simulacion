ciclos<-6
k<-125000

resultados<-data.frame()

for (replicas in 1:ciclos){
  
  source('~/GitHub/Simulacion/Simulacion/P8/Cod_P8_T8.R')
  
  t.paralel<-cbind("paralelo",replicas, k,tiempo)
  
  source('~/GitHub/Simulacion/Simulacion/P8/Original/Cod_P8.R')
  
  t.original<-cbind("original",replicas, k,tiempo)
  
  resultados<-rbind(resultados,t.paralel,t.original)
  
}

save.image(file="Resultados_Tarea.RData")

library(ggplot2)

colnames(resultados)<-c("Tipo","Replica","k","Tiempo")
resultados$Tipo <- as.factor(resultados$Tipo)
resultados$Tiempo<-as.numeric(levels(resultados$Tiempo))[resultados$Tiempo]


png("Variacion_paralelo.png")
ggplot(data=resultados, aes(x = Tipo, y= Tiempo)) +
  geom_boxplot(position=position_dodge(1),color = "black", fill = "steelblue")+
  scale_y_continuous(name="Tiempo (min)") +
  scale_x_discrete(name="Tipo")

dev.off()