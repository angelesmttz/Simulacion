ciclos<-10
resultados<-data.frame()

for (replicas in 1:ciclos){
  for (init in seq(200,800,200)){
    
    source('~/GitHub/Simulacion/Simulacion/P10/Cod_P10_T10.R')
    t.paralel<-cbind("paralelo",replicas, init,t)
    
    source('~/GitHub/Simulacion/Simulacion/P10/Cod_P10_T10_SPM.R')
    t.mparalel<-cbind("mparalelo",replicas,init,t)
    
    source('~/GitHub/Simulacion/Simulacion/P10/Cod_P10.R')
    t.original<-cbind("original",replicas,init,t)
    
    resultados<-rbind(resultados,t.paralel,t.mparalel,t.original)
  }
}



library(ggplot2)

colnames(resultados)<-c("Tipo","Replica","Init","Tiempo")
resultados$Tiempo<-as.numeric(levels(resultados$Tiempo))[resultados$Tiempo]
resultados$Tipo <- as.factor(resultados$Tipo)
resultados$Init <- as.factor(resultados$Init)

png("Variacion_pob.png")
ggplot(data=resultados, aes(x = Init, y= Tiempo,fill = Tipo)) +
  geom_boxplot(position=position_dodge(1))+
  ylab("Tiempo (s)") +
  xlab("Población")
dev.off()

save.image(file="Resultados.RData")