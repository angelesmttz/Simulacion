ciclos<-10
resultados<-data.frame()

for (n in seq (50,95,15)){
for (replicas in 1:ciclos){
  
  source('~/GitHub/Simulacion/Simulacion/P6/Cod_P6_parSapply.R')
  
  parS<-cbind("paralelo",t,n)
  
  source('~/GitHub/Simulacion/Simulacion/P6/Cod_P6.R')
  
  original<-cbind("original",t,n)
  
  print(original)
  
  resultados<-rbind(resultados,parS,original)
}
}

library(ggplot2)
colnames(resultados)<-c("Tipo","Tiempo","Agentes")
resultados$Tipo <- as.factor(resultados$Tipo)
resultados$Agentes <- as.factor(resultados$Agentes)
resultados$Tiempo<-as.numeric(levels(resultados$Tiempo))[resultados$Tiempo]

png("Variacion_paralelo.png")
ggplot(data=resultados, aes(x = Agentes, y= Tiempo,fill = Tipo)) +
  geom_boxplot(position=position_dodge(1))
dev.off()
  
 



