ciclos<-5
resultados<-data.frame()

for (replicas in 1:ciclos){
  for (k in seq(50000,200000,50000)){
  
  source('~/GitHub/Simulacion/Simulacion/P8/Cod_P8_T8.R')
  t.paralel<-cbind("paralelo",replica, k,tiempo)
  
  source('~/GitHub/Simulacion/Simulacion/P8/Original/Cod_P8.R')
  t.original<-cbind("original",replica, k,tiempo)
  
  resultados<-rbind(resultados,t.paralel,t.original)
  }
}

load("C:/Users/angel/Documents/Mis_resultados.Rdata")
library(ggplot2)

colnames(resultados)<-c("Tipo","Replica","k","Tiempo")
resultados$Tipo <- as.factor(resultados$Tipo)

resultados$Tiempo<-as.numeric(levels(resultados$Tiempo))[resultados$Tiempo]
resultados[resultados$Tiempo>30,4]<-resultados[resultados$Tiempo>30,4]/60

resultados$k<-as.numeric(levels(resultados$k))[resultados$k]
resultados$k<-resultados$k/10000
resultados$k <- as.factor(resultados$k)
 
png("Variacion_k.png")
ggplot(data=resultados, aes(x = k, y= Tiempo,fill = Tipo)) +
  geom_boxplot(position=position_dodge(1))+
  scale_y_continuous(name="Tiempo (min)") +
  scale_x_discrete(name="Tama\u{00f1}o de c\u{00fa}mulos(10^4)")
  
dev.off()
  
 



