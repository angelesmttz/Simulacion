setwd("~/GitHub/Simulacion/Simulacion/P12/Codigo")
t.pruebas<-200
var.n<-seq(0.02,0.99,0.19)
var.g<-seq(0.02,0.99,0.19)
var.b<-seq(0.02,0.99,0.19)
resultados<-data.frame()

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

for (negro in var.n){
  for (gris in var.g){
    for (blanco in var.b){

      source('~/GitHub/Simulacion/Simulacion/P12/Codigo/P12_T12.R')
      
proba<-cbind(negro,gris,blanco,acierto)
print(proba)
resultados<-rbind(resultados,proba)
    }
  }
  }
stopImplicitCluster()

save.image(file="Reto1.RData")
load("~/GitHub/Simulacion/Simulacion/P12/Codigo/Reto1.RData")
colnames(resultados)<-c("Negro","Gris","Blanco","Acierto")

library(ggplot2)
ggplot(resultados, aes(Blanco, Gris)) + 
  geom_raster(aes(fill=Acierto)) + 
  scale_fill_gradient(low="yellow", high="red")
ggsave("Heat_GB.png")
  
ggplot(resultados, aes(Negro, Gris)) + 
  geom_raster(aes(fill=Acierto)) + 
  scale_fill_gradient(low="yellow", high="red")
ggsave("Heat_GN.png")

ggplot(resultados, aes(Negro, Blanco)) + 
  geom_raster(aes(fill=Acierto)) + 
  scale_fill_gradient(low="yellow", high="red")
ggsave("Heat_BN.png")