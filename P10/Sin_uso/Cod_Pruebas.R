load("~/GitHub/Simulacion/Simulacion/P10/Optimos.RData")
colnames(resultados)<-c("Tipo","Replica","Valores","Paso")
resultados$Valores<-as.numeric(levels(resultados$Valores))[resultados$Valores]


for (replicas in 1:ciclos){
prueba.par<-resultados[resultados$Replica==replicas& resultados$Tipo=="paralelo",]
prueba.org<-resultados[resultados$Replica==replicas & resultados$Tipo=="original",]
  
dat.par<-prueba.par$Valores
dat.org<-prueba.org$Valores

test<-t.test(dat.org,dat.par)
print(test)
}