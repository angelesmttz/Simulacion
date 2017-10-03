load("C:/Users/angel/Documents/Mis_resultados.Rdata")
colnames(resultados)<-c("Tipo","Replica","k","Tiempo")
resultados$Tiempo<-as.numeric(levels(resultados$Tiempo))[resultados$Tiempo]
resultados[resultados$Tiempo>30,4]<-resultados[resultados$Tiempo>30,4]/60

for (k in seq(50000,200000,50000)){
prueba.par<-resultados[resultados$k==k& resultados$Tipo=="paralelo",]
prueba.org<-resultados[resultados$k==k & resultados$Tipo=="original",]
  
dat.par<-prueba.par$Tiempo
dat.org<-prueba.org$Tiempo

test<-t.test(dat.org,dat.par)
print(test)
}