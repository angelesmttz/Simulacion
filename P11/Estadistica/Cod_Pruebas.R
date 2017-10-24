load("C:/Users/angel/Documents/GitHub/Simulacion/Simulacion/P11/Workspaces/Resultados_Tarea.RData")

colnames(resultados)<-c("Tipo","Replica","k","n","Tiempo")
resultados$Tiempo<-as.numeric(levels(resultados$Tiempo))[resultados$Tiempo]

vark<-seq(3,15,3)
varn<-seq(200,1000,200)

for (k in vark){
  for (n in varn){
  
prueba.par<-resultados[resultados$k==k&resultados$Tipo=="paralelo"&resultados$n==n,]
prueba.org<-resultados[resultados$k==k&resultados$Tipo=="original"&resultados$n==n,]
  
dat.par<-prueba.par$Tiempo
dat.org<-prueba.org$Tiempo

test<-t.test(dat.org,dat.par)
print(k)
print(n)
print(test)
  }
}