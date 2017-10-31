load("~/GitHub/Simulacion/Simulacion/P12/Codigo/Tarea_P12.RData")

colnames(resultados)<-c("Replica","Tipo","Pruebas","Tiempo","Acierto")


for (t.pruebas in seq(300,900,300)){
    
    prueba.par<-resultados[resultados$Pruebas==t.pruebas&resultados$Tipo=="Paralelo",]
    prueba.org<-resultados[resultados$Pruebas==t.pruebas&resultados$Tipo=="Original",]
  
    dat.par<-prueba.par$Tiempo
    dat.org<-prueba.org$Tiempo
    
    test<-t.test(dat.org,dat.par)
    print(t.pruebas)
    print(test)
  }