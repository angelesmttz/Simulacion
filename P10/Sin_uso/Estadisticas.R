load("C:/Users/angel/Documents/GitHub/Simulacion/Simulacion/P10/Resultados.RData")

test<-c()
for (init in seq(200,800,200)){
  prueba.par<-resultados[resultados$Init==init& resultados$Tipo=="paralelo",]
  prueba.org<-resultados[resultados$Init==init & resultados$Tipo=="original",]
  
  dat.par<-prueba.par$Tiempo
  dat.org<-prueba.org$Tiempo
  
  test<-c(test,t.test(dat.org,dat.par))
  print(test)
  
}

v1 <- sapply(test, function(x) x$p.value)