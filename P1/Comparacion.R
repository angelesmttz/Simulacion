

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

for (replicas in 1:ciclos){
  for (k in vark){
    for (n in varn){
      
      source('~/GitHub/Simulacion/Simulacion/P11/P11.R')
      paralelo<-cbind("original",replicas,k,n,t)
      
      source('~/GitHub/Simulacion/Simulacion/P11/P11_T11.R')
      secuencial<-cbind("paralelo",replicas, k,n, t)
      
      resultados<-rbind(resultados,paralelo,secuencial)
    }
  }
  print(replicas)
}

stopImplicitCluster()

save.image(file="Resultados_Tarea_11.RData")