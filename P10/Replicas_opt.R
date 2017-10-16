ciclos<-1
init<-200
tmax<-3
resultados<-data.frame()

for (replicas in 1:ciclos){
  source('~/GitHub/Simulacion/Simulacion/P10/Cod_P10_T10.R')
  paralel<-cbind("paralelo",replicas,mejor)
  
  source('~/GitHub/Simulacion/Simulacion/P10/Cod_P10_T10_SPM.R')
  mparalel<-cbind("mparalelo",replicas,init,mejores,c(1:tmax))
  
  source('~/GitHub/Simulacion/Simulacion/P10/Cod_P10.R')
  original<-cbind("original",replicas,init,mejor)
  
  resultados<-rbind(resultados,paralel,mparalel,original)
  }
