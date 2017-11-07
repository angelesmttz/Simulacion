mw<-50000
DS=1.2
carbono<-6
posiciones<-c(1,3,6)
polimero<-c()

polimerizacion<-function(){
  base<-rep(0,carbono+1)
  cation<-sample(posiciones,1)
  base[cation]<-1
  return(base)
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

polimero<-foreach(1:mw,.combine=c)%dopar% polimerizacion()

stopImplicitCluster()

imagen<-matrix(polimero,ncol=mw/2)

plata<-1*(10**-3) #gramos
masa.plata<-108 #gramos/mol
avogrado<-6.023*(10**23) #avogrado

atomos.plata<- avogrado*plata/masa.plata
