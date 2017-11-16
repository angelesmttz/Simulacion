##### Condiciones ###
concentracion.cmc<-15 #mg/mL
volumen.cmc<-20 #mL
contenido.plata<-1 #mg
mw<-250*(10**3) #k(g/mol)
DS=1.2
temperatura<-90 #Centigrados
tiempo<-24 #horas
reduccion<-10**-19

## Calculos ###
avogrado<-6.023*(10**23) #avogrado
cmc<-volumen.cmc*concentracion.cmc  #g
moleculas.cmc<- round(avogrado*cmc*reduccion/mw)
plata<-contenido.plata*(10**-3) #gramos
masa.plata<-108 #g/mol
atomos.plata<- round(avogrado*plata*reduccion/masa.plata)
posiciones<-c(2,3,6)

polimerizacion<-function (){
  base<-rep(NA,7)
  
  for (j in 1:length(posiciones)){
    cation<-posiciones[j]
    if (runif(1,0,1)<(DS/3)){
      base[cation]<-0
    }
  }
  return(base)
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

polimero<-foreach(1:moleculas.cmc,.combine=c)%dopar%polimerizacion()

stopImplicitCluster()


polimero<-as.data.frame(polimero)
polimero$x<-0
polimero$y<-0
polimero$dx<-0
polimero$dy<-0

for (grupo in 1:dim(polimero)[1]){
  j<-polimero[grupo,]
  
}



