##### Condiciones ###
concentracion.cmc<-15 #mg/mL
volumen.cmc<-20 #mL
contenido.plata<-1 #mg
mw<-250*(10**3) #k(g/mol)
DS=1.2
temperatura<-90 #Centigrados
tiempo<-24 #horas
reduccion<-0.4*(10**-17)

## Calculos ###
avogrado<-6.023*(10**23) #avogrado
cmc<-volumen.cmc*concentracion.cmc  #g
moleculas.cmc<- round(avogrado*cmc*reduccion/mw)
plata<-contenido.plata*(10**-3) #gramos
masa.plata<-108 #g/mol
atomos.plata<- round(avogrado*plata*reduccion/masa.plata)
posiciones<-c(2,3,5)

polimerizacion<-function (){
  base<-rep("P",7)
  
  for (j in 1:length(posiciones)){
    cation<-posiciones[j]
    if (runif(1,0,1)<(DS/3)){
      base[cation]<-"C"
    }
  }
  return(base)
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

polimero<-foreach(1:moleculas.cmc,.combine=c)%dopar%polimerizacion()

stopImplicitCluster()

l<-1

cadenas<-as.data.frame(polimero)
cadenas$x<-0
cadenas$y<-0

cadenas[1,]$x<-runif(1,-l,l)
cadenas[1,]$y<-runif(1,-l,l)

paso<-l/30
dimension<-c(1:2)
dim(cadenas)[1]

for (g in 2:dim(cadenas)[1]){
  
    for (d in 1:length(dimension)){
      
    if(runif(1)<0.5){
       cadenas[g,d+1]<-cadenas[g-1,d+1]-paso}else{
         cadenas[g,d+1]<-cadenas[g-1,d+1]+paso  
       }
      
      #if (cadenas[g,d+1]<-l){cadenas[g,d+1]<-cadenas[d,d+1]+l}
      #if (cadenas[g,d+1]>l){cadenas[g,d+1]<-cadenas[d,d+1]-l}
      
    }
    print(g)
}
  

library(ggplot2)
ggplot()+
  geom_point(data=cadenas,aes(x=cadenas$x,y=cadenas$y,color=cadenas$polimero))





