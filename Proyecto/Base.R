setwd("~/GitHub/Simulacion/Simulacion/Proyecto")
### Condiciones   ###
concentracion.cmc<-15 *(10**-16) #mg/mL
volumen.cmc<-1 #mL
contenido.plata<-1 #mg
mw<-250*(10**3) #k(g/mol)
temperatura<-90 #Centigrados
tiempo<-24 #horas


## Calculos ###
avogrado<-6.023*(10**23) #avogrado
cmc<-volumen.cmc*concentracion.cmc  #g
moleculas.cmc<- round(avogrado*cmc/mw)
plata<-contenido.plata*(10**-3) #gramos
masa.plata<-108 #g/mol
atomos.plata<- round(avogrado*plata/masa.plata)
posiciones<-c(2,3,6)

polimerizacion<-function (){
  base<-rep(0,7)
  
  for (j in 1:length(posiciones)){
    cation<-posiciones[j]
    if (runif(1,0,1)<probabilidad){
      base[cation]<-1
    }
  }
  return(base)
}

resultados<-data.frame()
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

for (replicas in 1:10){
  for (DS in seq(0.5,3,0.5)){
    for (probabilidad in seq(0.05,1,0.05)){

grado<-DS
res2<-cbind(grado,"real",probabilidad)

polimero<-foreach(1:moleculas.cmc,.combine=c)%dopar%polimerizacion()


grado<-round(sum(polimero)/moleculas.cmc,digits=1)
res<-cbind(grado,"calculado",probabilidad)


resultados<-rbind(resultados,res,res2)

  }
  }
  print(replicas)
  }

stopImplicitCluster()

save.image("Prbabilidad3.RData")
colnames(resultados)<-c("DS","Tipo","Probabilidad")
resultados$DS<-as.numeric(levels(resultados$DS))[resultados$DS]
resultados$Probabilidad<-as.numeric(levels(resultados$Probabilidad))[resultados$Probabilidad]

library(ggplot2)

ggplot()+
  geom_boxplot(data=resultados, aes(x=Tipo, y=Probabilidad,fill=DS))
  

