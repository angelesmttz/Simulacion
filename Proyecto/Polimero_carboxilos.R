##### Condiciones ###
concentracion.cmc<-15 #mg/mL
volumen.cmc<-20 #mL
contenido.plata<-1 #mg
mw<-250*(10**3) #k(g/mol)
DS=1.2
temperatura<-90 #Centigrados
tiempo<-24 #horas
reduccion<-0.4*(10**-19)

## Calculos ###
avogrado<-6.023*(10**23) #avogrado
cmc<-volumen.cmc*concentracion.cmc  #g
moleculas.cmc<- round(avogrado*cmc*reduccion/mw)
plata<-contenido.plata*(10**-3) #gramos
masa.plata<-108 #g/mol
atomos.plata<- round(avogrado*plata*reduccion/masa.plata)
posiciones<-c(2,3,5)

polimerizacion<-function (){
  base<-rep(NA,7)
  
  for (j in 1:length(posiciones)){
    cation<-posiciones[j]
    if (runif(1,0,1)<(DS/3)){
      base[cation]<-0
    }#else{base[cation]<-"H"}
  }
  return(base)
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

polimero<-foreach(1:moleculas.cmc,.combine=c)%dopar%polimerizacion()

stopImplicitCluster()

l<-2

cadenas<-as.data.frame(polimero)
cadenas$x<-0
cadenas$y<-0

cadenas[1,]$x<-runif(1,0,l)
cadenas[1,]$y<-runif(1,0,l)

dimension<-2

colocar<-dim(cadenas)[1]

for (g in 2:colocar){
  
    for (d in 1:dimension){
    
    paso<-l/100
      
    if(runif(1)< 0.5){paso<--paso}
    
    cadenas[g,d+1]<-cadenas[g-1,d+1]+paso
      
    }
  

    print(round(g*100/colocar))
}


# Considerar que el polimero no es lineal si no ciclico
# Considerar la repulsión electroestática de los grupos funcionales 

cadenas$x<-cadenas$x-min(cadenas$x)
cadenas$y<-cadenas$y-min(cadenas$y)

library(ggplot2)
polymer<-ggplot()+
  geom_point(data=cadenas,aes(x=cadenas$x,y=cadenas$y,color=factor(cadenas$polimero)))+
  xlab("x")+ylab("y")+
  theme_bw()


atomos.plata<-atomos.plata+40

Ag<- data.frame(x = runif(atomos.plata,min(cadenas$x),max(cadenas$x)),
                y=runif(atomos.plata,min(cadenas$y),max(cadenas$y)))  

polymer+
  geom_point(data=Ag,aes(x=Ag$x,y=Ag$y),color="darkgoldenrod1")
ggsave("Inicial.png")

#save.image(file="P_C_Listo.RData")


##############Velocidad####

R<- 8.3144598 #(Kg)(m^2)/(s^2)(k)(mol)
T.k<-temperatura+273
mol.plata<- masa.plata*(10^-3)
k<- 1.38 *(10**-23) #J/K
Ag.v<- sqrt(3*R*T.k/mol.plata) #m/s
KE<- (3/2)*k*T.k
forma<-function(T.k){
  Energy<-(3/2)*k*T.k
  
  return(2*Energy/mol.plata)
}
speed<-foreach(T.k=0:1000,.combine = c)%dopar% forma(T.k)
curva<-data.frame(T.e=c(0:1000),Velocidad=speed)

################Union####

radio<-0.005
p<-0.5
Ag$estado<-"F"

condicional<-matrix(FALSE,ncol=atomos.plata,nrow=colocar)

  for (r in 1:colocar){
    
    if (is.na(cadenas[r,]$polimero)){
      
    cadenas[r,]$polimero<-cadenas[r,]$polimero
      
    }else{
  
    for (u in 1:atomos.plata){
      if (Ag$estado=="F"){
      
      dx<-Ag[u,]$x-cadenas[r,]$x
      dy<-Ag[u,]$x-cadenas[r,]$x
      d<- sqrt(dx^2 + dy^2)
      
      if (d<radio){
        if (runif(1)<p){
        condicional[r,u]<-TRUE
        cadenas[r,]$polimero<-cadenas[r,]$polimero+1
        Ag[u,]$estado<-"O"
        Ag[u,]$x<-cadenas[r,]$x
        Ag[u,]$y<-cadenas[r,]$y
        }
        
      }
      
      }
    }
    }
  }

polymer+
  geom_point(data=Ag,aes(x=Ag$x,y=Ag$y),color="darkgoldenrod1")
ggsave("Paso.png")