#####Condiciones######
concentracion.cmc<-15 #mg/mL
volumen.cmc<-20 #mL
contenido.plata<-1 #mg
mw<-250*(10**3) #k(g/mol)
DS=1.2
temperatura<-90 #Centigrados
tiempo<-24 #horas
reduccion<-0.4*(10**-17)


#####Calculos######
avogrado<-6.023*(10**23) #avogrado
cmc<-volumen.cmc*concentracion.cmc*(10**-3)  #mg
moleculas.cmc<- round(avogrado*cmc*reduccion/mw)
plata<-contenido.plata*(10**-3) #gramos
masa.plata<-108 #g/mol
atomos.plata<- round(avogrado*plata*reduccion/masa.plata)
posiciones<-c(2,3,5)
setwd("~/GitHub/Simulacion/Simulacion/Proyecto")

#####Funciones######
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

manhattan <- function(p1, p2) {
  return(sum(abs(p1 - p2)))
}

#####Librerias#####
suppressMessages(library(doParallel))
library(ggplot2)
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
max.x<-max(cadenas$x)
max.y<-max(cadenas$y)



  ggplot(data=cadenas,aes(x=cadenas$x,y=cadenas$y))+
  geom_point(aes(color=factor(cadenas$polimero)),shape=20,size=0.5)+
  xlab("x")+ylab("y")+
  ggtitle("Pol\u{00ed}mero en soluci\u{00fa}n")+
  scale_color_discrete(name="\u{00C1}tomos de\nplata",
                       breaks=c(0, NA),
                       labels=c(0, "Pol\u{00ed}mero"))+
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white"))
  ggsave("Polimero.png")
  

Ag<- data.frame(x = runif(atomos.plata,min(cadenas$x),max(cadenas$x)),
                y=runif(atomos.plata,min(cadenas$y),max(cadenas$y)),
                dx=runif(atomos.plata,-max.x/50,max.x/50),
                dy=runif(atomos.plata,-max.y/50,max.y/50))  

ggplot(data=cadenas,aes(x=cadenas$x,y=cadenas$y))+
  geom_point(aes(color=factor(cadenas$polimero)),shape=20,size=0.5)+
  xlab("x")+ylab("y")+
  ggtitle("Estado inicial")+
  scale_color_discrete(name="\u{00C1}tomos de\nplata",
                       breaks=c(0, NA),
                       labels=c(0, "Pol\u{00ed}mero"))+
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white"))+
  geom_point(data=Ag,aes(x=Ag$x,y=Ag$y),color="darkgoldenrod1")
ggsave("Inicial.png")

#save.image(file="P_C_Listo.RData")


#####Velocidad######

R<- 8.3144598 #(Kg)(m^2)/(s^2)(k)(mol)
T.k<-temperatura+273
mol.plata<- masa.plata*(10^-3)
k<- 1.38 *(10**-23) #J/K
Ag.v<- round(sqrt(3*R*T.k/mol.plata))#m/s


######Union######

p<-function (d){
  return(1/d)
}

radio<-0.005
p<-0.5  ###Hacer funcion a la distancia
Ag$estado<-"F"

for (hora in seq(0,tiempo,1)){
  
  for (r in 1:colocar){
     if (is.na(cadenas[r,]$polimero)){
      cadenas[r,]$polimero<-NA}
        else{
          for (u in 1:atomos.plata){
            if (Ag$estado=="F"){
          dx<-Ag[u,]$x-cadenas[r,]$x
          dy<-Ag[u,]$y-cadenas[r,]$y
          d<- sqrt(dx^2 + dy^2)
                  if (d<radio){
             if (runif(1)<p){
        cadenas[r,]$polimero<-cadenas[r,]$polimero+1
        Ag[u,]$estado<-"O"
        Ag[u,]$dx<-0
        Ag[u,]$dy<-0
                    }
                  }
        }
    }
        }
  print(round(r*100/colocar))
  }

Ag.free<-Ag[Ag$estado=="F",]
ggplot(data=cadenas,aes(x=cadenas$x,y=cadenas$y))+
  geom_point(aes(color=factor(cadenas$polimero)),shape=20,size=0.5)+
  xlab("x")+ylab("y")+
  ggtitle(paste("Paso ",hora))+
  scale_color_discrete(name="\u{00C1}tomos de\nplata",
                       breaks=c(0:10, NA),
                       labels=c(0:10, "Pol\u{00ed}mero"))+
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white"))+
  geom_point(data=Ag.free,aes(x=Ag.free$x,y=Ag.free$y),color="darkgoldenrod1")
ggsave(paste("Paso_",hora,".png"))


Ag$x <- Ag$x + Ag$dx
Ag$y <- Ag$y + Ag$dy
Ag[Ag$x < 0,]$x<-Ag[Ag$x < 0,]$x+max.x
Ag[Ag$x > max.x,]$x<-Ag[Ag$x > max.x,]$x-max.x
Ag[Ag$y < 0,]$y<-Ag[Ag$y < 0,]$y+max.y
Ag[Ag$y > max.y,]$y<-Ag[Ag$y > max.y,]$y-max.y

}
