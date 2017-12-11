#####Condiciones######
concentracion.cmc<-15 #mg/mL
volumen.cmc<-20 #mL
contenido.plata<-1.5 #mg
mw<-250*(10**3) #k(g/mol)
DS=1.2
temperatura<-90 #Centigrados
tiempo<-10 #horas


#####Timing#####
ti<-Sys.time()


#####Ajustes####
reduccion<-0.4*(10**-15)
options(digits=3)
p.point<-0.9
n.point<-1
Ag.point<-0.7

#####Calculos######
avogrado<-6.023*(10**23) #avogrado
cmc<-volumen.cmc*concentracion.cmc*(10**-3)  #mg
moleculas.cmc<- round(avogrado*cmc*reduccion/mw)
plata<-contenido.plata*(10**-3) #gramos
masa.plata<-108 #g/mol
atomos.plata<- round(avogrado*plata*reduccion/masa.plata)
posiciones<-c(2,3,5)

#####Experimentales#####
Grado<-1.2
taille<- c(21.9, 21.1,19,15.5)
var<-c(7.5,5,7.9,3.4)
silver<-c(1.5,3,6,8)
experimentales<-data.frame(Sustitucion=Grado,Plata=silver,
                           Variacion=var,Size=taille)
Ag.r<-160*(10**-12)
Ag.A<-pi*(Ag.r**2)

for (f in 1:dim(experimentales)[1]){
  if (experimentales[f,]$Sustitucion==DS & 
      experimentales[f,]$Plata==contenido.plata){
    Ag.exp<-rnorm(500,mean=experimentales[f,]$Size,
                  sd=experimentales[f,]$Variacion) #nm
    Ag.exp<-data.frame(Ag.exp)
  }
}
colnames(Ag.exp)<-"Size"
Ag.exp$Area<-pi*((Ag.exp$Size*(10**-9))**2)/4 #m**2
Ag.exp$nanoparticulas<-Ag.exp$Area/Ag.A
relacion<-atomos.plata*100/sum(Ag.exp$nanoparticulas)
Ag.exp$nanoparticulas<-Ag.exp$nanoparticulas*relacion

library(ggplot2)
ggplot(data=Ag.exp,aes(Ag.exp$nanoparticulas))+
  geom_histogram(bins=50, color = "black", fill = "gray")+
  ylab("Frecuencia")+
  xlab("\u{00C1}tomos de plata")+
  ggtitle(paste(contenido.plata, " mg de plata"))+
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white"))
ggsave (paste("Par_Experimental_",contenido.plata,".png"))


#####Funciones######
polimerizacion<-function (){
  base<-rep("P",7)
  for (j in 1:length(posiciones)){
    cation<-posiciones[j]
    if (runif(1,0,1)<(DS/3)){
      base[cation]<-0
    }
  }
  return(base)
}

distribucion<-function(u){
    if (Ag[u,]$estado=="F"){
    dx<-Ag[u,]$x-nucleos[r,]$x
    dy<-Ag[u,]$y-nucleos[r,]$y
    d<- sqrt(dx^2 + dy^2)
    
    if (d<radio){Union<-TRUE}else{Union<-FALSE}
    }else{Union<-FALSE}
    
  if(Union){
    puntos<-data.frame(x=nucleos[r,]$x,y=nucleos[r,]$y,
                       dx=0,dy=0,
                       estado="O",Union=1)
  }else{
    puntos<-data.frame(x=Ag[u,]$x,y=Ag[u,]$y,
                       dx=Ag[u,]$dx,dy=Ag[u,]$dy,
                       estado=Ag[u,]$estado,Union=0)
  }
  
   
  return(puntos)
}

#####Librerias#####
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))


#####Ubicacion#####
polimero<-foreach(1:moleculas.cmc,.combine=c)%dopar%polimerizacion()
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
    paso<-runif(1,0.002,0.005)
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


poli<-cadenas[cadenas$polimero=="P",]
nucleos<-cadenas[cadenas$polimero!="P",]
cationes<-dim(nucleos)[1]

colnames(nucleos)<-c("Cantidad","x","y")
nucleos$Cantidad<-as.numeric(levels(nucleos$Cantidad))[nucleos$Cantidad]

ggplot()+
  geom_point(data=poli,aes(poli$x,poli$y),color="skyblue3",size=p.point)+
  geom_point(data=nucleos,aes(nucleos$x,nucleos$y,
                              color=factor(nucleos$Cantidad)),size=n.point)+
  scale_color_discrete(name="\u{00C1}tomos de\nplata")+
  xlab("x")+ylab("y")+
  ggtitle("Pol\u{00ed}mero en soluci\u{00f3}n")+
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white"))
ggsave("Par_Polimero.png")


atomos.plata<-atomos.plata*(10**-1)
Ag<- data.frame(x = runif(atomos.plata,min(cadenas$x),max(cadenas$x)),
                y=runif(atomos.plata,min(cadenas$y),max(cadenas$y)),
                dx=runif(atomos.plata,-max.x/50,max.x/50),
                dy=runif(atomos.plata,-max.y/50,max.y/50))  

ggplot()+
  geom_point(data=poli,aes(poli$x,poli$y),color="skyblue3",size=p.point)+
  geom_point(data=nucleos,aes(nucleos$x,nucleos$y,
                              color=factor(nucleos$Cantidad)),size=n.point)+
  xlab("x")+ylab("y")+
  ggtitle("Estado inicial")+
  scale_color_discrete(name="\u{00C1}tomos de\nplata")+
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white"))+
  geom_point(data=Ag,aes(x=Ag$x,y=Ag$y),color="darkgoldenrod1",
             size=Ag.point)
ggsave("Par_Inicial.png")

#####Velocidad######

R<- 8.3144598 #(Kg)(m^2)/(s^2)(k)(mol)
T.k<-temperatura+273
mol.plata<- masa.plata*(10^-3)
k<- 1.38 *(10**-23) #J/K
Ag.v<- round(sqrt(3*R*T.k/mol.plata))#m/s


######Union######

radio<-abs(paso)/2
Ag$estado<-"F"
paralelo<-data.frame()

for (hora in seq(0,tiempo,1)){
  ti<-Sys.time()
  for (r in 1:cationes){
  Ag<-foreach(u=1:atomos.plata,.combine=rbind)%dopar%distribucion (u)
    nucleos[r,]$Cantidad<-sum(Ag$Union)
  }
  tf<-Sys.time()
  t<-difftime(tf,ti,units="mins")
  lapso<-cbind(hora,t)
  paralelo<-rbind(paralelo,lapso)
  
  Ag.free<-Ag[Ag$estado=="F",]
  
  ggplot(data=Ag.free,aes(Ag.free$x,Ag.free$y))+
    geom_point(color="darkgoldenrod1",size=Ag.point)+
    geom_point(data=poli,aes(poli$x,poli$y),color="skyblue3",size=p.point)+
    geom_point(data=nucleos,aes(nucleos$x,nucleos$y,
                                color=factor(nucleos$Cantidad)),size=n.point)+
    xlab("x")+ylab("y")+
    ggtitle(paste("Paso ",hora))+
    scale_color_discrete(name="\u{00C1}tomos de\nplata")+
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "white"))
  ggsave(paste("Par_Paso_",hora,".png"))
  
  
  Ag$x <- Ag$x + Ag$dx
  Ag$y <- Ag$y + Ag$dy
  Ag[Ag$x < 0,]$x<-Ag[Ag$x < 0,]$x+max.x
  Ag[Ag$x > max.x,]$x<-Ag[Ag$x > max.x,]$x-max.x
  Ag[Ag$y < 0,]$y<-Ag[Ag$y < 0,]$y+max.y
  Ag[Ag$y > max.y,]$y<-Ag[Ag$y > max.y,]$y-max.y
  
  print(hora)
}

stopImplicitCluster()

ggplot(data=nucleos,aes(nucleos$Cantidad))+
  geom_histogram(bins=50, color = "black", fill = "gray")+
  ylab("Frecuencia")+
  xlab("\u{00C1}tomos de plata")+
  ggtitle(paste(contenido.plata, " mg de plata"))+
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white"))
ggsave (paste("Par_Simulado_",contenido.plata,".png"))



save.image(file="Simulado.RData")