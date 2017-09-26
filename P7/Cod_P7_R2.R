library(ggplot2)


f <- function(x) { # modificamos para que sea interesante
  return(5 * cos(14*x - 3) * sin(2*x^2 - 4 * x) + 2 * x^2 - 4 * x)
}

low <- -2
high <- 4
step <- 0.2
varE<- seq(0.8,0.9,0.1)
varT<-seq(1,3001,1000)
tmax<-50
ciclos<-4

curr <- runif(1, low, high)
rnormal<-data.frame()
best<-curr

for (tiempo in 1:tmax) {
  delta <- runif(1, 0, step)
  
  left <- curr - delta
  right <- curr + delta
  fl <- f(left)
  fr <- f(right)
  
  if (fl > fr) {
    curr <- left
  } else {
    curr <- right
  }
  
  if (f(curr) > f(best)) {
    best <- curr
  }
  
  dnormal<-cbind(curr,f(curr),best,f(best),tiempo)
  rnormal<-rbind(rnormal,dnormal)
  
}

colnames(rnormal)<-c("x","f(x)","best","f(best)","paso")

replicas<-function(r){
  
  resultados<-data.frame()
  datos<-data.frame()
  rpasos<-data.frame()
  
for (E in varE){
  
for (Temp in varT){
  x<- runif(1, low, high)
  T0<-Temp
  #fx<-c()
  cuenta<-c()
  dpasos<-data.frame()
  best<-x
  
for (pasos in 1:tmax){
if (x<high){xi<-x
dx=runif(1,-step,step)
xp<-x+dx
delta<-f(xp)-f(x)

if (delta>0){x<-xp}
else{if (runif(1)<exp(-delta/Temp)){
  x<-xp
  Temp=Temp*E
  cuenta<-c(cuenta,1)
}}

if(f(x)>f(best)){best<-x}

dpasos<-cbind(r,E,T0,Temp,pasos,xi,xp,f(xi),f(xp),delta,sum(cuenta),best,f(best))
rpasos<-rbind(rpasos,dpasos)
}
  else
    break;
}
}
}
  return(rpasos)
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
datos<-foreach(r=1:ciclos,.combine=rbind) %dopar% replicas(r)
stopImplicitCluster()

colnames(datos)<-c("Replica","E","T0","Temperatura","Pasos","x","xp","f(x)","f(xp)","Delta","Aceptacion","Mejor","f(mejor)")


for (ve in 1:length(varE)){
g<-datos[datos$E==varE[ve],]


for (vt in 1:length(varT)){
  
g2<-g[g$T0==varT[vt],]
g2$Replica<- as.factor(g2$Replica)

ggplot() +
  geom_line(data=g2, aes(x = g2$Pasos, y= g2$`f(mejor)`,color=g2$Replica),size=0.6)+
  scale_y_continuous(name="Resultado de la función",limits = c(-10,30) )+
  scale_x_continuous(name="Paso")+
  ggtitle(paste("Temperatura",varT[vt],"E",varE[ve]))+
  geom_line(data=rnormal, aes(x = paso, y=rnormal$`f(best)`),linetype="dashed",colour="black")+
  labs(color="Réplica")

ggsave(paste("Variacion_E",varE[ve],"T",varT[vt],".png",sep="_"))

}
}
