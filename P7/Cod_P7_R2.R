library(ggplot2)

f <- function(x) { # modificamos para que sea interesante
  return(5 * cos(14*x - 3) * sin(2*x^2 - 4 * x) + 2 * x^2 - 4 * x)
}

low <- -10
high <- -low
step <- 0.2
varE<- seq(0.6,0.9,0.1)
varT<-seq(200,1000,200)
tmax<-600
ciclos<-100


replicas<-function(r){
  resultados<-data.frame()
  datos<-data.frame()
  
  for (E in varE){
for (Temp in varT){
  x<- runif(1, low, high)
  T0<-Temp
  fx<-c()

for (pasos in 1:tmax){
 
dx=runif(1,-step,step)
xp<-x+dx
delta<-f(xp)-f(x)
if (delta>0){x<-xp}
else{if (runif(1)<exp(-delta/Temp)){x<-xp
  Temp=Temp*E}}
 
 fx<-c(fx,f(x))
 
}
  Vmax<-max(fx)
  datos<-cbind(r,T0,E,Vmax)
  resultados<-rbind(resultados,datos)
}
  }
  return(resultados)
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
maximos<-foreach(r=1:ciclos,.combine=rbind) %dopar% replicas(r)
stopImplicitCluster()

colnames(maximos)<-c("Replica","T0","E","Valor")
maximos$E<-as.factor(maximos$E)
maximos$T0<-as.factor(maximos$T0)

png("Variacion_T_E.png")
ggplot()+
geom_boxplot(data=maximos, aes(x = maximos$T0, y= maximos$Valor,fill=E))+
  scale_y_continuous(name="Valor máximo") +
  scale_x_discrete(name="T inicial")+
  geom_boxplot(position=position_dodge(1))

graphics.off()
  