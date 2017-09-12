library(ggplot2)
library(parallel)

varruns<-seq (100000,700000,100000)
ciclos<-40

dat<-data.frame()
res<-data.frame()

rpi<-3.1415926535
digpi<-as.matrix(substring(rpi,seq(nchar(rpi)),seq(nchar(rpi))))

cluster <- makeCluster(detectCores() - 1)

for (runs in varruns){
  
 for (replica in 1:ciclos){
 
t<-system.time({
xs <- runif(runs,min=-0.5,max=0.5)
ys <- runif(runs,min=-0.5,max=0.5)
in.circle <- xs^2 + ys^2 <= 0.5^2
mc.pi <- (sum(in.circle)/runs)*4
  }
)

digmc<-matrix(substring(mc.pi,seq(nchar(mc.pi)),seq(nchar(mc.pi))))
for (new in 1:abs((length(digpi)-length(digmc)))){
  digmc<-rbind(digmc,0)
}

d<-numeric()

for (p in 3:12){
  if (digpi[p]== digmc[p]){d<-c(d,TRUE)}
  else
    break;
}

igu<-sum(d)
dif<-abs(mc.pi-rpi)
dat<-cbind(mc.pi,(runs/100000),igu,dif,t[3])
res<-rbind(res,dat)

  }
}

stopCluster(cluster)

colnames(res)<-c("Valor","Muestra","Presicion","Diferencia","Tiempo")
res$Muestra<-as.factor(res$Muestra)

png("Variacion_pi_digitos.png")
ggplot(data=res,aes(x=Muestra,y=Presicion))+
  geom_boxplot(fill="skyblue3")+
  scale_y_continuous(name="Precision")+
  scale_x_discrete(name="Numero de muestras (10^5)")
  
  
png("Variacion_pi_diferencia.png")
ggplot(data=res,aes(x=Muestra,y=Diferencia))+
  geom_boxplot(fill="steelblue")+
  scale_y_continuous(name="Diferencia")+
  scale_x_discrete(name="Numero de muestras (10^5)")

png("Variacion_pi_tiempo.png")
ggplot(data=res,aes(x=Muestra,y=Tiempo))+
  geom_boxplot(fill="lightseagreen")+
  scale_y_continuous(name="Tiempo")+
  scale_x_discrete(name="Numero de muestras (10^5)")


graphics.off()

rm(list=ls())
gc()