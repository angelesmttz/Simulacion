library(ggplot2)

f <- function(x) {
  return(cos(14.5*x - 0.3) + x * (x + 0.2) + 1.01)
}

low <- -20
high <- -low
x <- seq(low, high, 0.05)
step <- 0.3

tmax <- 50
varE<-seq(0.5,0.9,0.1)
varT<-seq(100,500,100)

for (E in varE){
  for (Temp in varT){
    
    curr<- runif(1, low, high)
    T0<-Temp
  
    for (pasos in 1:tmax){
      
    dx=runif(1,-step,step)
    xp<-curr+dx
    delta<-f(xp)-f(curr)
      
    if (delta>0){
      best<-xp
      curr<-xp}else{
      best<-curr
      if (runif(1)<exp(-(f(xp)-f(curr))/Temp)){
        curr<-xp
        Temp=Temp*E}}
      
      
        ggplot()+
        geom_line(aes(x,f(x)))+
        geom_point(aes(curr,f(curr)),size=3,color="red")+
        geom_vline(xintercept = best, linetype="solid", color = "green",size=1)+
        ggtitle(paste("Paso",pasos,sep=" "))+
        theme(plot.title = element_text(hjust = 0.5))+
        scale_y_continuous(name="f(x)") 
        
        ggsave(paste("p7","E",E,"T",T0,"Paso",pasos,".png", sep="_"))
    }
  }
}