md <- 3
tc <- 5
vc<-4
k<-2
n<-200
datos<-data.frame()

pick.one <- function(x) {
  if (length(x) == 1) {
    return(x)
  } else {
    return(sample(x, 1))
  }
}
poli <- function(maxdeg, varcount, termcount) {
  f <- data.frame(variable=integer(), coef=integer(), degree=integer())
  for (t in 1:termcount) {
    var <- pick.one(1:varcount)
    deg <- pick.one(1:maxdeg)
    f <-  rbind(f, c(var, runif(1), deg))
  }
  names(f) <- c("variable", "coef", "degree")
  return(f)
}
eval <- function(pol, vars, terms) {
  value <- 0.0
  for (t in 1:terms) {
    term <- pol[t,]
    value <-  value + term$coef * vars[term$variable]^term$degree
  }
  return(value)
}

domin.by <- function(target, challenger, total) {
  if (sum(challenger < target) > 0) {
    return(FALSE) # hay empeora
  } # si no hay empeora, vemos si hay mejora
  return(sum(challenger > target) > 0)
}
evaluacion<-function(j){
  datos<-double()
  for (i in 1:n) {# para todos los objetivos
    res<- eval(obj[[j]], sol[i,], tc)
    datos<-rbind(datos,res)
  }
  return(datos)
}
dominios<- function(i){
  d <- logical()
  for (j in 1:n) {
    d <- c(d, domin.by(sign * val[i,], sign * val[j,], k))
  }
  
  return(sum(d))
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

  obj <- list()
  obj<-foreach(i=1:k,combine=rbind) %dopar% poli(md, vc, tc)
  
  minim <- (runif(k) > 0.5)
  sign <- (1 + -2 * minim)
  
  sol <- matrix(runif(vc * n), nrow=n, ncol=vc)
  val <- matrix(rep(NA, k * n), nrow=n, ncol=k)
  
  val<-foreach(j=1:k,.combine=cbind)%dopar%evaluacion(j)

  cual <- c("max", "min")
  xl <- paste("1� objetivo (", cual[minim[1] + 1], ")", sep="")
  yl <- paste("2� objetivo (", cual[minim[2] + 1], ")", sep="")
  
  dominadores <- integer()
  dominadores<-foreach(i=1:n,.combine=c)%dopar% dominios(i)
  
  no.dom <- logical()
  for (i in 1:n){
    no.dom<-c(no.dom,dominadores[i]==0)
  }
  stopImplicitCluster()
  
  frente <- subset(val, no.dom) # solamente las no dominadas
  nf= dim(frente)[1]
  frente<-as.data.frame(frente)
  colnames(frente)<-c("x","y")
  val<-as.data.frame(val)
  colnames(val)<-c("x","y")
  
  library(ggplot2)
  
  ggplot()+
    geom_point(data=val,aes(x,y))+
    xlab(xl)+ylab(yl)+
    geom_point(data=frente,aes(x,y),color="red",size=2)+
    ggtitle("a) Frente Original")
  ggsave("Frente_original.png")
  
 
  
  if (nf>2){
  n.frente<-frente[order(frente$x),]
  
  distancia<-c()
  for (i in 1:nf-1){
    d<- sqrt((n.frente[i,]$x-n.frente[i+1,]$x)**2+(n.frente[i,]$y-n.frente[i+1,]$y)**2)
    distancia<-c(distancia,d)
  }
  
  rd<-mean(distancia)
  
  mantener<-rep(FALSE,nf)
  
  for (i in 1:nf){
    if (n.frente[i,]==head(n.frente,n=1)||n.frente[i,]==tail(n.frente,n=1)){
      mantener[i]=TRUE}else{
        j<-max(which(mantener))
        d<-sqrt((n.frente[i,]$x-n.frente[j,]$x)**2+(n.frente[i,]$y-n.frente[j,]$y)**2)
        if(d>=rd){mantener[i]=TRUE}else{mantener[i]=FALSE}
      }
  }
  
  diverso<-subset(n.frente,mantener)
  
  ggplot()+
    geom_point(data=val,aes(x,y))+
    xlab(xl)+ylab(yl)+
    geom_point(data=frente,aes(x,y),color="red",size=2)+
    geom_point(data=diverso,aes(x,y),color="green",size=3)+
    ggtitle("b) Ambos frentes")
  ggsave("Ambos_frentes.png")
  
  
  
  ggplot()+
    geom_point(data=val,aes(x,y))+
    xlab(xl)+ylab(yl)+
    geom_point(data=diverso,aes(x,y),color="green",size=3)+
    ggtitle("c) Frente diversificado")
    ggsave("Frente_diverso.png")
  
    }