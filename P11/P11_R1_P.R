ciclos<-5
md <- 3
tc <- 5
vc<-4
graficos<-FALSE
k<-2
vark<-2:16
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
 
  mejor1 <- which.max(sign[1] * val[,1])
  mejor2 <- which.max(sign[2] * val[,2])
  cual <- c("max", "min")
  xl <- paste("Primer objetivo (", cual[minim[1] + 1], ")", sep="")
  yl <- paste("Segundo objetivo (", cual[minim[2] + 1], ")", sep="")
  
  dominadores <- integer()
  dominadores<-foreach(i=1:n,.combine=c)%dopar% dominios(i)
  
  no.dom <- logical()
  for (i in 1:n){
    no.dom<-c(no.dom,dominadores[i]==0)
  }
  
  frente <- subset(val, no.dom) # solamente las no dominadas
  nf= dim(frente)[1]
  
  png("Frente_original11.png")
  plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
       ylab=paste(yl,"mejor con bolita naranja"),
       main="Ejemplo bidimensional")
  points(frente[,1], frente[,2], col="red", pch=16, cex=1.5)
  graphics.off()
  
  if (nf>2){
  frente<-as.data.frame(frente)
  colnames(frente)<-c("x","y")
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
  
  png("Frente_propuesto11.png")
  plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
       ylab=paste(yl,"mejor con bolita naranja"),
       main="Ejemplo bidimensional")
  points(frente[,1], frente[,2], col="red", pch=16, cex=1.5)
  points(diverso[,1], diverso[,2], col="green", pch=16, cex=1.5)
  graphics.off()
  
  png("Frente_nuevo11.png")
  plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
       ylab=paste(yl,"mejor con bolita naranja"),
       main="Ejemplo bidimensional")
  points(diverso[,1], diverso[,2], col="green", pch=16, cex=1.5)
  graphics.off()
  
  }