ti<-Sys.time()
para.mut<-function(i){
  if (runif(1) < pm) {
    return(mutacion(p[i,],n))}
}

para.rep<-function(i){
  padres <- sample(1:tam, 2, replace=FALSE)
  hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
  p.hijo<- hijos[1:n] # primer hijo
  s.hijo<- hijos[(n+1):(2*n)] # segundo hijo
  son<-rbind(p.hijo,s.hijo)
  return(son)
}

para.obj<-function(i){
  obj <- objetivo(p[i,], valores)
  fact <-factible(p[i,], pesos, capacidad)
  datos<-cbind(obj,fact)
  return(datos)
}
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

for (iter in 1:tmax) {
  
  p$obj <- NULL
  p$fact <- NULL
  
  for (i in 1:tam) { # cada objeto puede mutarse con probabilidad pm
    if (runif(1) < pm) {
      p <- rbind(p, mutacion(p[i,], n))
    }
  }
  
  p<-rbind(p,foreach(i=1:rep,combine=rbind)%dopar% para.rep(i))
  
  tam <- dim(p)[1]
  obj <- double()
  fact <- integer()
  
  rownames(p)<-c(1:dim(p)[1])
  
  p<- data.frame(sapply(p, function(x) as.numeric(as.character(x))))
  p<-cbind(p,foreach(i=1:tam,.combine=rbind)%dopar% para.obj(i))
  
  mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
  
  p <- p[mantener,]
  tam <- dim(p)[1]
  assert(tam == init)
  factibles <- p[p$fact == TRUE,]
  mejor <- max(factibles$obj)
  mejores <- c(mejores, mejor)
}

stopImplicitCluster()

tf<-Sys.time()

t<-tf-ti