ti<-Sys.time()

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
p <- poblacion.inicial(n, init)
tam <- dim(p)[1]
assert(tam == init)
mejores <- double()

for (iter in 1:tmax) {
  p$obj <- NULL
  p$fact <- NULL
  p<-rbind(p,foreach(i=1:tam,.combine = rbind)%dopar% para.mut(i))
  p<-rbind(p,foreach(i=1:rep,combine=rbind)%dopar% para.rep(i))
  
  tam <- dim(p)[1]
  obj <- double()
  fact <- integer()
  
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