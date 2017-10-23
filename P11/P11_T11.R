ti<-Sys.time()

obj <- list()
obj<-foreach(i=1:k,combine=rbind) %dopar% poli(md, vc, tc)

minim <- (runif(k) > 0.5)
sign <- (1 + -2 * minim)

sol <- matrix(runif(vc * n), nrow=n, ncol=vc)
val <- matrix(rep(NA, k * n), nrow=n, ncol=k)

val<-foreach(j=1:k,.combine=cbind)%dopar%evaluacion(j)
no.dom <- logical()
dominadores <- integer()
dominadores<-foreach(i=1:n,.combine=c)%dopar% dominios(i)


for (i in 1:n){
  no.dom<-c(no.dom,dominadores[i]==0)
}

frente <- subset(val, no.dom)

tf<-Sys.time()

t<-difftime(tf,ti,units="secs")