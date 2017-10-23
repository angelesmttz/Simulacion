source('~/GitHub/Simulacion/Simulacion/P3/Obt_primos.R')

primo <- function(n) {
  if (n == 1 || n == 2) {
    return(TRUE)
  }
  if (n %% 2 == 0) {
    return(FALSE)
  }
  for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
    if ((n %% i) == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

desde <- 1000
hasta <-  2000
original <- desde:hasta
invertido <- hasta:desde
prgr<-tail(primos,hasta-desde)
prpq<-primos[hasta-desde]
replicas <- 25
suppressMessages(library(doParallel))
resultados<-data.frame()
par(mfrow=c(1,(detectCores()-1)))

for (t in 1:(detectCores()-1)){
registerDoParallel(makeCluster(t))
ot <-  numeric()
it <-  numeric()
at <-  numeric()
gt<-numeric()
pt<-numeric()


for (r in 1:replicas) {
  ot <- c(ot, system.time(foreach(n = original, .combine=c) %dopar% primo(n))[3]) 
  it <- c(it, system.time(foreach(n = invertido, .combine=c) %dopar% primo(n))[3])
  at <- c(at, system.time(foreach(n = sample(original), .combine=c) %dopar% primo(n))[3])
  gt <- c(gt, system.time(foreach(n = prgr, .combine=c) %dopar% primo(n))[3])
  pt <- c(pt, system.time(foreach(n = prpq, .combine=c) %dopar% primo(n))[3])

  }

stopImplicitCluster()

resultados<-rbind(ot,it,at,gt,pt)
boxplot(data.matrix(resultados), use.cols=FALSE, 
        xlab="Ordanamiento", ylab="Tiempo",ylim=c(0,4),
        main=paste("Uso de",t,"nucleo(s)")
)

}
