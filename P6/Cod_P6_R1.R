probabilidad<-function(j){
  contagios <- rep(FALSE, n)
  for (i in 1:n) {
    if (agentes[i,5] == "I") { # desde los infectados
      if (!contagios[j]) { # aun sin contagio
        if (agentes[j,5] == "S") { # hacia los susceptibles
          dx <- agentes[i,1] - agentes[j,1]
          dy <- agentes[i,2] - agentes[j,2]
          d <- sqrt(dx^2 + dy^2)
          if (d < r) { # umbral
            p <- (r - d) / r
            if (runif(1) < p) {
              contagios[j] <- TRUE
            }
          }
        }
      }
    }
  }
  return(contagios[j])
}

dEstado <-function(i){
  if (enfermos[i]){agentes[i,5]="I"}
  else { if (agentes[i,5]=="I" &
             runif(1) < pr) {agentes[i,5] <- "R"}}
  agentes[i,5]<-agentes[i,5]
}

l <- 1.5
n<-50
pi <- 0.05
pr<-0.02
pv<-0.1
v <- l / 30
r <- 0.1
epidemia <- integer()
tmax <- 100
digitos <- floor(log(tmax, 10)) + 1
graficos<-TRUE

agentes <- data.frame(x = double(), y = double(), dx = double(), dy = double(), estado  = character())

for (i in 1:n) {
  if (runif(1) < pi) {e <- "I"}
  else{if(runif(1)<pv){e<-"V"}else{e<-"S"}}
  
  agentes <- rbind(agentes, data.frame(x = runif(1, 0, l), y = runif(1, 0, l),
                                       dx = runif(1, -v, v), dy = runif(1, -v, v),
                                       estado = e))
}

levels(agentes$estado) <- c("S", "V", "I","R")

clusterExport(cluster,"r")
clusterExport(cluster,"n")
clusterExport(cluster,"pr")

for (tiempo in 1:tmax) {
  
  infectados <- dim(agentes[agentes$estado == "I",])[1]
  epidemia <- c(epidemia, infectados)
  if (infectados == 0) {
    break
  }
  
  clusterExport(cluster,"agentes")

  enfermos<-parSapply(cluster,1:n,probabilidad)
  
  clusterExport(cluster,"enfermos")
  
  agentes$estado<-parSapply(cluster,1:n,dEstado)
  
  agentes$x <- agentes$x + agentes$dx
  agentes$y <- agentes$y + agentes$dy
  
  agentes[agentes$x < 0,1]<-agentes[agentes$x < 0,1]+l
  agentes[agentes$x > l,1]<-agentes[agentes$x > l,1]-l
  agentes[agentes$y < 0,2]<-agentes[agentes$y < 0,2]+l
  agentes[agentes$y > l,2]<-agentes[agentes$y > l,2]-l
  
if (graficos==TRUE){
  aS <- agentes[agentes$estado == "S",]
  aI <- agentes[agentes$estado == "I",]
  aR <- agentes[agentes$estado == "R",]
  aV <- agentes[agentes$estado == "v",]
  tl <- paste(tiempo, "", sep="")
  
  while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
  }
  
  salida <- paste("p6_t", tl, ".png", sep="")
  tiempo <- paste("Paso", tiempo)
  
  png(salida)
  plot(l, type="n", main=tiempo, xlim=c(0, l), ylim=c(0, l), xlab="x", ylab="y")
  if (dim(aS)[1] > 0) {
    points(aS$x, aS$y, pch=15, col="chartreuse3", bg="chartreuse3")
  }
  if (dim(aI)[1] > 0) {
    points(aI$x, aI$y, pch=16, col="firebrick2", bg="firebrick2")
  }
  if (dim(aR)[1] > 0) {
    points(aR$x, aR$y, pch=17, col="goldenrod", bg="goldenrod")
  }
  if (dim(aV)[1] > 0) {
    points(aV$x, aV$y, pch=20, col="darkblue", bg="darkblue")
  }
  graphics.off()
}
}


if (graficos==TRUE){
  png("p6e.png", width=600, height=300)
plot(1:length(epidemia), 100 * epidemia / n, xlab="Tiempo", ylab="Porcentaje de infectados")
graphics.off()
}