library(parallel)
cluster<-makeCluster(detectCores()-1)

l <- 1.5
n <- 50
pi <- 0.1
pr <- 0.5
v <- l / 30
agentes <- data.frame(x = double(), y = double(), dx = double(), dy = double(), estado  = character())
for (i in 1:n) {
  e <- "S"
  if (runif(1) < pi) {
    e <- "I"
  }
  agentes <- rbind(agentes, data.frame(x = runif(1, 0, l), y = runif(1, 0, l),
                                       dx = runif(1, -v, v), dy = runif(1, -v, v),
                                       estado = e))
}

levels(agentes$estado) <- c("S", "I", "R")

agentes1<-agentes

epidemia <- integer()
r <- 0.5
infectados <- dim(agentes[agentes$estado == "I",])[1]
epidemia <- c(epidemia, infectados)
  if (infectados == 0) {
    break
  }

  contagios <- rep(FALSE, n)
 
   for (i in 1:n) { # posibles contagios
    a1 <- agentes[i, ]
    if (a1$estado == "I") { # desde los infectados
      for (j in 1:n) {
        if (!contagios[j]) { # aun sin contagio
          a2 <- agentes[j, ]
          if (a2$estado == "S") { # hacia los susceptibles
            dx <- a1$x - a2$x
            dy <- a1$y - a2$y
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
  }
  
  cont2<-contagios
  
  dEstado <-function(i){
       if (contagios[i]){agentes[i,5]="I"} else {if (agentes[i,5]=="I"){
         if (runif(1) < pr) {
           agentes[i,5] <- "R"
         } else {agentes[i,5] <- "R"}
    }
       }
    {agentes[i,5]<-agentes[i,5]}
  }

  clusterExport(cluster,"agentes")
  clusterExport(cluster,"contagios")
  clusterExport(cluster,"pr")
  
  agentes$estado<-parSapply(cluster,1:n,dEstado)
  

  stopCluster(cluster)
  

  comparacion<-cbind(agentes1[,5],cont2,agentes[,5])
 rm(list=ls())
 gc()
 