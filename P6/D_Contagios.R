library(parallel)
cluster<-makeCluster(detectCores()-1)

l <- 1.5
n <- 50
pi <- 0.05
pr <- 0.02
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

r<-0.1

probabilidad<-function(j){
  contagios <- rep(FALSE, n)
  for (i in 1:n) {
    a1 <- agentes[i, ]
    if (a1$estado == "I") { # desde los infectados
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
  return(contagios[j])
}

clusterExport(cluster,"agentes")
clusterExport(cluster,"r")
clusterExport(cluster,"n")

enfermos<-parSapply(cluster,1:n,probabilidad)

print(enfermos)

stopCluster(cluster)