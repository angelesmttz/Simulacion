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

print(agentes)

agentes$x <- agentes$x + agentes$dx
agentes$y <- agentes$y + agentes$dy

agentes[agentes$x < 0,1]<-agentes[agentes$x < 0,1]+l
agentes[agentes$x > l,1]<-agentes[agentes$x > l,1]-l
agentes[agentes$y < 0,2]<-agentes[agentes$y < 0,2]+l
agentes[agentes$y > l,2]<-agentes[agentes$y > l,2]-l


rm(list=ls())
gc()

