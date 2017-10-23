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

vc <- 4
md <- 3
tc <- 5
k <- 2 # cuantas funciones objetivo
obj <- list()
for (i in 1:k) {
  obj[[i]] <- poli(vc, md, tc)
}
minim <- (runif(k) > 0.5)
sign <- (1 + -2 * minim)
n <- 100 # cuantas soluciones aleatorias
sol <- matrix(runif(vc * n), nrow=n, ncol=vc)
val <- matrix(rep(NA, k * n), nrow=n, ncol=k)
for (i in 1:n) { # evaluamos las soluciones
  for (j in 1:k) { # para todos los objetivos
    val[i, j] <- eval(obj[[j]], sol[i,], tc)
  }
}
mejor1 <- which.max(sign[1] * val[,1])
mejor2 <- which.max(sign[2] * val[,2])
cual <- c("max", "min")
xl <- paste("Primer objetivo (", cual[minim[1] + 1], ")", sep="")
yl <- paste("Segundo objetivo (", cual[minim[2] + 1], ")", sep="")

no.dom <- logical()
dominadores <- integer()
for (i in 1:n) {
  d <- logical()
  for (j in 1:n) {
    d <- c(d, domin.by(sign * val[i,], sign * val[j,], k))
  }
  cuantos <- sum(d)
  dominadores <- c(dominadores, cuantos)
  no.dom <- c(no.dom, cuantos == 0) # nadie le domina
}

frente <- subset(val, no.dom) # solamente las no dominadas

frente<-as.data.frame(frente)
colnames(frente)<-c("x","y")
n.frente<-frente[order(frente$x),]
nf= dim(n.frente)[1]


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

frente2<-subset(n.frente,mantener)

png("Frente_original1.png")
plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
     ylab=paste(yl,"mejor con bolita naranja"),
     main="Ejemplo bidimensional")
points(frente[,1], frente[,2], col="red", pch=16, cex=1.5)
graphics.off()

png("Frente_propuesto1.png")
plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
     ylab=paste(yl,"mejor con bolita naranja"),
     main="Ejemplo bidimensional")
points(frente[,1], frente[,2], col="red", pch=16, cex=1.5)
points(frente2[,1], frente2[,2], col="green", pch=16, cex=1.5)
graphics.off()

png("Frente_nuevo1.png")
plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
     ylab=paste(yl,"mejor con bolita naranja"),
     main="Ejemplo bidimensional")
points(frente2[,1], frente2[,2], col="green", pch=16, cex=1.5)
graphics.off()