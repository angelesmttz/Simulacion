ti<-Sys.time()
obj <- list()

for (i in 1:k) {
  obj[[i]] <- poli(md, vc, tc)
}
minim <- (runif(k) > 0.5)
sign <- (1 + -2 * minim)
sol <- matrix(runif(vc * n), nrow=n, ncol=vc)
val <- matrix(rep(NA, k * n), nrow=n, ncol=k)

for (i in 1:n) { # evaluamos las soluciones
  for (j in 1:k) { # para todos los objetivos
    val[i, j] <- eval(obj[[j]], sol[i,], tc)
  }
}

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

tf=Sys.time()

t<-difftime(tf,ti,units="secs")
print(t)