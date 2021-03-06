ti<-Sys.time()

binario <- function(d, l) {
  b <-  rep(FALSE, l)
  while (l > 0 | d > 0) {
    b[l] <- (d %% 2 == 1)
    l <- l - 1
    d <- bitwShiftR(d, 1)
  }
  return(b)
}

decimal <- function(bits, l) {
  valor <- 0
  for (pos in 1:l) {
    valor <- valor + 2^(l - pos) * bits[pos]
  }
  return(valor)
}

prueba<-function(t){
  d <- combinaciones[t]
  pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
  correcto <- binario(d, n)
  salida <- rep(FALSE, n)
  
  for (i in 1:n) {
    w <- neuronas[i,]
    deseada <- correcto[i]
    resultado <- sum(w * pixeles) >= 0
    salida[i] <- resultado
  }
  
   r <- min(decimal(salida, n), k) # todos los no-existentes van al final
  
  if (r==d){return(TRUE)}
}

modelos <- read.csv("digitos.modelo", sep=" ", header=FALSE, stringsAsFactors=F)
modelos[modelos=='n'] <- negro
modelos[modelos=='g'] <- gris
modelos[modelos=='b'] <- blanco

r <- 5
c <- 3
dim <- r * c
tasa <- 0.15
tranqui <- 0.993
tope <- 9
digitos <- 0:tope
k <- length(digitos)
n <- floor(log(k-1, 2)) + 1
neuronas <- matrix(runif(n * dim), nrow=n, ncol=dim)
test<-t.pruebas/length(digitos)
combinaciones<-rep(digitos,test)
combinaciones<-sample(combinaciones)


for (t in 1:5000) { # entrenamiento
  d <- sample(0:tope, 1)
  pixeles <- runif(dim) < modelos[d + 1,]
  correcto <- binario(d, n)
  for (i in 1:n) {
    w <- neuronas[i,]
    deseada <- correcto[i]
    resultado <- sum(w * pixeles) >= 0
    if (deseada != resultado) {
      ajuste <- tasa * (deseada - resultado)
      tasa <- tranqui * tasa
      neuronas[i,] <- w + ajuste * pixeles
    }
  }
}

acierto<-foreach(t=1:t.pruebas,.combine=rbind)%dopar%prueba(t)
acierto<-sum(acierto)/t.pruebas*100

tf<-Sys.time()

tiempo<-difftime(tf,ti,units="secs")