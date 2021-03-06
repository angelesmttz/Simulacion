library(testit)
k<-10000
n <- 1000000

originales <- rnorm(k)
cumulos <- originales - min(originales) + 1
cumulos <- round(n * cumulos / sum(cumulos))
assert(min(cumulos) > 0)
diferencia <- n - sum(cumulos)

if (diferencia > 0) {
  for (i in 1:diferencia) {
    p <- sample(1:k, 1)
    cumulos[p] <- cumulos[p] + 1
  }
} else if (diferencia < 0) {
  for (i in 1:-diferencia) {
    p <- sample(1:k, 1)
    if (cumulos[p] > 1) {
      cumulos[p] <- cumulos[p] - 1
    }
  }
}

assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
assert(sum(cumulos) == n)
c <- median(cumulos) # tamanio critico de cumulos
d <- sd(cumulos) / 4 # factor arbitrario para suavizar la curva

#FUNCIONES
rotura <- function(x) {
  return (1 / (1 + exp((c - x) / d)))
}

union <- function(x) {
  return (exp(-x / c))
}

romperse <- function(tam, cuantos) {
  romper <- round(rotura(tam) * cuantos) # independientes
  resultado <- rep(tam, cuantos - romper) # los demas
  if (romper > 0) {
    for (cumulo in 1:romper) { # agregar las rotas
      t <- 1
      if (tam > 2) { # sample no jala con un solo valor
        t <- sample(1:(tam-1), 1)
      }
      resultado <- c(resultado, t, tam - t)
    }
  }
  stopifnot(sum(resultado) == tam * cuantos) # no hubo perdidas
  return(resultado)
}

para.romperse<-function (){
  cumulos <- integer()
  urna <- freq[i,]
  if (urna$tam > 1) { # no tiene caso romper si no se puede
    cumulos <- c(cumulos, romperse(urna$tam, urna$num))
  } else {
    cumulos <- c(cumulos, rep(1, urna$num))}
  
  return(cumulos)
}

para.unirse<-function(){
  cumulos <- integer()
  urna <- freq[i,]
  cumulos <- c(cumulos, unirse(urna$tam, urna$num))
  return(cumulos)
}

unirse <- function(tam, cuantos) {
  unir <- round(union(tam) * cuantos) # independientes
  if (unir > 0) {
    division <- c(rep(-tam, unir), rep(tam, cuantos - unir))
    stopifnot(sum(abs(division)) == tam * cuantos)
    return(division)
  } else {
    return(rep(tam, cuantos))
  }
}

para.nt<-function(){
  suma <- juntarse[2*i-1] + juntarse[2*i]
  return(suma)
}

freq <- as.data.frame(table(cumulos))
names(freq) <- c("tam", "num")
freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
duracion <- 20
digitos <- floor(log(duracion, 10)) + 1

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

for (paso in 1:duracion) {
  assert(sum(cumulos) == n)
  
  cumulos<-foreach(i=1:dim(freq)[1],.combine=c) %dopar% para.romperse()
  
  assert(sum(cumulos) == n)
  assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
  freq <- as.data.frame(table(cumulos)) # actualizar urnas
  names(freq) <- c("tam", "num")
  freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
  assert(sum(freq$num * freq$tam) == n)
  
  cumulos<-foreach(i=1:dim(freq)[1],.combine=c) %dopar% para.unirse()
  
  
  assert(sum(abs(cumulos)) == n)
  assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
  juntarse <- -cumulos[cumulos < 0]
  mayores <- cumulos[cumulos > 0]
  assert(sum(mayores) + sum(juntarse) == n)
  nt <- length(juntarse)
  
  if (nt > 0) {
    if (nt > 1) {
      juntarse <- sample(juntarse)
      pares<-foreach(i=1:floor(nt / 2),.combine=c) %dopar% para.nt()
      cumulos<-c(mayores,pares)
    }
    
    if (nt %% 2 == 1) {
      cumulos<-c(pares,mayores,juntarse[nt])
    }
  }
  
  assert(sum(cumulos) == n)
  freq <- as.data.frame(table(cumulos))
  names(freq) <- c("tam", "num")
  freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
  assert(sum(freq$num * freq$tam) == n)
  
  #png(paste("p8_ct",".para." ,paso, ".png", sep=""), width=300, height=300)
  hist(cumulos, 
       main=paste("Paso", paso, "con ambos fen\u{00f3}menos"), freq=FALSE,
       ylim=c(0, 0.06), xlab="Tama\u{00f1}o", ylab="Frecuencia relativa")
  #graphics.off()
  
  filtro<-mean(cumulos) + sd(cumulos) / 2
  pasof<-integer()
  
  for (f in 1:length(cumulos)){
    if (cumulos[f]<filtro){
      pasof<-c(pasof,cumulos[f])}
  }
  
  cumulos<-pasof
  freq <- as.data.frame(table(cumulos))
  names(freq) <- c("tam", "num")
  freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
  
  n<-sum(cumulos)
  
  
  assert(sum(freq$num * freq$tam) == n)
}

stopImplicitCluster()
