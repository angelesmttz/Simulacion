ciclos<-50
md <- 3
tc <- 5
vc<-4
graficos<-FALSE
vark<-2:10
n<-200
datos<-data.frame()

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

evaluacion<-function(j){
  datos<-double()
  for (i in 1:n) {# para todos los objetivos
    res<- eval(obj[[j]], sol[i,], tc)
    datos<-rbind(datos,res)
  }
  return(datos)
}
dominios<- function(i){
  d <- logical()
  for (j in 1:n) {
    d <- c(d, domin.by(sign * val[i,], sign * val[j,], k))
  }
  
  return(sum(d))
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
for (replicas in 1:ciclos){
for (k in vark){
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

frente <- subset(val, no.dom) # solamente las no dominadas
tam<-dim(frente)[1]

res<-cbind(k,tam,replicas)
datos<-rbind(datos,res)

}
  print(replicas)
}

stopImplicitCluster()

colnames(datos)<-c("Objetivos","Soluciones","Replica")
datos$Objetivos<-as.factor(datos$Objetivos)

library(ggplot2)

ggplot(data=datos,aes(datos$Objetivos,(datos$Soluciones*100/n))) +
  geom_violin(scale="width",fill="dodgerblue4")+
  geom_boxplot(width=0.25,fill="gainsboro", color="black",outlier.size = 0.1) +
  xlab("Objetivos") +
  ylab("Frecuencia (%)")
ggsave("Frecuncia_objetivos_no.png")


