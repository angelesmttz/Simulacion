ciclos<-5
md <- 3
tc <- 5
vc<-4
graficos<-FALSE
vark<-seq(5,15,5)
varn<-seq(200,1000,200)
resultados<-data.frame()

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
   for (n in varn){
    
    source('~/GitHub/Simulacion/Simulacion/P11/P11.R')
    paralelo<-cbind("original",replicas,k,n,t)
    
    source('~/GitHub/Simulacion/Simulacion/P11/P11_T11.R')
    secuencial<-cbind("paralelo",replicas, k,n, t)
    
    resultados<-rbind(resultados,paralelo,secuencial)
     }
  }
  print(replicas)
}

stopImplicitCluster()

save.image(file="Resultados_Tarea_11.RData")

colnames(resultados)<-c("Tipo","Replica","Objetivos","Soluciones","Tiempo")
resultados$Tipo <- as.factor(resultados$Tipo)
resultados$Soluciones <- as.factor(resultados$Soluciones)
resultados$Objetivos <- as.factor(resultados$Objetivos)
resultados$Tiempo<-as.numeric(levels(resultados$Tiempo))[resultados$Tiempo]


library(ggplot2)
png("Paralelizacion_k5.png")
ggplot(data=resultados, aes(x = Soluciones, y= Tiempo,color=Objetivos)) +
  geom_boxplot()+facet_grid(Tipo~.,switch = "both")+
  theme(legend.position = "bottom")+
  theme_bw()
  
dev.off()

