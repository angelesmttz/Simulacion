library(parallel)
cluster<-makeCluster(detectCores()-1)
resultados<-data.frame()

for(p in 1:9){
  vivir<-parSapply(cluster,1:9,function(x){x/10})
   clusterExport(cluster,"vivir")
  dim<-10
  num<-dim**2
  v<-c()
  
  for (corrida in 1:100){
    
    actual <-matrix(sample(c(0,1),num ,prob=c(1-vivir[p],vivir[p]),replace=TRUE), nrow=dim, ncol=dim)
    suppressMessages(library("sna"))
    png(paste("P",vivir[p],"C",corrida,"Paso_0",".png",sep="_"))
    plot.sociomatrix(actual, diaglab=FALSE, main="Inicio")
    graphics.off()
    
  paso <- function(pos) {
    fila <- floor((pos - 1) / dim) + 1
    columna <- ((pos - 1) %% dim) + 1
    vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),
                        max(columna - 1, 1): min(columna + 1, dim)]
    return(1 * ((sum(vecindad) - actual[fila, columna]) == 3))
  }
  
  clusterExport(cluster, "dim")
  clusterExport(cluster, "paso")
  
 
  for (iteracion in 1:15) {
    clusterExport(cluster, "actual")
    siguiente <- parSapply(cluster, 1:num, paso)
    if (sum(siguiente) == 0) { 
            break;
    }
    actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
    salida = paste("P",vivir[p],"C",corrida,"Paso",iteracion,".png", sep="_")
    tiempo = paste("Paso", iteracion,sep=" ")
    png(salida)
    plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)
    graphics.off()
    
  }
  
    v<-c(v,iteracion-1)
  }

resultados<-rbind(resultados,v)

}

rownames(resultados)<-vivir

stopCluster(cluster)

png("Probabilidad.png")
boxplot(data.matrix(resultados), use.cols=FALSE, 
        xlab="Probabilidad", ylab="Paso",
                main="Iteraciones en f(probabilidad de vida)")
graphics.off()