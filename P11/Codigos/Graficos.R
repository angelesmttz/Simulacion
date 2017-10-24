res<-c()
for ( comb in 1:k){
  res<-c(res,k-comb)
}
posibilidades<-sum(res)

divisor<-2
while (posibilidades%% divisor!= 0){
  divisor<-divisor+1
}

png("graficos_k_t.png",width =1080,height = 720 )
par(mfrow=c(posibilidades/divisor,divisor))

for (i in seq(1,k-1,1)){
  for (j in seq(i+1,k,1)){
    
    mejor1 <- which.max(sign[i] * val[,i])
    mejor2 <- which.max(sign[j] * val[,j])
    cual <- c("max", "min")
    
    xl <- paste(i,"° objetivo (", cual[minim[1] + 1], ")", sep="")
    yl <- paste(j,"° objetivo (", cual[minim[1] + 2], ")", sep="")
    
    plot(val[,i], val[,j], xlab=xl,
         ylab=yl)
    points(val[mejor1, i], val[mejor1, j], col="blue", pch=15, cex=1.5)
    points(val[mejor2, i], val[mejor2, j], col="orange", pch=16, cex=1.5)
  }
}

graphics.off()