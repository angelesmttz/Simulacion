n <- 50
p <- data.frame(x = rnorm(n), y=rnorm(n), c=rnorm(n),m=rnorm(n))
xmax <- max(p$x)
xmin <- min(p$x)
p$x <- (p$x - xmin) / (xmax - xmin) # ahora son de 0 a 1
ymax <- max(p$y)
ymin <- min(p$y)
p$y <- (p$y - ymin) / (ymax - ymin) # las y tambien
p$m <- abs(p$m)
cmax <- max(p$c)
cmin <- min(p$c)
p$c <- 2 * (p$c - cmin) / (cmax - cmin) - 1 # cargas son entre -1 y 1
p$g <- round(5 * p$c) # coloreamos segun la carga a 11 niveles de -5 a 5
p$g<-as.factor(p$g)
paso <- floor(256 / 10)
niveles <- seq(0, 255, paso)
colores <- rgb(niveles, rep(0, 11), rev(niveles), max=255)





f<-c()
eps<-0.001

for (i in 1:n){
xi <- p[i,]$x
yi <- p[i,]$y
ci <- p[i,]$c
mi <- p[i,]$m
fx <- 0
fy <- 0
for (j in 1:n) {
  cj <- p[j,]$c
  dir <- (-1)^(1 + 1 * (ci * cj < 0))
  dx <- xi - p[j,]$x
  dy <- yi - p[j,]$y
  factor <- dir * abs(ci - cj) / (sqrt(dx^2 + dy^2) + eps)
  
  dx<- dx/mi
  dy<-dy/mi
  
  fx <- fx - dx * factor
  fy <- fy - dy * factor

  res<-c(fx,fy)
  f<-c(f,res)
}
}




delta <- 0.02 / max(abs(f)) # que nadie desplace una paso muy largo
d.x<-c()
for(i in 1:50){
 x<- max(min(p[i,]$x + delta * f[c(TRUE, FALSE)][i], 1), 0)
 
 d.x<-c(d.x,x)
}



library(ggplot2)

datos$i<-as.factor(datos$i)
ggplot(datos, aes(x=datos$mi, y=datos$dx))+
  stat_summary(fun.y = mean, geom = "point",
               shape = 18, size = 1, color = "blue")


