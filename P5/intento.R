x<-3.1415952
y<-3.14
d<-numeric()
px<-matrix(substring(x, seq(nchar(x)), seq(nchar(x))))
py<-as.matrix(substring(y, seq(nchar(y)), seq(nchar(y))))
w<-length(py)

for (new in 1:(length(px)-length(py))){
  py<-rbind(py,0)
}

for (a in 1:7){
  if(py[a]==px[a]){d<-c(d,TRUE)}else
    break;
  }


3.1415926535