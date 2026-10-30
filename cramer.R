cramer<-function(sada1,sada2){
  m<-nrow(sada1)
  n<-nrow(sada2)
  X1<-sada1[,1]
  X2<-sada1[,2]
  Y1<-sada2[,1]
  Y2<-sada2[,2]
  norma<-0
  xnorma<-0
  ynorma<-0
  for (i in 1:m){
    for (j in 1:n){
      norma<-norma+sqrt((X1[i]-Y1[j])^2+(X2[i]-Y2[j])^2)
    }
  }
  for (i in 1:m){
    for (j in 1:m){
      xnorma<-xnorma+sqrt((X1[i]-X1[j])^2+(X2[i]-X2[j])^2)
    }
  }
  for (i in 1:n){
    for (j in 1:n){
      ynorma<-ynorma+sqrt((Y1[i]-Y1[j])^2+(Y2[i]-Y2[j])^2)
    }
  }
  c<-(m*n/(m+n) * (((2/(m*n)) *norma/2)-((1/m^2) *(xnorma/2))-((1/n^2) * (ynorma/2))))
  return(c)
}