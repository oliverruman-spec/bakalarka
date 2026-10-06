tabesh<-function(sada1,sada2){
  X1<-sada1[,1]
  Y1<-sada1[,2]
  X2<-sada2[,1]
  Y2<-sada2[,2]
  X1_mean=mean(X1)
  Y1_mean=mean(Y1)
  X2_mean=mean(X2)
  Y2_mean=mean(Y2)
  d1=X1_mean-X2_mean
  d2=Y1_mean-Y2_mean
  
  if (d1*d2 >0){
    S1=X1+Y1
    S2=X2+Y2
    return(wilcox.test(S1,S2))
  }
  else if(d1*d2<0){
    S1=X1-Y1
    S2=X2-Y2
    return(wilcox.test(S1,S2))
  }
  else if ((d1==0&d2!=0)){
    return(wilcox.test(Y1,Y2))
  }
  else if ((d2==0&d1!=0)){
    return(wilcox.test(X1,X2))
  }
}