hotelling<-function(sada1,sada2){
  n1<-nrow(sada1)
  n2<-nrow(sada2)
  s1<-cov(sada1)
  s2<-cov(sada2)
  SS1<-(n1-1)*s1
  SS2<-(n2-1)*s2
  

  Spl<-(SS1+SS2)/(n1+n2-2)
    
  y1_mean<-colMeans(sada1)
  y2_mean<-colMeans(sada2)
  
  T<-(n1*n2/(n1+n2))*((y1_mean-y2_mean)%*%solve(Spl)%*%(y1_mean-y2_mean))
  p<-ncol(sada1)
  F<-((n1+n2-p-1)/((n1+n2-2)*p) )*T
  pf<-pf(F,df1=p,df2=n1+n2-p-1)
  return(as.numeric(1-pf))
}