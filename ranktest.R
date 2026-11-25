ranktest<-function(sada1,sada2){
  m<-nrow(sada1)
  n<-nrow(sada2)
  X<-c(sada1[,1],sada2[,1])
  Y<-c(sada1[,2],sada2[,2])
  R<-rank(X)
  Q<-rank(Y)
  
  S1<-sum(R[1:m])
  S2<-sum(Q[1:m])
  ES<-m*(m+n+1)/2
  
  V<-c(S1-ES,S2-ES)
  DS<-((m*n)*(m+n+1))/12
  kor<-cor(R,Q,method='spearman')
  C<-DS*kor
  sigma<-matrix(c(DS,C,C,DS),2,2)
  
  T<-V%*%solve(sigma)%*%V
  return(as.numeric(1-pchisq(T,df=2)))
}
