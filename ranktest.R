ranktest<-function(sada1,sada2){
  X1<-sada1[,1]
  Y1<-sada1[,2]
  X2<-sada2[,1]
  Y2<-sada2[,2]
  m<-nrow(sada1)
  n<-nrow(sada2)
  N<-m+n
  
  X<-c(X1,X2)
  Y<-c(Y1,Y2)
  R1<-rank(X)
  R2<-rank(Y)
  Rn<-cbind(R1,R2)
  ci<-c(rep(1,m),rep(0,n))
  aN1<-R1-mean(R1)
  aN2<-R2-mean(R2)
  Tn1<-sum(ci*aN1)
  Tn2<-sum(ci*aN2)
  
  v11<-sum(aN1*aN1)/(N-1)
  v22<-sum(aN2*aN2)/(N-1)
  v12<-sum(aN1*aN2)/(N-1)
  
  Cn<-sum((ci-mean(ci))^2)
  V<-matrix(c(v11,v12,v12,v22),2,2)
  Tn<-c(Tn1,Tn2)
  L<-as.numeric(t(Tn)%*%solve(V)%*%Tn)/Cn
  
  return(pchisq(L,df=2,lower.tail=FALSE))
}
