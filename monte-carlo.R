library(MASS)
montecarlo<-function(m,n,ro,d1,d2,pocet){
  tpoc<-0
  hpoc<-0
  for (i in 1:pocet){
    sada1<-mvrnorm(m,c(0,0),matrix(c(1 ,ro,ro,1),2,2))
    sada2<-mvrnorm(n,c(0,0)+c(d1,d2),matrix(c(1,ro,ro,1),2,2))
    tab<-tabesh(sada1,sada2)
    hot<-hotelling(sada1,sada2)
    if (tab<0.05){tpoc<-tpoc+1}
    if (hot<0.05){hpoc<-hpoc+1}
  }
  tzamietavost<-tpoc/pocet
  hzamietavost<-hpoc/pocet
  return(c(tzamietavost,hzamietavost))
}