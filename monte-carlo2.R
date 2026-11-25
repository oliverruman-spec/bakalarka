library(MASS)
montecarlo<-function(m,n,pocet){
  d1<-c(0,0.15,0.15,0.25,0.30,0.40,0.60,0.80,1.10,1.60, -0.15,0.15,-0.25,0.30,-0.40,0.60,-0.80,1.10,-1.60)
  d2<-c(0,0.15,0.25,0.25,0.40,0.50,0.70,0.90,1.30,1.80, 0.15,-0.25,0.25,-0.40,0.50,-0.70,0.90,-1.30,1.80)
  delty<-data.frame(d1,d2)
  vysledok<-data.frame()
  ro<-c(0,0.5,-0.5)
  g<-c(0.25,0.5)

  for (k in ro){
    for (j in 1:nrow(delty)){
      tpoc<-0
      hpoc<-0
      cpoc<-0
      rpoc<-0
      for (i in 1:pocet){
        delta1<-delty$d1[j]
        delta2<-delty$d2[j]
        sada1<-mvrnorm(m,c(0,0),matrix(c(1 ,k,k,1),2,2))
        sada2<-mvrnorm(n,c(0,0)+c(delta1,delta2),matrix(c(1,k,k,1),2,2))
        tab<-tabesh(sada1,sada2)
        hot<-hotelling(sada1,sada2)
        cram<-cramer(sada1,sada2)
        rnk<-ranktest(sada1,sada2)
        if (tab<0.05){tpoc<-tpoc+1}
        if (hot<0.05){hpoc<-hpoc+1}
        if (cram<0.05){cpoc<-cpoc+1}
        if (rnk<0.05){rpoc<-rpoc+1}
      }
      riadok<-data.frame(delta1=delta1,delta2=delta2,P=tpoc/pocet,T2=hpoc/pocet,R=rpoc/pocet,C=cpoc/pocet)
      vysledok<-rbind(vysledok,riadok)
    }
  }
  
  gvysledok<-data.frame()
  for (l in g){
    for (j in 1:nrow(delty)){
      tpoc<-0
      hpoc<-0
      cpoc<-0
      rpoc<-0
      for (i in 1:pocet){
        delta1<-delty$d1[j]
        delta2<-delty$d2[j]
        Z1<-mvrnorm(m,c(0,0),matrix(c(1 ,0,0,1),2,2))
        Z2<-mvrnorm(n,c(0,0)+c(delta1,delta2),matrix(c(1,0,0,1),2,2))
        sada1<-((exp(l*Z1)-1)/l)*exp((0.2*Z1^2)/2)
        sada2<-((exp(l*Z2)-1)/l)*exp((0.2*Z2^2)/2)
        tab<-tabesh(sada1,sada2)
        hot<-hotelling(sada1,sada2)
        cram<-cramer(sada1,sada2)
        rnk<-ranktest(sada1,sada2)
        if (tab<0.05){tpoc<-tpoc+1}
        if (hot<0.05){hpoc<-hpoc+1}
        if (cram<0.05){cpoc<-cpoc+1}
        if (rnk<0.05){rpoc<-rpoc+1}
      }
      riadok<-data.frame(delta1=delta1,delta2=delta2,P=tpoc/pocet,T2=hpoc/pocet,R=rpoc/pocet,C=cpoc/pocet)
      gvysledok<-rbind(gvysledok, riadok)
    }
  }

  hvysledok<-data.frame()
  for (j in 1:nrow(delty)){
    tpoc<-0
    hpoc<-0
    cpoc<-0
    rpoc<-0
    for (i in 1:pocet){
      delta1<-delty$d1[j]
      delta2<-delty$d2[j]
      Z1<-mvrnorm(m,c(0,0),matrix(c(1 ,0,0,1),2,2))
      Z2<-mvrnorm(n,c(0,0)+c(delta1,delta2),matrix(c(1,0,0,1),2,2))
      sada1<-Z1*exp((0.2*Z1^2)/2)
      sada2<-Z2*exp((0.2*Z2^2)/2)
      tab<-tabesh(sada1,sada2)
      hot<-hotelling(sada1,sada2)
      cram<-cramer(sada1,sada2)
      rnk<-ranktest(sada1,sada2)
      if (tab<0.05){tpoc<-tpoc+1}
      if (hot<0.05){hpoc<-hpoc+1}
      if (cram<0.05){cpoc<-cpoc+1}
      if (rnk<0.05){rpoc<-rpoc+1}
    }
    riadok<-data.frame(delta1=delta1,delta2=delta2,P=tpoc/pocet,T2=hpoc/pocet,R=rpoc/pocet,C=cpoc/pocet)
    hvysledok<-rbind(hvysledok, riadok)
  }

  return(list(vysledok,gvysledok,hvysledok))
}