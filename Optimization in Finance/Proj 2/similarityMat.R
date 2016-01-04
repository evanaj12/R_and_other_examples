#Project 2 - index tracking: similarity matrix

#Group 6:
#Evan Johnston
#Alyjan Daya
#Albert Mata
#Alexander Blasig

#Load LP library
require ("lpSolve")

similarityMat<- function(priceMat, sharesMat, unique_tickers,unique_dates){

  returns<-matrix(0,250,100)
  for (i in 1:ncol(returns)){
    for (x in 2:nrow(returns)){
      p1<-priceMat[x-1,i]
      p2<-priceMat[x,i]
      returns[x,i]<-(p2-p1)/p1
    }
  }
  correlation<-cor(returns,use="complete.obs",method=c("pearson"))
  rho<-correlation*correlation
  return (rho)
}
