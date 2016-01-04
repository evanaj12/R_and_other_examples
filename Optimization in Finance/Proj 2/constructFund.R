#Project 2 - index tracking: construct fund

#Group 6:
#Evan Johnston
#Alyjan Daya
#Albert Mata
#Alexander Blasig

#Load LP library
require ("lpSolve")

#Load formatted data
source('readData.R')

#######################################
#1: Calculate returns matrix for the given 100 stocks
returns<-matrix(0,250,100)
for (i in 1:ncol(returns)){
  for (x in 2:nrow(returns)){
    p1<-priceMat[x-1,i]
    p2<-priceMat[x,i]
    returns[x,i]<-(p2-p1)/p1
  }
}

#######################################
#2: Create and fill correlation matrix
correlations<-cor(returns,use="complete.obs",method=c("pearson"))

#######################################
#3 Creates the constructFund function which will give the weights. Notice
# an extra parameter n has been included so that the function can potentially
# be used to construct other funds.
constructFund <- function(rho, q, priceMat, sharesMat, unique_tickers, unique_dates){
  n=100
  
  #101
  n2<-n+1
  
  #10000
  n4<-n*n
  
  #10100
  n5<-n*n+n
  
  #10101
  n6<-n*n+n+1
  
  #10001
  n7<-n*n+1
  
  #Initialize size of objective function vector
  maximize<-(c(rep(0,(n5))))
  
  #Counting variables to spread similarity matrix (rho)
  # across by row, then leave the binary yj variables =0
  count1<-1
  count2<-1
  for (i in (1:n4)){
    maximize[i]<-rho[count1,count2]
    count2=count2+1
    if (i%%n==0){
      count1=count1+1
    }
    if(count2>n){
      count2=1
    }
  }
  
  #Initialize size of first constraints matrix
  constraints1<-matrix(0,n6,n5)
  
  #Set rows 1:n cols 1:n*n to 1 for each consecutive row (xij)
  # corresponding to the first constraint
  count3<-1
  for (i in 1:n4){
    constraints1[count3,i]<-1
    if(i%%100==0){
      count3<-count3+1
    }
  }
  
  #Set rows n+1:n*n cols 1:n*n to a large identity matrix
  # corresponding to the second constraint (xij)
  constraints1[n2:n5,1:n4]<-diag(n4)
  
  #Set rows n+1:n*n cols n*n+1:n*n+n+1 to negative identity matricies
  # of size n corresponding to the second constraint (yj)
  for (i in seq(n2,n5,n)){
    rows<-i+n-1
    cols<-((i-1)/n)
    constraints1[i:rows, n7:n5]<--1*diag(n)
  }
  
  #Sets final row of the first constraint matrix to 1 for
  # cols n*n+1:n*n+n+1 (yj) to one for final constraint
  constraints1[n6,n7:n5]<-1
  
  #Sets second constraint vector to values 1, 0, and q
  # for the first n constraints, the second n*n constraints, 
  # and the third 1 constraint
  constraints2<-c(rep(0,n6))
  constraints2[1:n]<-1
  constraints2[n2:n5]<-0
  constraints2[n6]<-q
  
  #Sets direction vector for:
  # for the first n constraints (=), the second n*n constraints (<=), 
  # and the third 1 constraint (=)
  dir<-c(rep("=",n), rep("<=", n4), "=")
  
  #Creates integer program object
  out<-lp("max",maximize,constraints1,dir,constraints2, all.bin=T)
  
  #These are the selected stocks y(j), the last n of the solutions vector
  selected<-out$solution[n7:n5]
  
  #Peels the final number of shares for each of the 100 stocks from the given matrix
  shares<-sharesMat[250,]
  
  #Similarly, peels the prices of said stocks
  prices<-priceMat[250,]
  
  #Finds the market capitalization of each of the 100 stocks based on the final
  # prices of 2012
  markCap<-shares*prices
  
  #Finds the market capitalization for the selected 25 stocks
  markCapSelect<-markCap*selected
  
  #Sums the market capitalizations for the 25 stocks over the year (all other
  # elements in the matrix are 0 at this point)
  markCapSelectTotal<-sum(markCapSelect)
  
  #Removes zeros, giving the weight of each of the 25 stocks
  weights<-markCapSelect[markCapSelect!=0]
  
  #Initializes and creates the normalized version of the weights vector
  weightsNorm<-c(rep(0,length(weights)))
  for (i in (1:length(weights))){
    weightsNorm[i]<-(weights[i]/markCapSelectTotal)
  }
  
  #Returns the weights vector for the 25 chosen stocks
  return (weightsNorm)  
}

#This is identical to constructFund but outputs the vector
# of selected stocks rather than the weights vector for later calcuations
constructFund2 <- function(rho, q, priceMat, sharesMat, unique_tickers, unique_dates){
  n=100
  n2<-n+1
  n4<-n*n
  n5<-n*n+n
  n6<-n*n+n+1
  n7<-n*n+1
  maximize<-(c(rep(0,(n5))))
  count1<-1
  count2<-1
  for (i in (1:n4)){
    maximize[i]<-rho[count1,count2]
    count2=count2+1
    if (i%%n==0){
      count1=count1+1
    }
    if(count2>n){
      count2=1
    }
  }
  constraints1<-matrix(0,n6,n5)
  count3<-1
  for (i in 1:n4){
    constraints1[count3,i]<-1
    if(i%%100==0){
      count3<-count3+1
    }
  }
  constraints1[n2:n5,1:n4]<-diag(n4)
  for (i in seq(n2,n5,n)){
    rows<-i+n-1
    cols<-((i-1)/n)
    constraints1[i:rows, n7:n5]<--1*diag(n)
  }
  constraints1[n6,n7:n5]<-1
  constraints2<-c(rep(0,n6))
  constraints2[1:n]<-1
  constraints2[n2:n5]<-0
  constraints2[n6]<-q
  dir<-c(rep("=",n), rep("<=", n4), "=")
  out<-lp("max",maximize,constraints1,dir,constraints2, all.bin=T)
  selected<-out$solution[n7:n5]
  return (selected)
}

#The output weights vector
weights<-constructFund(correlations, 25, priceMat, sharesMat, unique_tickers, unique_dates)

#######################################
#4 Constructs an index portfolio for the end of 2012

#The vector of 25 selected stocks
selected<-constructFund2(correlations, 25, priceMat, sharesMat, unique_tickers, unique_dates)

#Multiplies the weights by $1 million to create a vector of the 
# actual amount invested per stock in the 25
amt_inv<-weights*10^6

#Creates vector of the prices at the end of 2012 for the selected stocks
prices<-(selected*priceMat[250,])

#Remove zeros from said vector
selected_prices<-prices[prices!=0]

#Creates vector of the number of shares purchased in each of the 25 stocks
# using the prices found above
selected_shares<-(amt_inv/selected_prices)

#Creates matrix for the prices of the 25 stocks in 2013 by first initalizing
# an empty 12 x 25 matrix then filled with the prices of the 25 selected and
# collapsing the matrix to one of size 12 x 25
prices2013<-matrix(c(rep(0,12*25)), nrow=12)
for (i in (1:12)){
  vec<-(monthlyPriceMat[i,]*selected)
  prices2013[i,]<-vec[vec!=0]
}

#Calculates returns matrix for the first fund (correlation-based)
returns_correlation<-matrix(0,12,25)
for (i in 1:ncol(returns_correlation)){
  for (x in 2:nrow(returns_correlation)){
    p1<-prices2013[x-1,i]
    p2<-prices2013[x,i]
    returns_correlation[x,i]<-(p2-p1)/p1
  }
}

#Finds the mean of each row of the first fund as a portfolio return
portfolio_return1<-rowMeans(returns_correlation)

#Calculate returns for NASDAQ 100 index with the 25 selected stocks from the
# given information in the problem.
prices_N<-c(2731.53, 2738.58, 2818.69, 2887.44, 2981.76, 2909.60, 3090.19, 3073.81, 3218.20, 3377.73, 3487.82, 3592.00)
returns_N<-c(rep(0,12))
for (i in (2:length(prices_N))){
  returns_N[i]<-(prices_N[i]-prices_N[i-1])/prices_N[i-1]
}

#Visualizations
library("ggplot2")

month<-seq(1,12)
fund1<-data.frame(returns_N, portfolio_return1, month)
plot1<-ggplot(fund1, aes(x=month))+
  geom_point(aes(y=returns_N), size=3, color="blue")+
  geom_line(aes(y=returns_N), size=1, color="blue", linetype=2)+
  geom_point(aes(y=portfolio_return1), size=3, color="red")+
  geom_line(aes(y=portfolio_return1), size=1, color="red")+
  scale_x_discrete(name="Month")+
  scale_y_continuous(name="Returns", breaks=seq(-0.05,0.15,0.01))
#Blue line: NASDAQ returns with the portfolio
#Red line: Correlation portfolio returns
#plot1
#Dataframe with our returns of NASDAQ vs those of the correlation fund.
#fund1

#####################################
#5 Repeat step 4 with a new similarity matrix

source('similarityMat.R')
rho<-similarityMat(priceMat, sharesMat, unique_tickers, unique_dates)

weights_new<-constructFund(rho, 25, priceMat, sharesMat, unique_tickers, unique_dates)
selected_new<-constructFund2(rho, 25, priceMat, sharesMat, unique_tickers, unique_dates)
amt_inv_new<-weights_new*10^6
prices_new<-(selected_new*priceMat[250,])
selected_prices_new<-prices_new[prices_new!=0]
selected_shares_new<-(amt_inv/selected_prices_new)
prices2013_new<-matrix(c(rep(0,12*25)), nrow=12)
for (i in (1:12)){
  vec<-(monthlyPriceMat[i,]*selected_new)
  prices2013_new[i,]<-vec[vec!=0]
}

#Re-Calculate Returns for new index
returns_rho<-matrix(0,12,25)
for (i in 1:ncol(returns_rho)){
  for (x in 2:nrow(returns_rho)){
    p1<-prices2013_new[x-1,i]
    p2<-prices2013_new[x,i]
    returns_rho[x,i]<-(p2-p1)/p1
  }
}
portfolio_return2<-rowMeans(returns_rho)

#Visualizations
fund2<-data.frame(returns_N, portfolio_return1, portfolio_return2, month)
plot2<-ggplot(fund2, aes(x=month))+
  geom_point(aes(y=returns_N), size=3, color="blue")+
  geom_line(aes(y=returns_N), size=1, color="blue", linetype=2)+
  geom_point(aes(y=portfolio_return1), size=2, color="red")+
  geom_line(aes(y=portfolio_return1), size=2, color="red", linetype=3)+
  geom_point(aes(y=portfolio_return2), size=3, color="dark green")+
  geom_line(aes(y=portfolio_return2), size=1, color="dark green")+
  scale_x_discrete(name="Month")+
  scale_y_continuous(name="Returns", breaks=seq(-0.05,0.15,0.01))
#Blue line: NASDAQ returns with the portfolio
#Red line: Correlation portfolio returns
#Green line: R-squared portfolio returns
#plot2
#Dataframe with NASDAQ, correlations matrix, and r-squared matrix returns
#fund2
