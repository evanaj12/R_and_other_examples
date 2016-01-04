#Project 4.3 - Exotic Option Pricing

#Group 8:
#Evan Johnston
#Chinmay Baxi
#Roberto Toto

#3 - Estimate Volitility

callPricer<-function(p0,K,T,r,sigma,N){
  mu_int<-r/252
  sigma_int<-sigma*sqrt(dt)
  optionPayOffs<-vector()
  for(i in 1:N){
    returns<-rnorm(T,mu_int,sigma_int)
    prices<-p0*cumprod(returns+1)
    optionPayOffs[i]<-max(prices[T]-K,0)
  }
  optPrice<-mean(optionPayOffs)/(1+r)^maturity
  return(optPrice)
}

p0<-104.04
maturity<-153/252
dt<-1/252
T<-maturity/dt
K<-105
N<-10000

# modified risk free rate
r<-0

# vector of potential variations
sigma<-seq(0.05,0.6,0.01)

# initalize prices vector
prices<-rep(0,length(sigma))

# fill price vector with prices based on different variations
for (i in 1:length(sigma)){
  prices[i]<-callPricer(p0,K,T,r,sigma[i],N)
}

# create data frame of variations and respective prices
df1<-data.frame(sigma, prices)

# iterates through the dataframe and only shows prices/volatilities in the $10 range, that is those close to our price of $10.35.
targets<-vector()
for (i in 1:dim(df1)[1]){
  if ((df1$price[i]>10) & (df1$price[i]<11)){
    print(df1[i,])
    targets<-append(targets,i,after=length(targets))
  }
}

# plots this relationship with the closest values in blue
plot(sigma, prices, main="Volatility vs. Price")
for (i in 1:length(targets)){
  points(x=df1$sigma[targets[i]], y=df1$prices[targets[i]], pch=16, col="blue")
}
