#Project 4.2 - Exotic Option Pricing

#Group 8:
#Evan Johnston
#Chinmay Baxi
#Roberto Toto

#1 - Choose Stock
# Stock: Facebook

# inital stock price
p0<-104.04

# maturity date (June 17 - 153 days)
maturity<-153/252

# create period
dt<-1/252
T<-maturity/dt

# call option price (from Yahoo Finance)
c<-10.35

# strike price
K<-105

# risk free rate
r<-0.004

# standard deviation of daily stock
sigma<-0.34

# number of simulations
N<-10000

#2 - Call Option Pricing Function
callPricer<-function(p0,K,T,r,sigma,N){
  # p0 is the initial price of the underlying stock
  # K is the strike
  # T is the number of business days to expiry
  # r is the risk-free rate per business day
  # sigma is the standard deviation of daily stock return
  # N is the number of simulations to use.
  
  mu_int<-r/252
  sigma_int<-sigma*sqrt(dt)
  
  # initialize payoff vector
  optionPayOffs<-vector()
  
  for(i in 1:N){
    returns<-rnorm(T,mu_int,sigma_int)
    prices<-p0*cumprod(returns+1)
    optionPayOffs[i]<-max(prices[T]-K,0)
  }
  
  optPrice<-mean(optionPayOffs)/(1+r)^maturity
  return(optPrice)
  
}

# initalize prices vector
prices<-rep(0,100)

# fill price vector with prices based on different estimated prices
for (i in 1:100){
  prices[i]<-callPricer(p0,K,T,r,sigma,N)
}

# plots the prices with the mean in blue and best-fit in red
plot(prices, main="Call Option Price Estimates")
abline(h=mean(prices),col="blue")
lines(lowess(prices),col="red")
print (mean(prices))
