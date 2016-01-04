#Project 4.4 - Exotic Option Pricing

#Group 8:
#Evan Johnston
#Chinmay Baxi
#Roberto Toto

#4 - Reprice Exotic Option with Implied Volatility

# exotic call pricing function
exoticCallPricer<-function(p0,K,T,r,sigma,N){
  mu_int<-r/252
  sigma_int<-sigma*sqrt(dt)
  optionPayOffs<-vector()
  for(i in 1:N){
    returns<-rnorm(T,mu_int,sigma_int)
    prices<-p0*cumprod(returns+1)
    
    # logical value indicating if the price is above the strike or not
    value<-(max(prices[T]-K,0))>0
    
    # if the price is above, then the investor recieves $10 as garunteed by our exotic option. This is a less-risky option than a standard european call.
    if (value){
      optionPayOffs[i]<-10
    }
    # 0 otherwise
    else{
      optionPayOffs[i]<-0
    } 
  }
  optPrice<-mean(optionPayOffs)/(1+r)^maturity
  return(optPrice)
}

# inital stock price
p0<-104.04

# maturity date (June 17 - 153 days)
maturity<-153/252

# create period
dt<-1/252
T<-maturity/dt

# call option price
c<-10.35

# strike price
K<-105

# modified risk free rate
r<-0

# implied volatility from 4.3
sigma<-0.35

# number of simulations
N<-10000

# initalize prices vector
prices<-rep(0,100)

# fill price vector with prices based on different estimated prices
for (i in 1:100){
  prices[i]<-exoticCallPricer(p0,K,T,r,sigma,N)
}

# plots the prices with the mean in blue and best-fit in red
plot(prices, main="Exotic Call Option Price Estimates")
abline(h=mean(prices),col="blue")
lines(lowess(prices),col="red")
print(mean(prices))
