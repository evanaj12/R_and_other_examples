# Homework 5
# Evan Johnston, eaj628

#################################
#Problem 1

# create probability values for p
p<-seq(0,1,0.01)

# create probability function (see report)
prob<-function(p){
  return ( 2*p**6 - 6*p**5 + 6*p**4 - 2*p**3 + 3*p**2 - 3*p + 1)
}

# plot the values
plot(p, prob(p), type="l", xlab="Probability Values for P", ylab="Prob. of Series Ending Within 6 Games", main="Problem 1")
points(0.5, prob(0.5), pch=19)
abline(v=0.5, untf=F, par(lty=2))
abline(h=prob(0.5), untf=F, par(lty=2))

# note that if the average probability of winning is 0.5, then the probability of the series ending within 6 games is 0.21875, the minimum of the generated curve.
prob(0.5)

#########Class code###############
N=10000
vec<-c(0,N)
for (i in 1:N){
  
  #gen rv: generates 7 random values [0,1], uses logical operator, and converts to number
  wins<-(runif(7)>0.5)+0
  #sample(10,10) #generates 10 random numbers between 0 and 10
  
  #sim model
  count<-sum(wins)>4+0
  
  #keep track
  vec[i]<-count
}
print(mean(vec))

#alternatively
mean(rowSums((matrix(runif(70000), 10000, 7)>0.5)+0)>4)
# gives row sum of 70,000 rvs, which is 7 games of 10,000 each. The entire simulation in a single line.
# notice this is equivalent to:
mean(rbinom(10000,7,0.5)>4)

#Thus in all methods we see a probability of about 0.22 for the series ending before the 7th game

#################################
#Problem 2

# number of trials
N<-1000

# number of tickets sold, 30 to 60.
sold<-seq(30,60,1)

# initialize revenue matrix
all_revenues<-matrix(c(rep(0,(length(sold)*N+length(sold)))), ncol=N+1)
all_revenues[,1]<-sold

# initialize revenue means matrix
all_revenues_means<-matrix(c(rep(0,length(sold)*2)), ncol=2)
all_revenues_means[,1]<-sold

# function to calculate revenue from number of actual passengers
revenue<-function(showed){
  if (showed<=40){
    return(10*showed)
  }
  else {
    return(40*10-showed%%40*25)
  } 
}

# loops for each number of tickets sold
for (i in 1:length(sold)){
  # random distribution of N trials with 0.9 probability of sold[i] number of customers actually riding the bus.
  showed<-rbinom(N,sold[i],0.9)
  
  # initialize revenues vector for this sold[i]
  revenues<-c(rep(0,N))
  
  # keeps revenue values
  for (j in 1:length(showed)){
    rev<-revenue(showed[j])
    revenues[j]<-rev
  }
  
  # stores this iteration of sold[i] in revenue matricies
  all_revenues[i,2:(N+1)]<-revenues
  all_revenues_means[i,2]<-mean(revenues)
}

# shows the mean of all tested revenues
plot(all_revenues_means, type="l", xlab="Tickets Sold", ylab="Revenue", main="Problem 2", ylim=c(0,450), col="red")

# shows the actual revenues from each test of different sold values
matpoints(all_revenues[,1], all_revenues[,2:N], pch=1, col="black")

# shows the maximum revenue, on average
abline(v=43, untf=F, par(lty=2))
abline(h=max(all_revenues_means), untf=F, par(lty=2))
points(x=43, y=max(all_revenues_means), pch=15, col="blue")

# on average, the maximum revenue occurs at 43 tickets sold
max(all_revenues_means)

#################################
#Problem 3

# number of trials
N=10000

# initialize keep and switch vectors
# run 2 sims. Prob of win with staying (keep) and switching (swit)
keep<-rep(NA,N)
swit<-rep(NA,N)


for (i in 1:N){
  #gen rv's: the prize door and the first pick
  prize<-sample(33,1)
  pick1<-sample(33,1)
  
  #sim model:
  # this gives the difference between the entire set of choices and the set of choices the host cannot open. The host opens 5 doors.
  host<-sample(setdiff(c(1:33),union(prize,pick1)), 5)
  
  # this gives the difference between the first selection and those revealed by the host, selecting 1 of those not pick 1 or in host.
  pick2<-sample(setdiff(c(1:33),union(pick1,host)), 1)
  
  #keep track
  keep[i]<-(pick1==prize)
  swit[i]<-(pick2==prize)
}

print(mean(keep))
print(mean(swit))

# Thus it is still beneficial to switch doors, but the probability of success is much less with 33 doors compared to 3.

#alternatively - not used
#N=10000
#keep<-rep(NA,N)
#swit<-rep(NA,N)

#prize<-sample(33,N, replace=T)
#pick1<-sample(33,N, replace=T)

#print(mean(prize==pick1))
#print(mean(prize!=pick1))
