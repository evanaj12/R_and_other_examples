# Homework 4
# Evan Johnston, eaj628

#################################
#Problem 1

#Create production function
productionL<- function(L){
  return (-1*(0.05*L^(2/3))*(100000/15-4/5*L)^(1/3))
}
productionK<- function(K){
  return (-1*(0.05*(100000/12-5/4*K)^(2/3))*K^(1/3))
}
s1<-optim(1, productionL, method="CG")
s2<-optim(1, productionK, method="CG")

#################################
#Problem 2

#Read in data
bond_data <- read.csv("homework4stocks.csv")

#Trim data to only be returns
bond_data<-bond_data[1:77,2:28]

#Column (company) means of returns
means<-colMeans(bond_data)

#Variances of each companys' returns
variances<-c(rep(0,length(bond_data)))
for (i in (1:length(bond_data))){
  variance<-var(bond_data[1:77,i])
  variances[i]<-variance
}

#Correlations of all returns
correlations<-cor(bond_data)

#Covariances of all returns
covariances<-cov(bond_data)

#Notice that this is equivalent to the above matrix
covars<-diag(sqrt(variances)) %*% 
  correlations %*% 
  diag(sqrt(variances))

#Create input parameters
dvec<-c(rep(0,27))
Dvec<-2*covars
Avec<-matrix(c(rep(c(1,-1),27)), ncol=2, byrow=T)
Avec<-cbind(Avec,means)
Avec<-cbind(Avec,diag(27))
bvec<-c(1,-1,0.01)
bvec<-c(bvec,rep(0,27))

#Solve quadratic program to create portfolio weights
require("quadprog")
s2<-solve.QP(Dvec,dvec,Avec,bvec)

#Fractions of wealth per stock
solution<-s2$solution

#Estimated mean
estMeans<-solution%*%means

#Estimated variance
estVar<-solution%*%variances

#Estimated standard deviation
estStdDev<-sqrt(estVar)

#################################
#Problem 3

#Read in data
nfl_data <- read.csv("nflratings.csv", header=F, col.names=c("Week", "HT.Idx", "VT.Idx", "HTS", "VTS"))

require("dplyr")

#Actual Point Spread
ActPSpd<-c(rep(0,dim(nfl_data)[1]))
for (i in (1:dim(nfl_data)[1])){
  ActPSpd[i]<-nfl_data[i,4]-nfl_data[i,5]
}

#Add to nfl_data
nfl_data<-cbind(nfl_data,ActPSpd)

#Sum of Squared Errors function
SSE<-function(vec){
  squared_errors<-c()
  homeR<-vec[1]
  visitorR<-vec[2]
  htadv<-vec[3]
   
  for (j in (1:dim(team_data)[1])){
    error<-team_data$ActPSpd[j]-(homeR-visitorR+htadv)
    squared_errors<-cbind(squared_errors, error^2)
  }
  return (sum(squared_errors))
}

#Optimization
ratings<-c()
for (i in (1:32)){
  team_data<- nfl_data %>% filter(HT.Idx==i) %>% select(ActPSpd)
  S<-optim(c(1,1,1), SSE, method="CG")
  ratings<-cbind(ratings,S$par)
}

#Normalize the ratings
ratings_scaled<-scale(ratings[1,], center=T, scale=F)
ratings_scaled<-ratings_scaled+85

