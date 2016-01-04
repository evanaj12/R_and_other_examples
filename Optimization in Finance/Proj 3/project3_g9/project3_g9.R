#Project 3 - portfolio optimization with non-linear programming

#Group 9:
#Evan Johnston
#Jordan Tavarez
#Arjo Mozumder
#Orukeme Ukiri

require("quadprog")

# read in correlations matrix
covariances<-read.csv("C_mat.csv", header=F)
covariances<-as.matrix(covariances)

# labels
colnames(covariances)<-c("US Bonds", "Int’l Bonds", "US Large Growth", "US Large Value", "US Small Growth", "US Small Value", "Int’l Dev. Equity", "Int’l Emerg. Equity")
rownames(covariances)<-c("US Bonds", "Int’l Bonds", "US Large Growth", "US Large Value", "US Small Growth", "US Small Value", "Int’l Dev. Equity", "Int’l Emerg. Equity")

# create Black-Litterman model function
BL_eq<- function(P, q, Omega, tau, covMat){
    m<- solve(solve(tau*covMat) + t(P) %*% solve(Omega) %*% P) %*% (solve(tau*covMat) %*% means+ t(P) %*% solve(Omega) %*% q)
    m<-c(m)
    return (m)
}

# manually input given means of the 8 assets
means<-c(0.0008, 0.0067, 0.0641, 0.0408, 0.0743, 0.037, 0.048, 0.066)

################## 1 ################## 

# create vector of returns (minimum mean to maximum mean)
RVals<-seq(0.0008, 0.0743, 0.0001)

# initialize empty risk vector
StdDevs<-rep(0,length(RVals))

# initialize empty solution weights vector
Weights<-matrix(rep(0,length(RVals)*8), ncol=8)

# loops for return values
for (i in 1:length(RVals)){
  
  #"D"
  Dmat<-2*covariances
  
  #"d"
  dvec<-c(rep(0,8))
  
  #"A"
  Amat<-matrix(c(rep(c(1,-1),8)), ncol=2, byrow=T)
  Amat<-cbind(Amat,diag(8))
  Amat<-cbind(Amat,means)
  
  #"b"
  bvec<-c(1,-1,rep(0,8),RVals[i])
  
  S<-solve.QP(Dmat,dvec,Amat,bvec)
  StdDevs[i]<-sqrt(S$value)
  Weights[i,]<-S$solution
}

# convert to dataframe for visualization
df1<-data.frame(RVals, StdDevs, Weights)

# efficient frontier (dotted line at max return of 0.0743)
plot1<-ggplot(df1, aes(y=RVals))+
  geom_hline(yintercept=0.0743, linetype=2)+
  geom_line(aes(x=StdDevs), color="blue")+
  labs(x="Risk (Standard Deviation)", y="Returns", title="Portfolio Frontier")+
  scale_y_continuous(limits=c(0,0.08))

# plots the returns (x-axis) against the solution value (y-axis)
#  this shows the share of each stock for each return value
#  (dotted line at max return of 0.0743)
plot2<-ggplot(df1, aes(x=RVals))+
  geom_vline(xintercept=0.0743, linetype=2)+
  geom_line(aes(y=X1), color="black")+
  geom_line(aes(y=X2), color="blue")+
  geom_line(aes(y=X3), color="red")+
  geom_line(aes(y=X4), color="green")+
  geom_line(aes(y=X5), color="purple")+
  geom_line(aes(y=X6), color="orange")+
  geom_line(aes(y=X7), color="brown")+
  geom_line(aes(y=X8), color="yellow")+
  labs(x="Returns", y="Portfolio Share", title="Assets' Share of Portfolio ")+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(limits=c(0,0.08))
#Notice: the plot is bound from 0 to 1 on the y-axis which generates
# missing values for some of the assets. This is because some of the
# shares are small negative numbers which we would approximate as
# zero, e.g. for a return of 0.0364 you need "-4.336809e-19" shares in
# X(1), which can be interpreted as zero.

################## 2 ################## 

# P matrix
p<-matrix(c(0,0,0,0,0,1,0,0,
            0,0,0,0,0,0,-1,1,
            0,0,-1,0,1,0,0,0), nrow=3, byrow=T)

# Q vector
q<-c(0.061, 0.03, 0.002)

# Omega belief matrix
omega<-c(.000801,.009546,.000884)
omega<-diag(omega)

#BL-adjusted means
meansBL<-BL_eq(p, q, omega, 1, covariances)

# loops for return values
for (i in 1:length(RVals)){
  
  #"D"
  Dmat<-2*covariances
  
  #"d"
  dvec<-c(rep(0,8))
  
  #"A"
  Amat<-matrix(c(rep(c(1,-1),8)), ncol=2, byrow=T)
  Amat<-cbind(Amat,diag(8))
  Amat<-cbind(Amat,meansBL)
  
  #"b"
  bvec<-c(1,-1,rep(0,8),RVals[i])
  
  S<-solve.QP(Dmat,dvec,Amat,bvec)
  StdDevs[i]<-sqrt(S$value)
  Weights[i,]<-S$solution
}

# combine with previous dataframe
df1<-data.frame(df1, RVals, StdDevs, Weights)

# efficient frontier, blue is the original frontier, red is the adjusted
plot3<-ggplot(df1, aes(y=RVals))+
  geom_hline(yintercept=0.0743, linetype=2)+
  geom_line(aes(x=StdDevs), color="blue")+
  geom_line(aes(x=StdDevs.1), color="red")+
  labs(x="Risk (Standard Deviation)", y="Returns", title="Portfolio Frontier")+
  scale_y_continuous(limits=c(0,0.08))

# plots the returns (x-axis) against the solution value (y-axis)
#  this shows the share of each stock for each return value
plot4<-ggplot(df1, aes(x=RVals))+
  geom_vline(xintercept=0.0743, linetype=2)+
  geom_line(aes(y=X1.1), color="black")+
  geom_line(aes(y=X2.1), color="blue")+
  geom_line(aes(y=X3.1), color="red")+
  geom_line(aes(y=X4.1), color="green")+
  geom_line(aes(y=X5.1), color="purple")+
  geom_line(aes(y=X6.1), color="orange")+
  geom_line(aes(y=X7.1), color="brown")+
  geom_line(aes(y=X8.1), color="yellow")+
  labs(x="Returns", y="Portfolio Share", title="Assets' Share of Portfolio ")+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(limits=c(0,0.08))

################## 3 ################## 

#BL-adjusted means with tau=0.25
meansBL1<-BL_eq(p, q, omega, 0.25, covariances)

# loops for return values
for (i in 1:length(RVals)){
  
  #"D"
  Dmat<-2*covariances
  
  #"d"
  dvec<-c(rep(0,8))
  
  #"A"
  Amat<-matrix(c(rep(c(1,-1),8)), ncol=2, byrow=T)
  Amat<-cbind(Amat,diag(8))
  Amat<-cbind(Amat,meansBL1)
  
  #"b"
  bvec<-c(1,-1,rep(0,8),RVals[i])
  
  S<-solve.QP(Dmat,dvec,Amat,bvec)
  StdDevs[i]<-sqrt(S$value)
  Weights[i,]<-S$solution
}

# combine with previous dataframe
df1<-data.frame(df1, RVals, StdDevs, Weights)

# efficient frontier, blue is the original frontier,
#  red is the adjusted, and green is with tau=0.25
plot5<-ggplot(df1, aes(y=RVals))+
  geom_hline(yintercept=0.0743, linetype=2)+
  geom_line(aes(x=StdDevs), color="blue")+
  geom_line(aes(x=StdDevs.1), color="red")+
  geom_line(aes(x=StdDevs.2), color="dark green")+
  labs(x="Risk (Standard Deviation)", y="Returns", title="Portfolio Frontier")+
  scale_y_continuous(limits=c(0,0.08))

# plots the returns (x-axis) against the solution value (y-axis)
#  this shows the share of each stock for each return value
plot6<-ggplot(df1, aes(x=RVals))+
  geom_vline(xintercept=0.0743, linetype=2)+
  geom_line(aes(y=X1.2), color="black")+
  geom_line(aes(y=X2.2), color="blue")+
  geom_line(aes(y=X3.2), color="red")+
  geom_line(aes(y=X4.2), color="green")+
  geom_line(aes(y=X5.2), color="purple")+
  geom_line(aes(y=X6.2), color="orange")+
  geom_line(aes(y=X7.2), color="brown")+
  geom_line(aes(y=X8.2), color="yellow")+
  labs(x="Returns", y="Portfolio Share", title="Assets' Share of Portfolio ")+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(limits=c(0,0.08))