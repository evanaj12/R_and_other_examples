# Evan Johnston
# M348 - hwk 9 - 5 April 2016

# Euler's Method
# inputs:
#   a: first endpoint
#   b: last endpoint
#   n: integer
#   e: initial condition
#   f: ODE given function
euler<-function(a, b, n, e, f){
  emat<-matrix(rep(0,n*2+2),nrow=2)
  
  # create h, t, w
  h<- (b - a)/n
  t<- a
  w<- e
  
  # first approx
  emat[1,1]<-t
  emat[2,1]<-w
  
  # loop over the 1 to n iterations
  for (i in 1:n){
    
    # set next w and t
    w<- w + h*f(t,w)
    t<- a + i*h
    
    # save approxs
    emat[1,i+1]<- t
    emat[2,i+1]<- w
    
  }
  # output 
  return(emat)
}

# intial ODE function
f<-function(t,y){
  return(1+y/t)
}

# true function
f.tru<-function(t){
  return(t*log(t)+2*t)
}

# matrix of approximations with n=10 => h=0.1
ans.6b<-euler(1,2,10,2,f)
ans.6b

# matrix of approximations with n=1000 => h=0.001
ans.6c<-euler(1,2,1000,2,f)
ans.6c[,1:5]
ans.6c[,996:1001]

# vector of true solutions over [1,2] by 0.0001
true.6<-c(f.tru(seq(1,2,0.0001)))

# plot coordinate plane over relevant interval
plot(NA, xlim=c(1,2), ylim=c(0,6), xlab="X", ylab="Y")

# true solution (black)
lines(seq(1,2,0.0001), true.6, col="black")

# approximation (b) (blue)
lines(ans.6b[1,], ans.6b[2,], col="blue")

# approximation (c) (green)
lines(ans.6c[1,], ans.6c[2,], col="green")
