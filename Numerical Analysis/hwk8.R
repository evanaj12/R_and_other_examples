# Evan Johnston
# M348 - hwk 8 - 29 March 2016

# Composite Simpson's Rule
# inputs:
#   a: first endpoint: a
#   b: last endpoint: b
#   n: even + integer: n
#   f: function to integrate
cSimpson<-function(a, b, n, f){
  
  # create h
  h<- (b - a)/n
  
  # initialize approximations
  xI0<- f(a) + f(b)
  xI1<- 0
  xI2<- 0

  # loop over the 1 to n-1 iterations
  for (k in 1:(n-1)){
    
    # set first xk
    x<- a + k*h
    
    # if k is even
    if (k%%2==0){
      
      # update xi2
      xI2<- xI2 + f(x)
    }
    
    # if k is odd
    else{
      
      # update xi1
      xI1<- xI1 + f(x)
    }
  }
  # create final step approximation
  xI<- h*(xI0+2*xI2+4*xI1)/3
  
  return (xI)
}

# Composite Trapezoid Rule
# inputs:
#   a: first endpoint: a
#   b: last endpoint: b
#   n: even + integer: n
#   f: function to integrate
cTrapezoid<-function(a, b, n, f){
  
  # create h
  h<- (b - a)/n
  
  # initialize approximations
  xI0<- f(a)
  xI1<- 0
  xI2<- f(b)
  
  # loop over the 1 to n-1 iterations
  for (k in 1:(n-1)){
    
    # set first xk
    x<- a + k*h
    
    # update xi1
    xI1<- xI1 + f(x)
  }
  # create final step approximation
  xI<- h*(xI0+2*xI1+xI2)/2
  
  return (xI)
}

# create function to be integrated
g<-function(x){
  return(exp(1)^(2*x)*sin(3*x))
}

# create matrix for output
out<-matrix(rep(0,6),nrow=2)
colnames(out)<-c('n=10', 'n=100', 'n=1000')
rownames(out)<-c('cSimpson', 'cTrapezoid')

# fill matrix
for (i in (1:2)){
  for (j in (1:3)){
    n<-10^j
    if (i==1){
      out[i,j]<-cSimpson(0,2,n,g)
    }
    else{
      out[i,j]<-cTrapezoid(0,2,n,g)      
    }
  }
}

out

# Romberg Integration Method
# inputs:
#   a: first endpoint: a
#   b: last endpoint: b
#   n: + integer: n
#   f: function to integrate
romberg<-function(a, b, n, f){
  # initalize Rij matrix
  rmat<-matrix(rep(0,n^2),nrow=2)
  
  # create h
  h<- (b - a)
  
  # create R11
  rmat[1,1]<-h/2*(f(a)+f(b))
  print(rmat[1,1])
  
  # create error term
  e<-10^(-6)
  
  # initalize i
  i<-2
  
  # loop to create other Rij's
  while (1){
    i<-i+1
    
    xI1<-0
    # create the summed terms
    for (k in 1:(2^(i-2))){
      
      # set first xk
      x<- a + (k-0.5)*h
      
      # update xi1
      xI1<- xI1 + f(x)
    }
    
    # set R21
    rmat[2,1]<-0.5*(rmat[1,1]+h*xI1)
    
    # loop to create the R2j's
    for (j in 2:i){
      
      # update R2j
      rmat[2,j]<-rmat[2,(j-1)]+(rmat[2,(j-1)]-rmat[1,(j-1)])/(4^(j-1)-1)
    }
    print(rmat[2,])
    
    # check tolerance condition
    if (abs(rmat[1,1]-rmat[2,2])<e){
      print(i)
      rmat
      break
    }
    
    # update h
    h<-h/2
    
    # update the previous row in the Rij matrix
    for (j in 1:i){
      rmat[1,j]<-rmat[2,j]
    }
  }
  rmat
  return (rmat)
}

# function to integrate
h<-function(x){
  return (cos(x)^2)
}

