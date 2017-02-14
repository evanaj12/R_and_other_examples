# Romberg Integration Method
# inputs:
#   a: first endpoint
#   b: last endpoint
#   n: + integer
#   f: function to integrate
romberg<-function(a, b, n, f){
  # initalize Rij matrix
  rmatout<-matrix(rep(0,n^2),nrow=n)
  
  # initlaize R(1-2)(1-2) matrix
  rmat<-matrix(rep(0,n*2),nrow=2)
  
  # create h
  h<-b-a
  
  # set r11
  rmat[1,1]<-h/2*(f(a)+f(b))
  
  # output r11
  rmatout[1,1]<-rmat[1,1]
  
  # set r2i's
  for(i in 2:n){
    
    # create sum
    sigma<-0
    for(k in 1:(2^(i-2))){
      
      # create x
      x<-a+(k-0.5)*h
      
      # update sigma
      sigma<-sigma+f(x)
    }
    
    # set r21
    rmat[2,1]<-1/2*(rmat[1,1]+h*sigma)
    
    # set r2j's
    for (j in 2:i){

      #update r2j's
      rmat[2,j]<-(rmatout[2,(j-1)]+(rmatout[2,(j-1)]-rmatout[1,(j-1)])/(4^(j-1)-1))
    }
    
    # output r2j's
    for (j in 1:i){
      rmatout[i,j]<-rmat[2,j]
    }
    
    # update h
    h<-h/2
    
    # update R1j
    for(j in 1:i){
      rmat[1,j]<-rmat[2,j]
    }
  }
return(rmatout)
}

# function to integrate
h<-function(x){
  return (sin(x))
}

romberg(0,pi,10,h)