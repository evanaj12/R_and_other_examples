# Evan Johnston
# M348 - hwk 2 - feb. 2 2016

# bisection function
# incuts:
#   a: beginning coint of interval
#   b: ending coint of interval
#   tol: tolerance level of error
#   n: max number of iterations
#   func: function to iterate over
bisection<-function(a,b,tol,n,func){
  
  # initialize counter
  k<-1
  
  # create f(a)
  fa<-func(a)
  
  # while looc to iterate up to n times
  while (k<=n){
    
    # find c
    c<-(a+b)/2
    
    # find f(c)
    fc<-func(c)
    
    # if root found or error within tolerance then output and exit
    if ((fc==0)|((b-a)/2<tol)){
      
      # vector of outputs
      output<-c(k,c,fc)
      return (output)
    }
    
    # if fa and fc have the same sign,
    #   then set interval to c,b
    if (fa*fc>0){
      a<-c
      fa<-fc
      
    # if not
    #   then set interval to a,c
    }else{
      b<-c
    }
    
    # print current output
    output<-c(k,c,fc)
    print (output)
    
    # iterate counter
    k<-k+1
  }
  
  # output final result
  output<-c(k,c,fc)
  return (output)
}

# create input function to be iterated over
f<-function(x){
  return (exp(1)^x - x^2 +3*x -2)
}

# run the bisectional function with:
#   given function f on [0,1]
#   with error limit 1e-08 for 1000 iterations
bisection(0, 1, 1e-08, 1000, f)
