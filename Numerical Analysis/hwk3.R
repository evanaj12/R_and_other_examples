# Evan Johnston
# M348 - hwk 3 - feb. 11 2016

# fixed point iteration
# inputs:
#   pinit: initial approximation
#   tol: tolerance level of error
#   n: max number of iterations
#   gfunc: function to iterate over
fpi<-function(pinit,tol,n,gfunc){
  
  # as the output is longer I decided to format it neater in a matrix
  final<-matrix(rep(0,n*4), nrow=n)
  colnames(final)<-c('iteration', 'p=g(p0)', 'p0', 'difference')
  
  # initialize counter
  k<-1
  
  # preserve initial guess
  p0<-pinit
  
  # while loop to iterate up to n times
  while (k<=n){
    
    # find g(p) for current iteration
    p<-gfunc(p0)
    
    # if found or error within tolerance then output and exit
    if (abs(p-p0)<tol){
      
      # vector of outputs (iteration #, p=g(p0), p0, abs(p-p0) )
      output<-c(k,p,p0,abs(p-p0))
      final[k,]<-output
      return (final[1:k,])
    }
    
    # record current output
    output<-c(k,p,p0,abs(p-p0))
    final[k,]<-output
    
    # update p0
    p0<-p
    
    # iterate counter
    k<-k+1
  }
  
  # record last result
  output<-c(k,p,p0,abs(p-p0))
  final[k,]<-output
  return (final)
}

# create input function to be iterated over
# note: x=tan(x) -> x=arctan(x)=g(x)
g<-function(x){
  return (atan(x))
}

# run the fixed point iteration function with:
#   rearranged function g=arctan(x)
#   inital guess is 3/2*pi=4.7123 which is on [4,5]
#   with error limit 10e-05 for 1000 iterations
result<-fpi(3/2*pi,10e-05,1000,g)

print(result[1:10,])
print(result[327:337,])
