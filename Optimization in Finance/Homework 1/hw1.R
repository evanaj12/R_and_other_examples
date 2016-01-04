# Homework 1
# Evan Johnston, eaj628

#################################
#Problem 1

#creates function that creates Lehmer matrix of a given dimenson
Lehmer <- function(m){
  
  #set size of Lehmer matrix
  mn<-m**2
  
  #create list to be converted to matrix
  one_row<-c()
  
  #loop to create empty matrix
    for (i in 1:mn){
      one_row[i]<-0
    }
  
  #create matrix of square dimenson m
  mat<-matrix(one_row, nrow=m, byrow=T)

  #put in Lehmer values
  for (i in 1:m){ #loops for m columns
    for (j in 1:m){ #loops for m rows
      if (j>=i){
        mat[i,j]<-i/j
      }else{
        mat[i,j]<-j/i
      }
    }
  }
  return (mat)
}

A<-Lehmer(20)
A

#################################
#Problem 2

#creates function testing the symmetric of a given input matrix
symmetric <- function(a){
  
  #transforms the input matrix
  b<-t(a)
  
  #returns if the matricies are equivalent (symmetric) or not
  return (isTRUE(all.equal(a,b)))
}

symmetric(A)

#################################
#Problem 3

#function that creates and tests the inverse of a matrix
#for simplicity, this function uses the package MASS function ginv
inverse <- function(a, m){
  #loads MASS package
  require(MASS)
  
  #inverts given matrix
  c<-ginv(a)
  
  #the identity matrix of dimension m
  d<-diag(m)
  
  return (isTRUE(all.equal(a*c, d)))
}

inverse(A,20)
#NOTE: R shows the C*A to be =/= to the identity (though it should be) due to rounding errors

#################################
#Problem 4

#creates matricies
A4<-matrix(c(3,-1,2,1,0,3,3,-2,-5), nrow=3, byrow=T)
B4<-matrix(c(3,-6,-3,7,-14,-7,-1,2,1), ncol=3, byrow=T)

#multiply
AB<-A4%*%B4
BA<-B4%*%A4

AB
BA
isTRUE(all.equal(AB,BA))

#################################
#Problem 5

#creates matrix "A" (A5 for coding purposes) from problem 5 (see homework)
A5<-matrix(c(1, 1, 1, 1, -0.45, 0.55, 0, 0, 0, 1, 0, 0, 0.14, 0.2, 0.2, 0.1), nrow=4, byrow=T)

#creates matrix "b"(b for coding purposes) from problem 5 (see homework)
b5<-matrix(c(250, 0, 62.5, 37.5), nrow=4)

#pre-multiplies the inverse of A and b to find solutions to variables (see homework)
x<-ginv(A5)%*%b5

M1<-x[1,1]
M2<-x[2,1]
Hi<-x[3,1]
Po<-x[4,1]

M1
M2
Hi
Po
