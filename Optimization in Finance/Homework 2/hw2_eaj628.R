# Homework 2
# Evan Johnston, eaj628

#################################
#Problem 1 - see homework

#################################
#Problem 2

#load package
require ("lpSolve")

#part B: create matricies corresponding to system of equations (homework)
c<-c(2000,3000)
A<-matrix(c(3,2,1,2,4,1), nrow=3)
b<-c(1000,1200,450)
dir<-c("<=", "<=","<=")

#solve the maximization problem
s<-lp("max",c,A,dir,b,compute.sens=T)

#solution set
s$solution

#solution profit
s$objval

#part C: create dataframe of shadow prices
data.frame(s$duals, s$duals.from, s$duals.to)
#Row 2 is the shadow price and range corresponding to fertilizer.

#function to find changes for each ton increase by a factor f
fertilizer_increase <- function(c,A,dir,f){

    #loops for the variable range 200 to 2200 tons by f
    for (i in seq(200, 2200, f)){
      
      #sets the new "b" matrix
      b1<-c(1000,i,450)
      
      #recalculates the optimal solution
      s1<-lp("max",c,A,dir,b1,compute.sens=T)
      
      #prints:
      # the limit of fertilizer for the iteration
      # the new profit and solution set where row 1 is wheat and 2 is corn
      print(i)
      print(data.frame(s1$objval, s1$solution))
    }
  }

#factor of 100
fertilizer_increase(c,A,dir,100)
#From the print out we see that the farmer:
# stops wheat production if the there are at least 1800 tons of fertilizer
# stops corn production if there are 600 or less tons of fertilizer

#factor of 10
fertilizer_increase(c,A,dir,10)
#From the print out we can see that profit increases:
# by 10000 for every 10 tons from 200 - 660 (or 666.67)
# by 8750 from 660 - 670
# by 6250 from 670 - 1600
# by 5000 from 1600 - 1800 where profits stabilize at $1,350,000
#At this point the fertilizer constrain is no longer binding so
# increasing it does not increase profit as all 450 acres are being
# used for corn production

#################################
#Problem 3

c3<-matrix(c(13,13,0,0,0,0,0,0,0,0,
            0,0,16,16,0,0,0,0,0,0,
            0,0,0,0,16,16,0,0,0,0,
            0,0,0,0,0,0,14,14,0,0,
            0,0,0,0,0,0,0,0,39,39), nrow=5, byrow=T)
A3<-matrix(c(11,0,53,0,5,0,5,0,29,0,
            0,3,0,6,0,5,0,1,0,34), nrow=2, byrow=T)
b3<-c(40,20)
dir3<-c("<=", "<=")
s3<-lp("max",c3,A3,dir3,b3,compute.sens=T)
#this does not comupte the solution as non-integer methods are needed
#see homework for explanation
