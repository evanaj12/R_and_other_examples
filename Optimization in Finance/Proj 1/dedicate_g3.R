#Project 1 - dedicated portfolio

#Group 3:
#Evan Johnston
#Jenny Lai
#Soo Kim
#Dylan Kennedy

#Load LP library
require ("lpSolve")

#1

#DECISION VARIABLES:
#x(i) for all i in 1:10, representing the amount of money invested in
# each bond i

#OBJECTIVE:
#Chose x(i) to minimize the cost to the investor
# min [cost]
# cost = 102x(1) + 99x(2)
#         + 101x(3) + 98x(4)
#         + 98x(5) + 104x(6)
#         + 100x(7) + 101x(8)
#         + 102x(9) + 94x(10)

# Bond      1   2   3   4   5   6   7   8   9   10
# Price     102 99  101 98  98  104 100 101 102 94
# Coupon    5   3.5 5   3.5 4   9   6   8   9   7
# Maturity  1   2   2   3   4   5   5   6   7   8

# Year      1       2       3       4       5
# Liability 12,000  18,000  20,000  20,000  16,000 

#           6       7       8
#           15,000  12,000  10,000

#CONSTRAINTS:
#Minimize the cost subject to meeting the liability that must be 
# covered each year. Notice that the constraints show the face value
# + coupon for a bond that matures in that year with the sum of all
# remaining bonds must be greater than or equal to the necessary
# liability.

#105x(1) +  3.5x(2) +  5x(3) +  3.5x(4) +  4x(5) +
# 9x(6) +  6x(7) +  8x(8) +  9x(9) +  7x(10)  >= 12,000

#0x(1) +  103.5x(2) +  105x(3) +  3.5x(4) +  4x(5) +
# 9x(6) +  6x(7) +  8x(8) +  9x(9) +  7x(10) >= 18,000

#0x(1) +  0x(2) +  0x(3) +  103.5x(4) +  4x(5) +
# 9x(6) +  6x(7) +  8x(8) +  9x(9) +  7x(10) >= 20,000

#0x(1) +  0x(2) +  0x(3) +  0x(4) +  104x(5) +
# 9x(6) +  6x(7) +  8x(8) +  9x(9) +  7x(10) >= 20,000

#0x(1) +  0x(2) +  0x(3) +  0x(4) +  0x(5) +
# 109x(6) +  106x(7) +  8x(8) +  9x(9) +  7x(10) >= 16,000

#0x(1) +  0x(2) +  0x(3) +  0x(4) +  0x(5) +
# 0x(6) +  0x(7) +  108x(8) +  9x(9) +  7x(10) >= 15,000

#0x(1) +  0x(2) +  0x(3) +  0x(4) +  0x(5) +
# 0x(6) +  0x(7) +  0x(8) +  109x(9) +  7x(10) >= 12,000

#0x(1) +  0x(2) +  0x(3) +  0x(4) +  0x(5) +
# 0x(6) +  0x(7) +  0x(8) +  0x(9) +  107x(10) >= 10,000

#2

#build constraint matrix
constraints=matrix(0,8,10)

constraints[1,]<-c(105, 3.5, 5, 3.5, 4, 9, 6, 8, 9, 7)
constraints[2,]<-c(0, 103.5, 105, 3.5, 4, 9, 6, 8, 9, 7)
constraints[3,]<-c(0, 0, 0, 103.5, 4, 9, 6, 8, 9, 7)
constraints[4,]<-c(0, 0, 0, 0, 104, 9, 6, 8, 9, 7)
constraints[5,]<-c(0, 0, 0, 0, 0, 109, 106, 8, 9, 7)
constraints[6,]<-c(0, 0, 0, 0, 0, 0, 0, 108, 9, 7)
constraints[7,]<-c(0, 0, 0, 0, 0, 0, 0, 0, 109, 7)
constraints[8,]<-c(0, 0, 0, 0, 0, 0, 0, 0, 0, 107)

#price vector
price<-c(102, 99, 101, 98, 98, 104, 100, 101, 102, 94)

#liability vector
liability<-c(12000, 18000, 20000, 20000, 16000, 15000, 12000, 10000)

#direction vector
dir<-c(rep(">=",8))

#linear program object
s = lp ("min",price,constraints,dir,liability,compute.sens=T)

#solution
s$solution

#3
#dedicated portfolio creation function, assuming a face value of 100
# with inputs vectors of prices, coupons, and maturity periods for
# each bond, and a liability vector for each period
dedicate<- function(price, coup, mat, liab){

  #find number of bonds, n
  n<-length(price)
  
  #find number of periods, y
  y<-length(liab)
  
  #create direction matrix
  dir<-c(rep(">=",y))
  
  #create empty constraints matrix
  constraints=matrix(0,y,n)
  
  for (period in 1:y){#for each period y
    for (bond in 1:n){#for each bond n
      if (mat[bond]==period){#if the maturity of bond n is period y
        
        #set the row for the period to the remaining coupon vector
        constraints[period:period, bond:n]<-coup[bond:n]
        
        #set the given bond for that period equal to the coupon + 100
        # assuming face value of 100
        constraints[period,bond]<-coup[bond]+100
      }
    }
  }
  #computes optimization with linear program
  out <<- lp ("min",price,constraints,dir,liab,compute.sens=T)
  
  #returns global variable out
  return (out)
}

p<-c(102, 99, 101, 98, 98, 104, 100, 101, 102, 94)
c<-c(5, 3.5, 5, 3.5, 4, 9,  6, 8, 9, 7)
m<-c(1, 2, 2, 3, 4, 5, 5, 6, 7, 8)
l<-c(12000, 18000, 20000, 20000, 16000, 15000, 12000, 10000)

#test dedicate function
out3<-dedicate(p,c,m,l)
out3$solution

#4

#read in csv data from website
#retrieved 21 September 2015
bond_data = read.csv("bond_data.csv")

#create maturity vector
mat4<-bond_data$Maturity

#create coupon vector
coup4<-bond_data$Coupon

#create price vector
price4<-bond_data$Asked

#create liability vector
liab4<-c(6,9,9,10,10,6,6,9,9,10,10,8,8)*10^6

#compute optimal investment strategy
out4<-dedicate(price4,coup4,mat4,liab4)
out4$solution

#dataframe of sensitivities as presented in class
data.frame(out4$duals, out4$duals.from, out4$duals.to)

#The sensitivity analysis is split between bond ask prices and
# liabilities. The first 26 rows represent the shadow price of an
# increase in one additional bond and its effect on the total cost to
# cover the liabilities. For example, in row 1, for every increase in
# the purchase of bond 1, the total cost will increase by $0.98. The
# relevant range in which this is true is between 2,081,170 to
# infinite bonds. In rows 27 to 39, these rows represent the increase
# in cost to cover liabilities if liabilities corresponding to
# periods 1 through 13 increase by $1. For example, in row 27, an
# inrease in liability for period 1 of $1 would increase the cost to
# cover liabilities by $2.93 for relevant range between -$4,504,392
# to $48,691
