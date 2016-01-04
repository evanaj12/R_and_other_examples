# Homework 3
# Evan Johnston, eaj628

require ("lpSolve")

#################################
#Problem 1

#Part 1: manual tree solution, see homework

#Original LP:
 #maximize matrix
 c<-c(-1, 4)

 #constraints
 a<-matrix(c(-10,20,5,10,1,0), nrow=3, byrow=T)

 #direction
 dir<-c("<=","<=","<=")

 #constraints continued
 b<-c(22,49,5)

#LP0
s0<-lp("max",c,a,dir,b,compute.sens=T)
s0$solution
s0$objval

#LP1: add constraint of x(1)<=3

 #Only changed the 5 to a 3 as the rest of the constraints are unchanged
 b<-c(22,49,3)

#LP1
s1<-lp("max",c,a,dir,b,compute.sens=T)
s1$solution
s1$objval

#LP2: add constraint of x(1)>=4

 #constraints
 a<-matrix(c(-10,20,5,10,1,0,1,0), nrow=4, byrow=T)

 #direction
 dir<-c("<=","<=","<=",">=")

 #constraints continued
 b<-c(22,49,5,4)

#LP2
s2<-lp("max",c,a,dir,b,compute.sens=T)
s2$solution
s2$objval

#LP3: add constraint of x(2)<=2 to left branch (see homework)

 #constraints
 a<-matrix(c(-10,20,5,10,1,0,0,1), nrow=4, byrow=T)

 #direction
 dir<-c("<=","<=","<=","<=")

 #constraints continued
 b<-c(22,49,3,2)

#LP3
s3<-lp("max",c,a,dir,b,compute.sens=T)
s3$solution
s3$objval

#LP4: add constraint of x(2)>=2 to left branch (see homework)

 #constraints
 a<-matrix(c(-10,20,5,10,1,0,0,1), nrow=4, byrow=T)

 #direction
 dir<-c("<=","<=","<=",">=")

 #constraints continued
 b<-c(22,49,3,3)

#LP4
s4<-lp("max",c,a,dir,b,compute.sens=T)
s4$solution
s4$objval

#LP5: add constraint of x(2)<=2 to right branch (see homework)

 #constraints
 a<-matrix(c(-10,20,5,10,1,0,1,0,0,1), nrow=5, byrow=T)

 #direction
 dir<-c("<=","<=","<=",">=","<=")

 #constraints continued
 b<-c(22,49,5,4,2)

#LP5
s5<-lp("max",c,a,dir,b,compute.sens=T)
s5$solution
s5$objval

#LP6: add constraint of x(2)>=3 to right branch (see homework)

 #constraints
 a<-matrix(c(-10,20,5,10,1,0,1,0,0,1), nrow=5, byrow=T)

 #direction
 dir<-c("<=","<=","<=",">=",">=")

 #constraints continued
 b<-c(22,49,5,4,3)

#LP6
s6<-lp("max",c,a,dir,b,compute.sens=T)
s6$solution
s6$objval

#LP7: add constraint of x(1)<=1 to left branch (see homework)

 #constraints
 a<-matrix(c(-10,20,5,10,1,0,0,1), nrow=4, byrow=T)

 #direction
 dir<-c("<=","<=","<=","<=")

 #constraints continued
 b<-c(22,49,1,2)

#LP7
s7<-lp("max",c,a,dir,b,compute.sens=T)
s7$solution
s7$objval

#LP8: add constraint of x(1)>=2 to left branch (see homework)

 #constraints
 a<-matrix(c(-10,20,5,10,1,0,0,1,1,0), nrow=5, byrow=T)

 #direction
 dir<-c("<=","<=","<=","<=",">=")

 #constraints continued
 b<-c(22,49,3,2,2)

#LP8
s8<-lp("max",c,a,dir,b,compute.sens=T)
s8$solution
s8$objval

#Part 2: integer program
 #constraints
 a<-matrix(c(-10,20,5,10,1,0), nrow=3, byrow=T)

 #direction
 dir<-c("<=","<=","<=")

 #constraints continued
 b<-c(22,49,5)

ss<-lp("max",c,a,dir,b,compute.sens=T, int.vec=1:2)
ss$solution
ss$objval

#From the tree we see 2 feasible solutions satisfying all constraints, including the integer constraint.
# LP(5): x(1) = 4.0, x(2) = 2.0, z = 4.0, the first lower bound
# LP(8): x(1) = 2.0, x(2) = 2.0, z = 6.0, the second lower bound and optimal integer solution.

#Relaxing the integer constraint shows 7 feasible solutions:
# LP(0): x(1) = 3.8, x(2) = 3.0, z = 8.2, optimal linear solution
# LP(1): x(1) = 3.0, x(2) = 2.6, z = 7.4
# LP(2): x(1) = 4.0, x(2) = 2.9, z = 7.6
# LP(3): x(1) = 1.8, x(2) = 2.0, z = 6.2
# LP(5): x(1) = 4.0, x(2) = 2.0, z = 4.0, the first lower bound
# LP(7): x(1) = 1.0, x(2) = 1.6, z = 5.4
# LP(8): x(1) = 2.0, x(2) = 2.0, z = 6.0, optimal integer solution

#Part 3
#There are 8 branches and 7 feasible solutions with a difference of 8 - 7 = 1

#################################
#Problem 2

#maximization matrix
c<-c(3,2,1,2)

#constraints
a<-matrix(c(6,3,5,2,
            1,0,1,0,
            0,1,0,1,
            1,1,0,0,
            0,0,1,0,
            0,0,0,1), nrow=6, byrow=T)

#direction
dir<-c("<=","<=","<=",">=","<=","<=")

#constraints continued
b<-c(11,1,1,1,1,1)

#integer program
ss2<-lp("max",c,a,dir,b,compute.sens=T, int.vec=1:4)
ss2$solution
ss2$objval

#################################
#Problem 3 (detailed explaination in homework)

#minimization matrix
schedule_costs<-c(330, rep(360,4), 330, 300)

#constraints
constraints<-matrix(
  c(0,1,1,1,1,1,0,
    0,0,1,1,1,1,1,
    1,0,0,1,1,1,1,
    1,1,0,0,1,1,1,
    1,1,1,0,0,1,1,
    1,1,1,1,0,0,1,
    1,1,1,1,1,0,0),
  nrow=7, byrow=T)

#direction
dir<-c(rep(">=",7))

#constraints continued
requirements<-c(5, 13, 12, 10, 14, 8, 6)

#integer program
ss3<-lp("min",schedule_costs,constraints,dir,
        requirements,compute.sens=T, int.vec=1:7)
ss3$solution
ss3$objval