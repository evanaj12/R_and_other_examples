#Project 5 - Dynamic Programming Game

#Group:
# Evan Johnston
# Albert Mata
# Jordan Tavarez
# Jenny Lai

# Intelligent opponent function
gameStrategy <- function(goal){
  # probabilities
  p1<-1/6
  p2<-1/6
  p3<-1/6
  p4<-1/6
  p5<-1/6
  p6<-1/6
  
  # the ending score of the game
  sN<-goal
  
  # highest possible score (6-sided die)
  max_score<-goal+5
  
  # intialize the value matrix V and decisions matrix U
  V<-array(NA,c(max_score+(1),max_score+(1),max_score+(1)))
  U<-array(NA,c(max_score+(1),max_score+(1),max_score+(1)))
  
  # iterates over all possible values of the sum of i and j (0 to 210)
  for (sumIJ in (max_score*2):0){
    
    # if the current sum is greater than the max (105), 
    #  the max for player i is the max score and the min
    #  score must make up the difference
    if (sumIJ>max_score){
      maxI<-max_score
      minI<-sumIJ-max_score
    }
    # if not (the current sum is <= the max (105))
    #  then the max for player i is the sum the min is zero
    else{
      maxI<-sumIJ
      minI<-0
    }
    
    # iterates over all possible values of i/j (0 to 105)
    for (i in maxI:minI){
      
      # From the sum loop above, j must be the difference between the sum and i's score
      j<-sumIJ-i
      
      # iterates over all possible values of k (0 to 105)
      for (k in max_score:0){
        
        # If player i has won, the prob. of winning is 1 and the loop ends.
        if (i>=(sN-1)){
          V[i+(1),j+(1),k+(1)]<-1
          U[i+(1),j+(1),k+(1)]<-2
        }
        # If player j has won, the prob. of winning is 0 and the loop ends.
        else if (j>=sN){
          V[i+(1),j+(1),k+(1)]<-0
          U[i+(1),j+(1),k+(1)]<-0
        }
        # If the turn total k is at least as big as the goal
        #  then the player holds and wins.
        else if (k>=sN){
          V[i+(1),j+(1),k+(1)]<-1
          U[i+(1),j+(1),k+(1)]<-2
        }
        # If the turn total k is at least as big as the difference
        #  between player i's current score and the goal, 
        #  then player i holds and wins.
        else if (k>=(sN-i)){
          V[i+(1),j+(1),k+(1)]<-1
          U[i+(1),j+(1),k+(1)]<-2
        }
        else{
          # value of rolling
          roll<-p1*(1-V[j+(1), i+1+(1), (1)])+ # rolling a 1
            p2*(V[i+(1),j+(1),k+2+(1)])+ # rolling a 2
            p3*(V[i+(1),j+(1),k+3+(1)])+ # rolling a 3
            p4*(V[i+(1),j+(1),k+4+(1)])+ # rolling a 4
            p5*(V[i+(1),j+(1),k+5+(1)])+ # rolling a 5
            p6*(V[i+(1),j+(1),k+6+(1)])  # rolling a 6
          
          # Value of Holding:       
          # if roll total k is 0, then the player held without
          #  rolling this turn, equivalent to rolling a 1
          if (k == 0) {
            hold<-1-V[j+(1), i+1+(1), (1)]
          }
          # if roll total k is not 0, then the player held 
          #  after at least 1 roll.
          else {
            hold<-1-V[j+(1), i+k+(1), (1)]
          }
          
          # maximum of rolling, holding
          V[i+(1),j+(1),k+(1)]<-max(c(roll,hold))
          U[i+(1),j+(1),k+(1)]<-which.max(c(roll,hold))
        }
        
      } # k loop
    } # j loop
  } # i loop
  
  # save and return decisions matricies
  save(list=c('V', 'U'), file='VUfile.Rdata')
  return (list(V=V,U=U))
}
