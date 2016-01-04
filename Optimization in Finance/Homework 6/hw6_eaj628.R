# Homework 6
# Evan Johnston, eaj628

##################################################################
#Problem 1

# create weighted adjacency matrix
#      To: 1  2  3  4  5  6  7  8  9  10
# From: 1  0  0  3  2  0  0  0  0  0  0
#       2  0  0  4  2  4  0  0  0  0  0
#       3  0  0  0  0  0  3  2  0  0  0
#       4  0  0  0  0  0  4  0  2  5  0
#       5  0  0  0  0  0  0  2  2  0  0
#       6  0  0  0  0  0  0  0  0  0  3
#       7  0  0  0  0  0  0  0  0  0  4
#       8  0  0  0  0  0  0  0  0  0  2
#       9  0  0  0  0  0  0  0  0  0  3
#       10 0  0  0  0  0  0  0  0  0  0

# for simplicity, apply igraph package to aid in path calculation
require ("igraph")

adj_mat1<-matrix(c(0, 0, 3, 2, 0, 0, 0, 0, 0, 0,
                   0, 0, 4, 2, 4, 0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0, 3, 2, 0, 0, 0,
                   0, 0, 0, 0, 0, 4, 0, 2, 5, 0,
                   0, 0, 0, 0, 0, 0, 2, 2, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 3,
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 4,
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 2,
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 3,
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=10, byrow=T)

# create graph object
network<-graph_from_adjacency_matrix(adj_mat1, mode="directed", weighted=T)
plot(network, layout=layout_nicely(network, dim = 2))

# all paths from 1 to 10
paths1<-all_simple_paths(network, 1, 10)

# shortest path from 1 to 10
spath1<-shortest_paths(network, 1, 10)
spath1
# weight of shortest path from 1 to 10
distances(network, 1, 10)

# all paths from 2 to 10
paths2<-all_simple_paths(network, 2, 10)

# shortest path from 2 to 10
spath2<-shortest_paths(network, 2, 10)
spath2
distances(network, 2, 10)

##################################################################
#Problem 2

years<-c(seq(1,6))
resales<-c(14000, 12000, 8000, 6000, 4000, 2000)
costs<-c(600, 1000, 1600, 2400, 3200, 4400)
values<-c(rep(0,6))
decisions<-c(rep(0,6))

# the only state in the problem is time
# note that there is no discounting in this problem
for (year in length(years):1){
  
  # owner will sell in year 7, thus there is no future operating cost
  if (year==length(years)){
    costChoices<-c(-resales[year]+costs[year], 0)
    value<-min(costChoices)
    decision<-which.min(costChoices)
  }else{
    # Bellman: Each year the choices are to sell or operate, goal is to minimize cost
    # V(t)=min[-resale(t)+cost(t), value(t+1)]
    costChoices<-c(-resales[year]+costs[year], values[year+1])
    value<-min(costChoices)
    decision<-which.min(costChoices)
  }
  #print (costChoices)
  
  # The minimum cost is chosen
  values[year]<-value
  
  # store decisions here
  decisions[year]<-decision
}

# Clearly the best decision is to sell the car as quickly as possible
#  as it continues to decrease in value and increases in cost every year.
#  Thus the decsion vector indicates selling is the best option in all
#  periods until the last (6) where is is more beneficial to wait as
#  selling still gives a negative value.
values
decisions

##################################################################
#Problem 3

# create matrix of net gains from traveling
# 1: Indianapolis [end]
# 2: Bloomington [start]
# 3: Chicago

#      To: 1      2       3 
# From: 1  120-0  160-50  170-20
#       2  120-50 160-0   170-70
#       3  120-20 160-70  170-0

travel<-matrix(c(120, 110, 150,
                 70, 160, 100,
                 100, 90, 170), nrow=3, byrow=T)

# states are time (days) and place (cities)
days<-4
cities<-3
names<-c("Ind", "Bloom", "Chic")

values<-matrix(NA, days, cities)
decisions<-matrix(NA, days, cities) 

# iterate time
for (t in 1:days){
  print(paste("t=",t))
  # iterate cities
  for (c in 1:cities){
    
    # At t=1, the salesperson must start at 2 (Bloom)
    if (t==1){
      choices<-travel[2,]
      value<-max(choices)
      decision<-which.max(choices)
      print(paste("from", names[2], "to", names[decision]))
    }  
    # At all other t, salesperson must arrive at the position at t+1
    #  for a given c
    if (t>1 & t<4){
      choices<-travel[decisions[t-1,c],]
      value<-max(choices)
      decision<-which.max(choices)
      print(paste("from", names[decision], "to", names[decisions[t-1,c]]))
    }
    # At end of the period salesperson must be at 1 (Indy)
    if (t==4){
      choices<-travel[decisions[t-1,c],1]
      value<-max(choices)
      decision<-which.max(choices)
      print(paste("from", names[decision], "to", names[1]))
    }
    print("choices")
    print(choices)
    print("decision")
    print(decision)
    print("")
    values[t,c]<-value
    decisions[t,c]<-decision
  }
}

values
decisions

