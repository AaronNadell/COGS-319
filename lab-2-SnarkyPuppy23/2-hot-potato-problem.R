# this problem is taken from the Riddler series on fivethirtyeight.com
# if you want some additional practice writing monte carlo simulations
# the Riddler series questions can usually be solved with a monte carlo
# approach.

# A class of 30 children is playing a game where they all stand in a 
# circle along with their teacher. The teacher is holding two things:
# a coin and a potato. The game progresses like this: The teacher tosses 
# the coin. Whoever holds the potato passes it to the left if the coin 
# comes up heads and to the right if the coin comes up tails. The game 
# ends when every child except one has held the potato, and the one 
# who hasn’t is declared the winner.

# How do a child’s chances of winning change depending on where they are 
# in the circle? In other words, what is each child’s win probability? 

# Visualize the final result with a graph that shows the position relative
# to the starting location of the potato on the x-axis and the probability
# of winning on the y-axis.
a<- c(1, -1:-29)

times.with.potato <- c(1, matrix(0:0, 1, 29))
position = c(1:30)
pot<- mapply(function(x){
  if (x<0){
    return(F)
  }else{
    return(T)
  }}, a
  )

child <- data.frame(      #Set up data frame
  pos = position,
  potato = pot,
  hadpotato = pot,
  ti.wi = times.with.potato
)

pass.left <-function(i, child){
    if (child[i,'pos'] == 1){ #deals with were the loop would break
      child[1,'potato'] <- FALSE#this and next line effectively pass the potato
      child[30,'potato'] <- TRUE
      child[30, 'ti.wi'] <- child[30, 'ti.wi'] + 1#adds to the running tally of times with potato
      child[30, 'had.potato'] <- TRUE
    }
    else{
      child[i, 'potato'] <- FALSE #attempt to make the general case of passing left
      child[(i-1), 'potato'] <- TRUE
      child[(i-1), 'ti.wi'] <- child[(i-1), 'ti.wi'] + 1
      child[(i-1), 'had.potato'] <- TRUE
    }
  return(child)
}
pass.right <- function(i, child){
  if (child[i,'pos'] == 30){ #deals with were the loop would break
    child[30,'potato'] = FALSE#this and next line effectively pass the potato
    child[1,'potato'] = TRUE
    child[1, 'ti.wi'] = child[1, 'ti.wi'] + 1 #adds to the running tally of times with potato
    child[1, 'had.potato'] = TRUE
  }
  else{
    child[i, 'potato'] = FALSE #attempt to make the general case of passing right
    child[(i+1), 'potato'] = TRUE
    child[(i+1), 'ti.wi'] = child[(i+1), 'ti.wi'] + 1 #adds to the running tally of times with potato
    child[(i+1), 'had.potato'] = TRUE
  }
  return(child)
  }#,
  #child$pos, child$potato, child$hadpotato, child$ti.wi)

check.had.potato<- function(x){
    if (sum(x) < 29){ #if only one child hadn't had the potato then this statement will be false 
      return(T)
    }else{
      return(F)
    }
} #this is a working function
which.has.potato <- function(x){ #cycles through data frame to determine which child has potato
  for (x in 1:30){
    if (child[x, 'potato'] == T){
      return(x)
  }else{
      x = x+1
      }
    }
  }#this is a working function

compiler <- function(y, child){
  #while (check.had.potato(child$hadpotato)) #This should return true while more than one child hasn't had the potato
  x = 0
  while (x< 1000)
   if (runif(1,0,1)<0.5){ #operationalized the 50% chance of passing in either direction
      pass.left(which.has.potato(y), child) #child with potato passes left
      x = x + 1
      return(child)
    }else{
      pass.right(which.has.potato(y), child) #child with potato passes right
      x = x + 1
      return(child)
    }
  return(x)
  return(child)
}
# This code ran into another problem that when I ran a function on the data.frame
# it didn't save it to the actual data.frame.
#####################################################################################
one.round.hot.potato <- function(){
  children <- rep(F, 30)  #initializes
  potato.location <- 0    #initializes
  while (sum(children == T) < 29){  #as long as completion condition is not met...continue
    pass.direction <- sample(c(1,-1), 1) #pass left or pass right
    potato.location <- pass.direction + potato.location#update potato location
    if (potato.location == -1){ #deals with boundary cases 
      potato.location <- 30}
    if (potato.location == 31){ #deals with boundary cases 
      potato.location <- 0
    }
    children[potato.location] <- T  #updates whether a child had a potato
  }
  return(which(children == F)) #final output of child's location that is the winner
}
plays <- replicate(10000, one.round.hot.potato()) #run the function many times
probability <- sapply(1:30, function(x){  #calculate the probability for each child
  return(sum(plays == x)/ length(plays))
})

plot(1:30, probability, ylim =c(0,0.2), type = "o") #plot each probability







#if all but one has had potato then game ends
# this will take some planning to think about how to formalize this problem
# i highly recommend discussing it with a classmate! the simpler you can
# make the abstraction of this problem, the easier it will be to write the
# code. either way, this is a challenging problem so give yourself 
# a high-five when you solve it.