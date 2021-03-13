# part 1 ####

# suppose there are 30 students in a class. estimate the probability that at least one pair
# students has the same birthday.

# you can ignore the possibility of people being born on Feb 29 during a leap-year.
individual.number <- 30 #gives the total number of individuals

count = 1
p.notsame <- function(x){ #gives the probability of x individuals having the same birthday
  for (i in 0:(x-1)){         #with one individual
    count = (365-x)/365 * count
}
  print(count)
} 
total.probability <- (1 - p.notsame(individual.number))

# part 2 ####

# estimate the probability for class sizes from 5-60, and plot the resulting curve 
# (x axis is class size, y axis is probability of at least one shared birthday)
a <- c()
  for (i in 5:60){
    a[[i]] = 1 - p.notsame(i)#runs my probability not same function on individuals from 5 to 60
  }
print(a)
a[1] = 0 #had to correct NAs in my list because it returned them for some reason
a[2] = 0
a[3] = 0
a[4] = 0
plot.default((5:64), a)


