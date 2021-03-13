# let's imagine that we scaled up Fisher's tea tasting experiment
# and had 10 people each taste 8 cups of tea, counting the number 
# of correct responses for each person.

tea.tasting.data <- data.frame(
  subject = 1:10,
  correct.responses = c(5,3,8,6,5,3,7,4,4,8)
)

# first: what's the likelihood of a model that says that everyone is randomly guessing?
# to answer this you need to calculate the probability of each subject's number of correct
# responses assuming that each subject has a 50% chance of a correct response. you can use
# the binomial distribution for this, specifically the dbinom() function, which gives the
# density of the binomial distribution.
a <- c()
for (i in 1:10){
  a <- c(a, dbinom(tea.tasting.data[i,2], 8, prob = 0.5, log = F))
  return(a)
}


b <- dbinom(tea.tasting.data[1,2], 8, prob = 0.5, log = F)*
dbinom(tea.tasting.data[2,2], 8, prob = 0.5, log = F)*
dbinom(tea.tasting.data[3,2], 8, prob = 0.5, log = F)*
dbinom(tea.tasting.data[4,2], 8, prob = 0.5, log = F)*
dbinom(tea.tasting.data[5,2], 8, prob = 0.5, log = F)*
dbinom(tea.tasting.data[6,2], 8, prob = 0.5, log = F)*
  dbinom(tea.tasting.data[7,2], 8, prob = 0.5, log = F)*
  dbinom(tea.tasting.data[8,2], 8, prob = 0.5, log = F)*
  dbinom(tea.tasting.data[9,2], 8, prob = 0.5, log = F)*
  dbinom(tea.tasting.data[10,2], 8, prob = 0.5, log = F)












# second: generalize your code above into a function that takes a single argument, theta,
# and calculates the likelihood for that model.
theta <- 0.5
calculate.likelihood <- function(theta){
  tea.tasting.data <- tea.tasting.data %>%
    mutate(p.response = dbinom(correct.responses, 8, theta))
  likelihood <- prod(tea.tasting.data$p.response)
}

# third: generate a plot with theta values on the X-axis, and L(H) on the Y-axis.
# hint: create an array of X values from 0 to 1 in small steps, then use sapply() to
# get the corresponding array of Y values.
X_values <- seq(from=0, to=1, by=0.1)
y <- sapply(X_values, calculate.likelihood)

plot(X_values, y, xlab = "theta values", ylab = "likelihood")


