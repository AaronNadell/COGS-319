# implement the model by filling in the function below
# the model should return a data frame with two columns: correct and rt
# the correct column should be TRUE or FALSE, and rt should contain the
# number of steps it took to reach the criterion.

# note that the function takes four arguments:
# samples is the number of samples to draw from the model
# rate.1 is the evidence accumulation rate for the correct response (default value is 40)
# rate.1 is the evidence accumulation rate for the incorrect response (default value is 40)
# criterion is the threshold for a response (default value is 3)

# one oddity: note that higher values for rate.1 and rate.2 will actually produce slower RTs.
# this is because the rate parameter is controlling the rate of decay of the exponential distribution,
# so faster rates mean that less evidence is likely to accumulate on each step. we could make
# these parameters more intuitive by taking 1/rate.1 and 1/rate.2 as the values to rexp().

accumulator.model <- function(samples, rate.1=40, rate.2=40, criterion=3){
    rt.array <- c()
    accuracy.array <- c()
    for (i in 1:samples){ #counts number of samples
      value.1 <- 0
      value.2 <- 0
      rt <- 0
      while (value.1 < criterion && value.2 < criterion){ #loop performs main function
        value.1 <- value.1 + rexp(1, rate.1) #stores and adds values to variable 1...takes random steps
        value.2 <- value.2 + rexp(1, rate.2) #stores and adds values to variable 2...takes random steps
        rt <- rt + 1} #add to running count of reaction time
      
      if(value.1 > criterion && value.2 > criterion){ #if they go over on the same rt then the bigger one
        if (value.1 >= value.2){ #is counted
          accuracy.array[i] <- T
        }
        else{
          accuracy.array[i] <- F
        
        }
      }
      if (value.1 > criterion && value.2 < criterion){     #takes the values we built up and builds an array of values
        accuracy.array[i] <- T
      }
      else{
        accuracy.array[i] <- F
      }
      rt.array[i]<- rt
    }
    output <- data.frame( #creates data.frame with our values
      correct = accuracy.array,
      rt = rt.array
    )
    return(output)
}

# test the model ####

# if the model is working correctly, then the line below should generate a data frame with 
# 1000 samples and about half of the samples should be correct. the average rt will probably
# be around 112, but might vary from that by a bit.

initial.test <- accumulator.model(1000)
sum(initial.test$correct) / length(initial.test$correct) # should be close to 0.5
mean(initial.test$rt) # should be about 112

# visualize the RT distributions ####

# we can use dplyr to filter the data and visualize the correct and incorrect RT distributions

library(dplyr)

correct.data <- initial.test %>% filter(correct==TRUE)
incorrect.data <- initial.test %>% filter(correct==FALSE)

hist(correct.data$rt)
hist(incorrect.data$rt)