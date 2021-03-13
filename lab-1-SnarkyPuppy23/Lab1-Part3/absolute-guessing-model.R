#### Setting up the data ####
# This section of the code will create a data frame that describes
# each trial in the experiment. The data frame will have two columns:
#
# stimulus - The ordinal value of the stimulus on the current trial
# last.stimulus - The ordinal value of the stimulus on the last trial
#
# The order of trials is generated in a way that avoids repeats of the
# same stimulus on neighboring trials.

# Parameters to control trial generation
n.trials <- 200 # How many trials for each stimulus?
n.stimuli <- 9 # How many different stimuli?

# Create a random order of trials, with no neighboring repeats.
trials <- sample(1:n.stimuli)
for(i in 2:n.trials){
  next.order <- sample(1:n.stimuli)
  while(next.order[1] == trials[length(trials)]){
    next.order <- sample(1:n.stimuli)
  }
  trials <- c(trials, next.order)
}

# Create the array that describes the value of the last trial
# Use NA for the first trial, to represent no previous information.
last.trial <- c(NA, trials[1:(length(trials)-1)])

# Create the data frame
trial.data <- data.frame(stimulus=trials, last.stimulus=last.trial)

#### Model of responses ####

# Your work starts here. Implement the model described in the readme
# file. You should add a new column to trial.data that indicates whether the
# model guessed correctly (TRUE) or incorrectly (FALSE).

# You may want to start by adding a column to indicate what stimulus the model
# guessed. Then you can create a fourth column to indicate whether the guess
# was correct.
trial.data$guess <- mapply(function(last, current){
    if(is.na(last)){  #I didn't know how to check for NA so I defined the NA in
      return(sample(1:n.stimuli, 1)) # the first trial to be 0
    }
    else if (last < current){
      low <- last + 1                    #I didn't know how to efficiently define the boundaries
      high <- n.stimuli                 #that I wanted to guess between.
    }else{                         
      low <- 1 
      high <- last - 1
       #guess a number lower than the last one.
    }
  guess <- sample(low:high, size = 1)  # define a guess based on knowledge about previous and current trials
    return(guess) #return the output of my guessing model.
  }, trial.data$last.stimulus, trial.data$stimulus)


trial.data$correct <- trial.data$stimulus == trial.data$guess #determines whether stimulus matches guess


# Don't forget about the better.sample function that you wrote in the tutorial file!


#### Aggregate the data ####

# Now that you have a model that can generate a response for every trial, you need
# to group the data and find the proportion of trials that the model answered correctly
# for each value of the stimulus column. Then you can compare the data your model generated
# to the data generated in the Neath and Brown experiment.

# This is where you will need to use the dplyr summarize function. Generate a new data frame
# that has the proportion of correct responses for each value of stimulus. (Note that the proportion
# correct is equivalent to taking the mean of the correct column if you code incorrect responses as 0
# and correct responses as 1).

library(dplyr)
summarized.data <- trial.data %>% group_by(stimulus) %>% summarize(mean = mean(correct)) #I didn't know how to use this pipleline efficiently.

#ones<-(trial.data %>% filter(stimulus == 1) %>% filter(correct == TRUE))
#ones1<-(trial.data %>% filter(stimulus == 1) %>% filter(correct == FALSE))
#twos<-(trial.data %>% filter(stimulus == 2) %>% filter(correct == TRUE))
#twos1<-(trial.data %>% filter(stimulus == 2) %>% filter(correct == FALSE))
#threes<-(trial.data %>% filter(stimulus == 3) %>% filter(correct == TRUE))
#threes1<-(trial.data %>% filter(stimulus == 3) %>% filter(correct == FALSE))
#fours<-(trial.data %>% filter(stimulus == 4) %>% filter(correct == TRUE))
#fours1<-(trial.data %>% filter(stimulus == 4) %>% filter(correct == FALSE))
#fives<-(trial.data %>% filter(stimulus == 5) %>% filter(correct == TRUE))
#fives1<-(trial.data %>% filter(stimulus == 5) %>% filter(correct == FALSE))
#sixes<-(trial.data %>% filter(stimulus == 6) %>% filter(correct == TRUE))
#sixes1<-(trial.data %>% filter(stimulus == 6) %>% filter(correct == FALSE))
#sevens<-(trial.data %>% filter(stimulus == 7) %>% filter(correct == TRUE))
#sevens1<-(trial.data %>% filter(stimulus == 7) %>% filter(correct == FALSE))
#eights<-(trial.data %>% filter(stimulus == 8) %>% filter(correct == TRUE))
#eights1<-(trial.data %>% filter(stimulus == 8) %>% filter(correct == FALSE))
#nines<-(trial.data %>% filter(stimulus == 9) %>% filter(correct == TRUE))
#nines1<-(trial.data %>% filter(stimulus == 9) %>% filter(correct == FALSE))

#on<-length(ones$stimulus)/length(ones1$stimulus)
#tw<-length(twos$stimulus)/length(twos1$stimulus)
#th<-length(threes$stimulus)/length(threes1$stimulus)
#fo<-length(fours$stimulus)/length(fours1$stimulus)
#fi<-length(fives$stimulus)/length(fives1$stimulus)
#si<-length(sixes$stimulus)/length(sixes1$stimulus)
#se<-length(sevens$stimulus)/length(sevens1$stimulus)
#ei<-length(eights$stimulus)/length(eights1$stimulus)
#ni<-length(nines$stimulus)/length(nines1$stimulus)

#### Plot the results ####

# Plot the curve with stimulus on the X axis and proportion of correct
# responses on the Y axis.

# Remeber that you can extract a column of data with the $ operator, so something like:
# plot(data$stimulus, data$proportion.correct) should get you close to where you want to be.
plot(summarized.data$stimulus, summarized.data$mean, type="l")

#wt =c(1,2,3,4,5,6,7,8,9)
#mpg=c(on,tw,th,fo,fi,si,se,ei,ni)
#plot(wt, mpg, main="Scatterplot", 
    # xlab="Stimulus ", ylab="Percent Correct", pch=19)


#### Short answer questions (reply using a comment below each number)

# 1. Why does the model's output change slightly each time you run it?
      #because it is based on random guesses...
# 2. Try increasing and decreasing the number of trials per stimulus. How does
#    this affect the stability of the model's predictions from run to run?
#    Explain why this happens.
        #The model will approach the limit of chance becoming more parabolic.
        #This follows the rules of long term means.
# 3. Explain why the stimuli at the ends have a higher proportion correct than
#    those in the middle under this model.
         # Because there is a larger range to guess from, 
        # there is a greater chance of being inaccurate
# 4. Compare the model's accuracy to the data from Neath and Brown (2005). What
#    is the major difference? What does this suggest about the guessing model?
      #The overall model is less accurate than the actual experimental data, which
      # suggests that participants weren't guessing soley based on chance.

# I think I deserve an A- on this because I implemented a model that was close to working,
# it just had a few kinks and was more inefficient than your model.
