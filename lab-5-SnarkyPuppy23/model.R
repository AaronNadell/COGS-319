library(dplyr)
library(ggplot2)

# this model will simulate a group of subjects completing the experiment described in the README.
# your job is to implement the reinforcement learning model

# experiment parameters
n.subjects <- 60 # how many simulated subjects to include
n.trials <- 30 # how many trials will each subject complete
prob.reward.A <- 0.8 # the probability of a positive reward for chosing option A
prob.reward.B <- 0.2 # the probability of a positive reward for chosing option B
reward.value <- 1 # the value of a positive reward

# model parameters
alpha <- 0.1 # controls the learning rate of the model
temperature <- 0.125 # controls the explore/exploit tradeoff of the decision rule

# implementing softmax ####

# the softmax decision rule is described in the README. implement the function here,
# returning the probability of selecting option A.

softmax.rule <- function(a.value, b.value, temperature){
        Pa <- exp((a.value*1)/temperature)/(exp(a.value/temperature)+exp(b.value*1/temperature))
        return(Pa)
}
softmax.rule(a.value=2, b.value=1, temperature=1) # should be 0.731
softmax.rule(a.value=10, b.value=1, temperature=5) # should be 0.858

# model ####

# this function should run one subject through all trials.
# you should store the choice that the model makes and
# the model's probability of choosing option 'a' at each trial.
# at the end, you will return a data frame that contains this information.
# this part of the code has been provided to you.

# for the model, follow the description in the README file. 
#initialize values
A <- 0
B <- 0
choices.history <- c()
prob.a.history <-c()
run.one.subject <- function(){
  for (i in 1:n.trials){ #start a loop or 1:n.trials
    #initialize values
    reward <- 0
    sample.1 <- softmax.rule(A,B, temperature) #compute probability of choosing 'a'
    sample.2 <- sample(c('a', 'b'), 1, prob=c(sample.1, (1-sample.1)), replace = T) #choose 'a' or 'b'
    if (sample.2 == 'a'){ #if sample chosen is 'a'
      reward <- sample(c(0, 1), 1, prob = c((1-prob.reward.A), prob.reward.A))#do they get a reward?
      A <- A + alpha*(reward - A) #update A's value based on reward or not
    }else if (sample.2 == 'b'){ #if sample chosen is 'b'
      reward <- sample(c(0, 1), 1, prob = c((1-prob.reward.B), prob.reward.B))#do they get a reward?
      B <- B + alpha*(reward - B)  #update B's value based on reward or not
      }
      choices.history <- c(choices.history, sample.2) # please make sure that the values added to this are either 'a' or 'b'
      prob.a.history <- c(prob.a.history, sample.1)
    }
  return(data.frame(trial=1:n.trials, choice=choices.history, probability=prob.a.history))
}

# if you've implemented the model above correctly, then running this block of code with the
# default parameters (30 trials, prob.reward.A = 0.8, prob.reward.B = 0.2, reward.value = 1, alpha = 0.1, temperature = 0.125)
# should produce a final probability of selecting option A on trial 30 of 0.9975339. the model will also, by strange coincidence,
# choose option 'a' for every trial. This doesn't have to match up... may depend on how I implemented it...
set.seed(12604)
test.run.data <- run.one.subject()

# this code is provided to you to run multiple subjects, and aggregate their data into a single data frame
# note that it will cause R to display a WARNINGS message, but this is OK.
experiment.data <- NA # create variable to hold data.
for(s in 1:n.subjects){ # loop through each subject
  subject.data <- run.one.subject() # run the subject with a fresh model
  if(is.na(experiment.data)){ # if there is no data yet...
    experiment.data <- subject.data # ... then make this subject's data the experiment.data
  } else { # otherwise...
    experiment.data <- rbind(experiment.data, subject.data) # ... add this subject's data to the end of the set
  }
}

# this code uses the dplyr library to calculate the percentage of subjects who chose 'a' on each trial
# and to calculate the mean probability of selecting 'a' according to the model.
summarized.data <- experiment.data %>%
  group_by(trial) %>%
  summarize(choice_pct = sum(choice=="a")/n(), prob_mean = mean(probability))

# this code uses the ggplot2 library to make a plot similar to Figure 1 in the Pessiglione et al. (2006) paper.
ggplot(summarized.data, aes(x=trial, y=choice_pct))+
  geom_point()+
  geom_line(aes(y=prob_mean))+
  ylim(c(0,1))+
  labs(x="Trial Number", y="Modelled choices (%)")+
  theme_bw()


########################################################
#PREDICTION ERROR
A <- 0
B <- 0
choices.history <- c()
prob.a.history <-c()
PE.history <-c()
run.one.subject <- function(){
  for (i in 1:n.trials){ #start a loop or 1:n.trials
    #initialize values
    reward <- 0
    sample.1 <- softmax.rule(A,B, temperature) #compute probability of choosing 'a'
    sample.2 <- sample(c('a', 'b'), 1, prob=c(sample.1, (1-sample.1)), replace = T) #choose 'a' or 'b'
    if (sample.2 == 'a'){ #if sample chosen is 'a'
      reward <- sample(c(0, 1), 1, prob = c((1-prob.reward.A), prob.reward.A))#do they get a reward?
      PE <- reward - A
      A <- A + alpha*PE#update A's value based on reward or not
    }else if (sample.2 == 'b'){ #if sample chosen is 'b'
      reward <- sample(c(0, 1), 1, prob = c((1-prob.reward.B), prob.reward.B))#do they get a reward?
      B <- B + alpha*(reward - B)#update B's value based on reward or not
      PE <- reward - B
    }
    choices.history <- c(choices.history, sample.2) # please make sure that the values added to this are either 'a' or 'b'
    prob.a.history <- c(prob.a.history, sample.1)
    PE.history <- c(PE.history, PE)
  }
  return(data.frame(trial=1:n.trials, choice=choices.history, probability=prob.a.history, PE=PE.history))
}
experiment.data <- NA # create variable to hold data.
for(s in 1:n.subjects){ # loop through each subject
  subject.data <- run.one.subject() # run the subject with a fresh model
  if(is.na(experiment.data)){ # if there is no data yet...
    experiment.data <- subject.data # ... then make this subject's data the experiment.data
  } else { # otherwise...
    experiment.data <- rbind(experiment.data, subject.data) # ... add this subject's data to the end of the set
  }
}
summarized.data <- experiment.data %>%
  group_by(trial) %>%
  summarize(choice_pct = sum(choice=="a")/n(), prob_mean = mean(probability)/n(), PE_mean=mean(PE))

ggplot(summarized.data, aes(x=trial, y=PE_mean))+
  geom_point()+
  ylim(c(0,1))+
  labs(x="Trial Number", y="PE")+
  theme_bw()
# QUESTIONS ####

# 1. Try running the model with different values for alpha and temperature. What is the effect of each parameter
#    on the final figure that is produced? Explain why these effects happen.
        #with alpha = 0.5 and same Temp, The model curve aproached the assymptote sooner after
                #approx. 10 trials
        #with alpha = 0.001 and same Temp, The model's cure was linear and never approched the assymptote.
        #with same alpha and Temp = 0.5, This made the model's curve more linear with more variability in % modelled choices
                #which is expected.
        #with same alpha and Temp = 0.001, This made the models curve approach 0.00 very quickly
                #because the model has very low chances of choosing 'a' after it chose 'b' on the first round
# 2. Pessiglione et al. also included a condition where the reward was negative instead of positive. They plot
#    the results as squares in Figure 1. Simulate this data. Can you match the general result? Why is the probability
#    curve in both Figure 1 and your simulation less smooth for this simulation than when the reward is positive?
                #it will inherently be less smooth because there's more variability for each stimulus
                #more specifically on some trials the model may be thrown off because it might get a punishment
                #for stimulus 'a' and thereby avoid it for a bit before learning that
                # 'a' has the best probability for reward.
# 3. CHALLENGE (If you completed the rest of the lab relatively quickly, do this problem. If it took you plenty of
#    effort to complete the model, you can choose whether to pursue this problem or not.): 
#    In the paper, the authors use the model's reward prediction error to find brain regions that 
#    correlate with this signal. Modify the model to save the reward prediction error on each step. Then plot
#    the average reward prediction error for the 30 trials. Explain the shape of the graph. You may want to copy-and-paste
#    the model code into a new file to do this.
        # The model has decreasing reward prediction errors which makes sense because as the model
        # learns, the predicted and actual values should become closer so their difference should
        # go toward zero. 

