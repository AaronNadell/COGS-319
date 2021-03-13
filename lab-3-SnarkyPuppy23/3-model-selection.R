# model selection ####

# suppose we have data from an experiment like this:
# mean RT correct = 250ms
# mean RT incorrect = 246ms
# accuracy = 0.80
set.seed(12604)
#                                           #shifts bias    #shifts RTs
initial.test.rw <- random.walk.model(10000, drift = 0.00802, sdrw= 0.1820, criterion = 3)
correct.data.rw <- initial.test.rw %>% filter(correct==TRUE)
incorrect.data.rw <- initial.test.rw %>% filter(correct==FALSE)
#mean(initial.test.rw$rt)
mean(correct.data.rw$rt)
mean(incorrect.data.rw$rt)
sum(initial.test.rw$correct) / length(initial.test.rw$correct)
##############################################################################
# Intreasing the rates causes longer RTs...                                          
initial.test.acc <- accumulator.model(10000, rate.1=81, rate.2=87, criterion=3)
correct.data.acc <- initial.test.acc %>% filter(correct==TRUE)
incorrect.data.acc <- initial.test.acc %>% filter(correct==FALSE)
#mean(initial.test.acc$rt)
mean(correct.data.acc$rt)
mean(incorrect.data.acc$rt)
sum(initial.test.acc$correct) / length(initial.test.acc$correct)
# try to fit this data with both models by adjusting the parameters of the model
# HINT: you can speed up your parameter search by using a small number of samples
# initially, and then increasing the samples as you get closer to a viable set
# of parameters.
# 2nd HINT: Don't adjust the sdrw parameter of the random.walk.model or the criterion
# paramter of the accumulator model.

# You don't need to get a perfect match. Just get in the ballpark. 


# Can both models do a reasonable job of accounting for the mean RT and accuracy? Report the
# results of your efforts:
        #The random walk can get reasonably close however,
        # the accumulator model cannot. This is because the rate.1 must be
        # smaller than rate.2 which means that on average it will also have
        # a shorter run time(RT).


# Using the parameters that you found above, plot histograms of the distribution of RTs
# predicted by each model. Based on these distributions, what kind of information could
# we use to evaluate which model is a better descriptor of the data for the experiment?
# Describe briefly how you might make this evaluation.

# RW model 
hist(correct.data.rw$rt)
hist(incorrect.data.rw$rt)

# Acc model
hist(correct.data.acc$rt, breaks=seq(0,2000,10), main="ACC Model, correct", xlab="RT")
hist(incorrect.data.acc$rt,  breaks=seq(0,2000,10), main="ACC Model, correct", xlab="RT")
# We could judge the models based on how wide the distribution of points are
# and where the mean of their distributions are.
# The RW model has means that are near the target values but has a wide range of values.
# the Acc model doesn't have means where they should be but has a much narrower range of
# values. You might choose which you wish to prioritize, mean values, or a narrow SD. 