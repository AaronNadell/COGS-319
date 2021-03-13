# This interactive activation network will recognize digits.
# The digits are represented as 7-dimensional binary vectors
# (There are 7 TRUE or FALSE values per digit)
# These represent the presence or absence of a line-segment feature.

# The function below will draw the stimuli. Load it into the R environment
# by running the code. Then scroll down.

draw.stimulus <- function(features){
  line.width <- 10
  par(mar = c(1, 1, 1, 1))
  plot.new()
  plot.window(xlim=c(0,1),ylim=c(0,2),asp=1,)
  lines(c(0,0),c(0.1,0.9), col="grey90", lend="butt", lwd=line.width)
  lines(c(0,0),c(1.1,1.9), col="grey90", lend="butt", lwd=line.width)
  lines(c(1,1),c(0.1,0.9), col="grey90", lend="butt", lwd=line.width)
  lines(c(1,1),c(1.1,1.9), col="grey90", lend="butt", lwd=line.width)
  lines(c(0.1,0.9),c(0,0), col="grey90", lend="butt", lwd=line.width)
  lines(c(0.1,0.9),c(1,1), col="grey90", lend="butt", lwd=line.width)
  lines(c(0.1,0.9),c(2,2), col="grey90", lend="butt", lwd=line.width)
  if(features[1] == 1){
    lines(c(0,0),c(0.1,0.9), col="red", lend="butt", lwd=line.width)
  }
  if(features[2] == 1){
    lines(c(0,0),c(1.1,1.9), col="red", lend="butt", lwd=line.width)
  }
  if(features[3] == 1){
    lines(c(1,1),c(0.1,0.9), col="red", lend="butt", lwd=line.width)
  }
  if(features[4] == 1){
    lines(c(1,1),c(1.1,1.9), col="red", lend="butt", lwd=line.width)
  }
  if(features[5] == 1){
    lines(c(0.1,0.9),c(0,0), col="red", lend="butt", lwd=line.width)
  }
  if(features[6] == 1){
    lines(c(0.1,0.9),c(1,1), col="red", lend="butt", lwd=line.width)
  }
  if(features[7] == 1){
    lines(c(0.1,0.9),c(2,2), col="red", lend="butt", lwd=line.width)
  }
}

# To see how the digits are represented, try drawing some:
draw.stimulus(c(1,1,1,1,1,-1,1))

# Notice that there is a line for each 1 and no line for a -1. 
# 1s therefore represent the presence of a feature, and -1s represent the absence of the feature.

# Draw the digits 0-9 below. This will also help you set up your connection weights in the next step.

# Drawing digits 0-9
layout(matrix(1:10, nrow=2, byrow=T))
# 0
draw.stimulus(c(1,1,1,1,1,-1,1))
# 1
draw.stimulus(c(-1,-1,1,1,-1,-1,-1))
# 2
draw.stimulus(c(1,-1,-1,1,1,1,1))
# 3
draw.stimulus(c(-1,-1,1,1,1,1,1))
# 4
draw.stimulus(c(-1,1,1,1,-1,1,-1))
# 5
draw.stimulus(c(-1,1,1,-1,1,1,1))
# 6
draw.stimulus(c(1,1,1,-1,1,1,1))
# 7
draw.stimulus(c(-1,-1,1,1,-1,-1,1))
# 8
draw.stimulus(c(1,1,1,1,1,1,1))
# 9
draw.stimulus(c(-1,1,1,1,-1,1,1))

#### Setting up the connection weights ####


# In this network, we will hand-code the connections between the input features and the digits.
# We'll use a matrix to represent these weights, where each ROW of the matrix is one of the
# line segment features and each COLUMN is one of the 10 possible digits. If the line segment
# represented by a row appears in the digits represented by the column, we want a 1 to appear
# in that cell of the matrix. Otherwise, we want a -1 to appear.

# Note that by drawing the stimuli above, you've already created all of the weights. You just
# need to get them into the correct matrix format. HINT: You can use c() to combine arrays, like:
# c(
#   c(1,1,1,1),
#   c(1,-1,-1,1)
# )

# row is a feature, column is a digit.
feature.to.digit.connections <- matrix(
  c(
    c(1,1,1,1,1,-1,1),
    c(-1,-1,1,1,-1,-1,-1),
    c(1,-1,-1,1,1,1,1),
    c(-1,-1,1,1,1,1,1),
    c(-1,1,1,1,-1,1,-1), #4
    c(-1,1,1,-1,1,1,1),
    c(1,1,1,-1,1,1,1),
    c(-1,-1,1,1,-1,-1,1),
    c(1,1,1,1,1,1,1),
    c(-1,1,1,1,-1,1,1)), 7, 10
)

# If you've done this correctly then this chunk of code should draw a 2, because
# the third column of the matrix represents the connections for the digit 2.
layout(1)
draw.stimulus(feature.to.digit.connections[,3])  


#### Defining the model ####

# Model parameters

# Now that you have the correct set of connection weights from the input features
# to the digits, we need to set the strength of these connections. We'll make all
# excitatory connections have the same weight. We can change this value to explore
# the behavior of the model.

excitatory.weight <- 0.05

# In this model, each digit will have an inhibitory connection to all other digits.
# This parameter defines the strength of all of those connections.
inhibitory.weight <- -0.1

# One cycle of the model is equal to one set of activity updates. We can run the model
# for multiple cycles, which we can think of as additional samples from the perceptual
# environment (as in the sequential sampling models). The more cycles, the more stable
# the output.
cycles <- 10

## Programming a cycle of the model ####

# We'll represent the input to the model as 7 features with 1s and -1s indicating the
# presence and absence of a feature.
input <- c(-1,1,1,1,-1,1,-1)
draw.stimulus(input) # visualize the input.

# First, let's define a vector to record the activity of the 10 digit units. We'll
# set the starting activation value for each digit to 0.
digit.units <- rep(0,10)

# Now we need to write the main update rule for the model. Here's what needs to happen:

# On each cycle of the model:
# 1. Compute the excitatory input to each of the 10 digit units. The excitatory input is
#    equal to the connection vector for that digit (one COLUMN of feature.to.digit.connections)
#    times the input.weight parameter (which will scale the 1s and -1s to be equal to the weight
#    times the input vector. Finally, take the sum of this vector. All of these steps together
#    are equivalent to the dot product of the input and the weights.
# 2. Add the excitatory input sum for each of the 10 digit units to the current activity
#    of that unit.
# 3. Check that none of the digit units is less than 0. If it is, set it equal to 0.
# 4. Now we need to calculate the inhibitory connections between digit units. Each digit
#    receives 9 inhibitory connections, one from each of the other 9 digits. Each connection
#    has the same weight: inhibitory.weight. This means that we can use a trick: rather than
#    calculate each weight*activity pair separately, we can simply sum the activity of all
#    the digit.units OTHER than the unit we are calculating for, and multiply this sum by 
#    the inhibitory weight. Here's a neat R trick to help with this. Say we want the sum
#    of all the values in this array, except we don't want to include the 3rd value:

        a <- c(4,2,6,4)

#    We can put a minus sign in front of the indecies that we don't want when using [] to 
#    to select from an array:
        
        sum(a[-3])
        
#    Notice that the answer is 10. Use this trick to help find the total amount of inhibitory
#    activity to each node. HINT: use a for loop with (i in 1:10) and use digit.units[-i] to 
#    select all the nodes that don't match i!
# 5. Once you have calculated the total inhibitory activity for each of the 10 units, add this
#    activity to all 10 units at once. (If you add the activity as you go, then you'll be changing
#    the amount of inhibitory activity that some nodes receive.) HINT: Remember that you can add
#    two vectors together like so:
#      c(1,3,5) + c(2,4,6)
#    will equal
#      c(3,7,11)
# 6. Check that no digit.unit is below 0. If it is, set it to 0.
        
# Repeat the steps above on each cycle until the number of desired cycles is reached.

#Initializes the variables
digit.units <- rep(0,10) 
excitatory.weight <- 0.05
inhibitory.weight <- -0.10
cycles <- 10 #initialize the Cycles
  for (x in 1:cycles){ #Creates the overall cycle for loop
    a <- rep(0,10)
    inhibitory.connections<-rep(0,10)
    for (i in 1:10){ #takes the dot product of input and weights
      a[i] <- sum(feature.to.digit.connections[,i] * excitatory.weight * input)
    }
    digit.units <- digit.units + a  #adds the above to the running count.
    for (i in 1:10){ #if any of the places in digit.units is negative then make it zero
      if (digit.units[i] < 0){
        digit.units[i] <- 0
      }else{
        digit.units[i] <- digit.units[i]
      }
    }
    for (i in 1:10){#calculates the inhibitory weights
      inhibitory.connections[i] <- sum(digit.units[-i]) * inhibitory.weight
    }
    digit.units <- digit.units + inhibitory.connections #adds the inhibitory weights to the running count
    for (i in 1:10){ #again removes non-positive numbers from running count.
      if(digit.units[i] < 0){
        digit.units[i] <- 0
      }else{
        digit.units[i] <- digit.units[i]
      }
    }
  }


        
# After running the model for the desired number of cycles, the digit.units vector will
# contain the activation value for each of the digits.
        
# One way to interpret these values is as a kind of probability of selecting each digit.
# We can make this more clear by normalizing the vector (dividing each element by the sum
# of the whole vector). When we do this, the sum of the vector will be 1.0, so we have 
# a set of probabilities! 

response.probability <- digit.units / sum(digit.units)

# If you have implemented the model correctly and you are using the default parameters,
# (excitatory.weight = 0.05, inhibitory.weight = -0.10, cycles = 10), then the
# response.probability vector should be:
# [1] 0.0000000 0.0000000 0.0000000 0.0000000 0.7946372 0.0000000 0.0000000 0.0000000 0.0000000
# [10] 0.2053628

# So about an 80% chance of identifying the input as a 4, and a 20% chance of identifying 
# it as a 9.

### explore the model ####

# 1. What happens when you set the inhibitory.weight to 0, removing the inhibitory connections?
      #There is less accuracy because the digit with greater matches aren't inhibiting the
      #other digits. The correct digit that actually matches still has the greatest chance of being
      #chosen, but significantly less than if the model had inhibtory connections.
# 2. Try showing the model various "partial" digits, where you remove or add a feature to a correct digit.
#    Does the model still make the correct classification? There are probably cases where it does, and cases
#    where it doesn't. See if you can find an example of each. Explain what is happening.
draw.stimulus(c(-1,-1,-1,1,-1,1,-1))
  #When I removed two stimuli to the reference #4 it changes the probability ot be entirely equal to 9
  #for the same input. 
draw.stimulus(c(1,1,1,1,-1,1,-1))
  #When I added a stimuli to the reference the probabilities stayed roughly the same that it was
  #a 4.
  #This happens because in the first all the stimuli match the 9, but in the second cause more of
  #the stimuli match the 4 than any other number. 
# 3. Change the input patterns you give the model so that instead of using -1s to represent the
#    absence of a feature, you use 0. Compare the model's predictions in this case to the case with
#    -1s. What's different, and why?
    #There are 3 numbers that are equally likely, 4, 8, and 9. These three numbers share all
    #4 stimuli but 8 and 9 have extra stimuli. The excitarory weights will be the same for all of
    #the numbers because of the absence of -1 to counterbalance the excitatory sum.
    #They will then all inhibit one another equally as well, so they will have the
    # same proability of being chosen. 
