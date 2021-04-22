library(dslabs)
data(murders)

# Define a variable states to be the state names from the murders data frame
state <- murders$state

# Define a variable ranks to determine the population size ranks 
ranks <- rank(murders$population)

# Define a variable ind to store the indexes needed to order the population values
ind <- order(murders$population)

# Create a data frame my_df with the state name and its rank and ordered from least populous to most 
my_df <- data.frame("states" = state[ind],"ranks" = ranks[ind])m