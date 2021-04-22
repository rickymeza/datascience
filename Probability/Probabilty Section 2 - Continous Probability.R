## Probability Section 2: Continuous Probability -----

library(dslabs) ## Used in the data science course 
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gridExtra)
library(dplyr)
library(RColorBrewer)
library(psych)
library(readr)
library(gtools)

## Define x as male heights from the dslabs heights dataset:
  
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

## Given a vector x, we can define a function for computing the CDF of x using:
  
F <- function(a) mean(x <= a)
1 - F(70)    # probability of male taller than 70 inches

## Code: Using pnorm() to calculate probabilities

## Given male heights x:
  
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

## We can estimate the probability that a male is taller than 70.5 inches using:
  
  1 - pnorm(70.5, mean(x), sd(x))

## Code: Discretization and the normal approximation

# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))

## Plotting the Probability Density

x <- seq(-4, 4, length = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()

## dnorm(z, mu, sigma)

## Code: Generating normally distributed random numbers

# define x as male heights from dslabs data
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

# generate simulated height data using normal distribution - both datasets should have n observations
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s)

# plot distribution of simulated_heights
data.frame(simulated_heights = simulated_heights) %>%
  ggplot(aes(simulated_heights)) +
  geom_histogram(color="black", binwidth = 2)

## Code: Monte Carlo simulation of tallest person over 7 feet

B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, s)    # generate 800 normally distributed random heights
  max(simulated_data)    # determine the tallest height
})
mean(tallest >= 7*12)    # proportion of times that tallest person ex

## Code: Plotting the normal distribution with dnorm

## Use d to plot the density function of a continuous distribution. Here is the density function for the normal distribution (abbreviation norm()):
  
  x <- seq(-4, 4, length.out = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x,f)) +
  geom_line()