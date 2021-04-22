## Section 2 Assessment 

library(dslabs) ## Used in the data science course 
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(dplyr)
library(gridExtra)

data(heights)
data(murders)
p <- ggplot(murders)

class(p)

p <- heights %>% ggplot(aes(x = height))

murders %>% ggplot(aes(x = population, y = total)) +
  geom_point()

murders %>% ggplot(aes(x = population, y = total, label = abb, color = region)) +
  geom_label()

## adding the plot properties to p 
p <- murders %>% ggplot(aes(population, total, label = abb, color = region)) + geom_label()

## adding layes of log 10 for the axis 
p + scale_x_log10() + 
  scale_y_log10() + 
  ggtitle("Gun murder data")

## histogram with heights 
p <- heights %>% ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1)

# density plot with heights
heights %>% 
  ggplot(aes(height)) + 
  geom_density()

# density plot with heights group by sex
heights %>% 
  ggplot(aes(height, group = sex)) + 
  geom_density()

# density plot with heights group and colored by sex
heights %>% 
  ggplot(aes(height, group = sex, color = sex)) + 
  geom_density()

# density plot with heights group and filled by sex
# alpha affects the intensity of the color 
heights %>% 
  ggplot(aes(height, fill = sex)) + 
  geom_density(alpha = .8 )

