library(tidyverse)
library(dslabs)
library(psych)
data(stars)
options(digits = 3)

mag <- stars$magnitude

## Question 1

describe(mag)

## Question 2

stars %>% ggplot(aes(magnitude)) +
  geom_density()

## Question 3

stars %>% ggplot(aes(temp)) +
  geom_density()

## Question 4

stars %>% ggplot(aes(temp,magnitude)) +
  geom_point()

## Question 5

stars %>% ggplot(aes(temp,magnitude)) +
  geom_point() + 
  scale_x_continuous(trans = "log10") +
  scale_y_reverse() +
  scale_x_reverse()


## Question 6

stars %>%
  ggplot(aes(log10(temp), magnitude, color = type)) +
  geom_point() +
  scale_x_reverse() +
  scale_y_reverse() 

## Question 7

## Question 8

## Question 9