dev.off() ## clean graphs 
rm(list=ls()) ## clean environment 
cat("\014") ## clean console 

library(dslabs)
library(tidyverse)

data(heights)

class(heights)

class(heights$sex)

class(heights$height)

nrow(heights)

heights$height[777]

max(heights$height)

which.min(heights$height)

mean(heights$height)

median(heights$height)

taller <- heights %>% filter(height > 78 & sex == "Female")
nrow(taller)

mean(heights$sex == "Male")
summary(heights)
