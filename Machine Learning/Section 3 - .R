dev.off() ## clean graphs 
rm(list=ls()) ## clean environment 
cat("\014") ## clean console 

library(dslabs)
library(tidyverse)
library(caret)
data(heights)

# 2.1  --------------------------------------------------------------------