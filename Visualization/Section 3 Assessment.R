## Visualization - Section 3 Assessment 

library(dslabs) ## Used in the data science course 
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gridExtra)
library(dplyr)
library(NHANES)

data(NHANES)

library(dslabs)
data(na_example)
mean(na_example)
sd(na_example)

mean(na_example, na.rm = TRUE)
sd(na_example, na.rm = TRUE)

tab <- NHANES %>% filter(AgeDecade == " 20-29" & Gender  == "female") 

ref <- NHANES %>% filter(AgeDecade == " 20-29" & Gender  == "female") %>% summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve , na.rm = TRUE))
ref

ref_avg <- NHANES %>%
  filter(AgeDecade == " 20-29" & Gender == "female") %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), 
            standard_deviation = sd(BPSysAve, na.rm=TRUE)) %>% .$average

NHANES %>% filter(AgeDecade == " 20-29" & Gender  == "female") %>% summarize(minbp = min(BPSysAve, na.rm=TRUE), maxbp = max(BPSysAve, na.rm=TRUE))

NHANES %>% group_by(AgeDecade, Gender) %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), 
            standard_deviation = sd(BPSysAve, na.rm=TRUE)) 

NHANES %>% filter(AgeDecade == " 40-49" & Gender  == "male") %>% group_by(Race1) %>% summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm=TRUE)) %>% arrange(average)




