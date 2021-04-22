## Probability Assessment Section 1

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

set.seed(1)

## Question 1: Olympic running


perm <- permutations(8,3)    # order matters
perm
com <- combinations(8,3)    # order does not matter
com

perm <- permutations(3,3)
perm

jam <- (3/8)*(2/7)*(1/6)
jam

runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")

B <- 10000

result <- replicate(B, {
    b_win <- sample(runners,3, replace = FALSE)
    all_jam <- sum(b_win %in% "Jamaica") == 3
  })

mean(result)

## Question 2: Restaurant management

entree <- 6 
side <- 6
drink <- 2

sideCom <- nrow(combinations(6,2))

mealCom <- entree*sideCom*drink
mealCom

drink <- drink +1 

mealCom <- entree*sideCom*drink
mealCom

sideCom <- nrow(combinations(6,3))

mealCom <- entree*sideCom*drink
mealCom

entree <- 1:12

sideCom <- nrow(combinations(6,2))

f <- function(entree){
  print(drink*sideCom*entree)
}

sapply(entree,f)

## 2e 

sideOpt <- 2:12
entree <- 6
drink <- 3

sideF <- function(sides){
  print(drink*entree*nrow(combinations(sides,2)))
}

sapply(sideOpt, sideF)

# Questions 3 and 4: Esophageal cancer and alcohol/tobacco use, part 1 ------

head(esoph)

nrow(esoph)

all_cases <- sum(esoph$ncases)
all_cases

all_controls <- sum(esoph$ncontrols)
all_controls

data <- esoph

esoph %>% filter(alcgp == "120+") %>%
  summarize(sum_cases=sum(ncases), tot=sum(ncontrols) + sum(ncases), probability=sum_cases/tot)

esoph %>% filter(alcgp == "0-39g/day") %>%
  summarize(sum_cases=sum(ncases), tot=sum(ncontrols)+sum(ncases), probability=sum_cases/tot)

esoph %>% summarize(tot_cases = sum(ncases))
esoph %>% filter(tobgp != "0-9g/day") %>%
  summarize(smoking10_cases = sum(ncases))

122/200

esoph %>% summarize(tot_cases = sum(ncontrols))
esoph %>% filter(tobgp != "0-9g/day") %>%
  summarize(smoking10_cases = sum(ncontrols))
450/975

## Questions 5 and 6: Esophageal cancer and alcohol/tobacco use, part 2 --------

####### QUESTION 5a/b/c/d #######
# For cases, what is the probability of being in the highest alcohol group?
esoph %>% filter(alcgp == "120+") %>%
  summarize(sum_cases=sum(ncases))
45/all_cases

# For cases, what is the probability of being in the highest tobacco group?
esoph %>% filter(tobgp == "30+") %>%
  summarize(sum_cases=sum(ncases))
31/all_cases

# For cases, what is the probability of being in the highest alcohol group and
#  the highest tobacco group?
esoph %>% filter(alcgp == "120+" & tobgp =="30+") %>%
  summarize(sum_cases = sum(ncases))
10/all_cases

# For cases, what is the probability of being in the highest alcohol group or
#  the highest tobacco group?
esoph %>% filter(alcgp == "120+" | tobgp =="30+") %>%
  summarize(sum_cases = sum(ncases))
66/all_cases


####### QUESTION 6a/b/c/d/e/f #######
# For controls, what is the probability of being in the highest alcohol group?
esoph %>% filter(alcgp == "120+") %>%
  summarize(contr_sum = sum(ncontrols), probability = contr_sum/all_controls)

# How many times more likely are cases than controls to be in the highest alcohol
#  group?
esoph %>% filter(alcgp == "120+") %>%
  summarize(contr_sum = sum(ncontrols), case_sum = sum(ncases),
            co_prob = contr_sum/all_controls, ca_prob = case_sum/all_cases,
            ratio = ca_prob/co_prob)

# For controls, what is the probability of being in the highest tobacco group?
esoph %>% filter(tobgp == "30+") %>%
  summarize(contr_sum = sum(ncontrols), probability = contr_sum/all_controls)

# For controls, what is the probability of being in the highest alcohol group and
#  the highest tobacco group?
esoph %>% filter(tobgp == "30+" & alcgp == "120+") %>%
  summarize(contr_sum = sum(ncontrols), probability = contr_sum/all_controls)

# For controls, what is the probability of being in the highest alcohol group or
#  the highest tobacco group?
esoph %>% filter(tobgp == "30+" | alcgp == "120+") %>%
  summarize(contr_sum = sum(ncontrols), probability = contr_sum/all_controls)

# How many times more likely are cases than controls to be in the highest alcohol
#  group or the highest tobacco group?
esoph %>% filter(alcgp == "120+" | tobgp == "30+") %>%
  summarize(contr_sum = sum(ncontrols), case_sum = sum(ncases),
            co_prob = contr_sum/all_controls, ca_prob = case_sum/all_cases,
            ratio = ca_prob/co_prob)




