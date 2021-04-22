options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

age_distribution <- titanic %>% ggplot(aes(Age,color = Sex,y = ..count..)) +
  geom_density()

age_distribution <- titanic %>% ggplot(aes(Age,color = Sex,y = ..count..)) +
  geom_density()

age_distribution

age_boxplot <- titanic %>% ggplot(aes(Sex,Age)) +
  geom_boxplot()

age_boxplot

## Question 3 QQ-plot
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

p <- titanic %>% filter(!is.na(Age)) %>%
  ggplot(aes(sample = Age))
p + geom_qq(dparams = params) + geom_abline()


## Question 4 Survived 

Plot_survived <- titanic %>% ggplot(aes(Survived,color = Sex, fill = Sex)) +
  geom_bar()
Plot_survived

## Question 5

age_sur_distribution <- titanic %>% ggplot(aes(Age,y = ..count.., fill = Survived)) +
  geom_density()

age_sur_distribution

## Question 6 

fare_survival <- titanic %>% filter(Fare > 0) %>% 
  ggplot(aes(Fare, Survived, group = Survived)) +
  scale_x_continuous(trans = "log2") +
  geom_boxplot() +   
  geom_jitter(width = 0.1, alpha = 0.2) 

fare_survival
                                                         

## Question 7

first <- titanic %>% 
  ggplot(aes(Pclass,fill = Survived)) + 
  geom_bar()

first

second <- titanic %>% 
  ggplot(aes(Pclass,fill = Survived)) + 
  geom_bar(position = position_fill())

second

third <- titanic %>% 
  ggplot(aes(Survived,fill = Pclass)) + 
  geom_bar(position = position_fill())

third

## Question 8

grid <- titanic %>% 
  ggplot((aes(Age, fill = Survived,y = ..count..)))+
  geom_density(position = "stack") + 
  facet_grid(Sex ~ Pclass)

grid



