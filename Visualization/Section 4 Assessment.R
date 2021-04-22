library(dslabs) ## Used in the data science course 
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gridExtra)
library(dplyr)

data(gapminder)
head(gapminder)

## fill out the missing parts in filter and aes
gapminder %>% filter( year ==2012 & continent == "Africa" ) %>%
  ggplot(aes( fertility, life_expectancy, color = region)) +
  geom_point()

df <- gapminder %>% filter( year ==2012 & continent == "Africa" & fertility <= 3 & life_expectancy >= 70) %>% select(country,region)

## vietnam ward question

tab <- gapminder %>% filter( year >= 1960 & year <= 2010 & country %in% c("United States", "Vietnam"))

p <- tab %>% 
  ggplot(aes(year, life_expectancy, color = country)) + 
  geom_line()

## cambodia

gapminder %>% filter( year >= 1960 & year <= 2010 & country == "Cambodia") %>% 
  ggplot(aes(year, life_expectancy)) + 
  geom_line()

## dollars per day 

daydollars <- gapminder %>% filter( year ==2010 & continent == "Africa" & !is.na(gdp)) %>% mutate(dollars_per_day = gdp/population/365)

daydollars %>%
  ggplot(aes(dollars_per_day)) +
  scale_x_continuous(trans = "log2") +
  geom_density()

## two years

daydollars <- gapminder %>% filter( year %in% c(1970, 2010) & continent == "Africa" & !is.na(gdp)) %>% mutate(dollars_per_day = gdp/population/365)

daydollars %>%
  ggplot(aes(dollars_per_day)) +
  scale_x_continuous(trans = "log2") +
  geom_density() +
  facet_grid(. ~ year)

## stacked 

daydollars <- gapminder %>% filter( year %in% c(1970, 2010) & continent == "Africa" & !is.na(gdp)) %>% mutate(dollars_per_day = gdp/population/365)

daydollars %>%
  ggplot(aes(dollars_per_day, fill = region)) +
  scale_x_continuous(trans = "log2") +
  geom_density(bw = 0.5,position = "stack") +
  facet_grid(. ~ year) + 
  theme_classic()

## 11 & 12

gapminder_Africa_2010 <- gapminder %>% filter( year ==  2010 & continent == "Africa" & !is.na(gdp)) %>% mutate(dollars_per_day = gdp/population/365)

gapminder_Africa_2010 %>%
  ggplot(aes(dollars_per_day,infant_mortality, color = region)) + 
  scale_x_continuous(trans = "log2") +
  geom_point()

gapminder_Africa_2010 %>%
  ggplot(aes(dollars_per_day,infant_mortality, color = region, label = country)) + 
  scale_x_continuous(trans = "log2") +
  geom_point() + 
  geom_text() + 
  theme_fivethirtyeight()

# 14 

daydollars <- gapminder %>% filter( year %in% c(1970, 2010) & continent == "Africa" & !is.na(gdp) &!is.na(infant_mortality)) %>% mutate(dollars_per_day = gdp/population/365)

daydollars %>%
  ggplot(aes(dollars_per_day, infant_mortality, color = region,label = country)) +
  scale_x_continuous(trans = "log2") +
  geom_point() + 
  geom_text() +
  facet_grid( year ~ .) + 
  theme_wsj()
 

