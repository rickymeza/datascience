## Climate Change Assessment 


library(dslabs) ## Used in the data science course 
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gridExtra)
library(dplyr)
library(RColorBrewer)
library(psych)

data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

## question 2
carb <- temp_carbon$carbon_emissions
describe(carb)

temp_carbon %>%  filter(!is.na(carbon_emissions)) %>% select(year) %>% max()
temp_carbon %>%  filter(!is.na(carbon_emissions)) %>% select(year) %>% min()

temp_carbon %>% filter(year %in% c(1751,2014)) %>% 
  ggplot(aes(year,carbon_emissions)) + 
  geom_point()

## question 3

temp_max <- temp_carbon %>%  filter(!is.na(temp_anomaly)) %>% select(year) %>% max()
temp_min <- temp_carbon %>%  filter(!is.na(temp_anomaly)) %>% select(year) %>% min()

val_max <- temp_carbon %>% filter(year == temp_max) %>% .$temp_anomaly
val_min <- temp_carbon %>% filter(year == temp_min) %>% .$temp_anomaly

temp_min
temp_max
val_max - val_min

## question 4

p <- temp_carbon %>%  filter(!is.na(temp_anomaly)) %>% 
  ggplot(aes(year,temp_anomaly)) + 
  geom_line(col="red")

p <- p + geom_hline(aes(yintercept = 0), col = "blue")

p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue") +
  geom_line(aes(year,land_anomaly,col = "yellow")) + 
  geom_line(aes(year,ocean_anomaly,col = "blue"))

## question 8 

library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)
  
greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas ~ ., scales = "free") +
  geom_vline(aes(xintercept = 1850), col = "blue") +
  ylab("Concentration (CH4/N2O ppb, CO2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000 CE")

## question 10 

temp_carbon %>% 
  ggplot(aes(year,carbon_emissions)) +
  geom_line()

## question 11

co2_time <- historic_co2 %>%
  ggplot(aes(year, co2, color = source)) +
  geom_line() + 
  scale_x_continuous(limits = c(-3000, 2018)) 

co2_time
  