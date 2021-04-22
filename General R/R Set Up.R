## Different areas to set up the R workplace 



# Workplace Cleanup -------------------------------------------------------

dev.off() ## clean graphs 
rm(list=ls()) ## clean environment 
cat("\014") ## clean console 


# Package installation ----------------------------------------------------


install.packages("dslabs")
install.packages("tidyverse")
install.packages("ggplot2")



# Loading packages --------------------------------------------------------



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
library(lubridate)
library(rvest)
library(tidytext)
library(pdftools)
library(HistData)
library(broom)
library(Lahman)
library(dslabs)
library(reshape2)
library(lpSolve)


# Set working directory ---------------------------------------------------

setwd("~/Documents/Data Science Course/Machine Learning")



