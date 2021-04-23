## Different areas to set up the R workplace 



# Workplace Cleanup -------------------------------------------------------

dev.off() ## clean graphs 
rm(list=ls()) ## clean environment 
cat("\014") ## clean console 


# Package installation ----------------------------------------------------

install.packages("remotes")
install.packages("usethis")
install.packages("devtools")
install.packages("dslabs")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggrepel")
install.packages("ggthemes")
install.packages("gridExtra")
install.packages("dplyr")
install.packages("RColorBrewer")
install.packages("psych")
install.packages("readr")
install.packages("gtools")
install.packages("lubridate")
install.packages("rvest")
install.packages("tidytext")
install.packages("pdftools")
install.packages("HistData")
install.packages("broom")
install.packages("Lahman")
install.packages("dslabs")
install.packages("reshape2")
install.packages("lpSolve")



# Loading packages --------------------------------------------------------


library(remotes)
library(usethis)
library(devtools)
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



