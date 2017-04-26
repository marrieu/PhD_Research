#######################################
####### ANALYSING SURVEYMONKEY DATA SET
#######################################

# R version 3.2.3 (2015-12-10)
#"/Applications/R/PhD_Research"

##Library
library(stringr)
library(ggplot2)
library(dplyr)
#pipe %>%

####Getting the data set
Raw_Survey<-read.csv(file.choose(),header=T)
