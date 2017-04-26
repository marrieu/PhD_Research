#######################################
####### ANALYSING SURVEYMONKEY DATA SET
#######################################

# R version 3.2.3 (2015-12-10)
#"/Applications/R/PhD_Research"


source("2017-04-25-Order Variables.R")

#######################################
####### PART 5-EXPORTING DATA
#######################################

###exporting data
write.csv(Survey, file = "Survey.csv")

##########Data set with only outsourcing companies
Survey_outs<-subset(Survey,Q7.Outsourcing = "Yes")
##########Data set with only mining companies
Survey_nonExp<-subset(Survey,Exploration != 1)
