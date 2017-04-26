#######################################
####### ANALYSING SURVEYMONKEY DATA SET
#######################################

# R version 3.2.3 (2015-12-10)
#"/Applications/R/PhD_Research"


source("2017-04-25-Rename Variables.R")

#######################################
####### PART 3 - CREATING VARIABLES
#######################################

#######################################
####Creating variable for exploration companies
#######################################

##checking if there is any duplicated name
length(unique(Survey$ID))==nrow(Survey)


####find if they have the word "exploration" in any answer 
Exploration<-subset(Survey,str_detect(Q4.Comments,"xploration") | str_detect(Q14.Comments,"xploration")|str_detect(Q12.Comments,"xploration")| str_detect(Q23.Comments,"xploration"),select=ID)

##Create the variable exploration indicating if it is an exploration company
Survey$Exploration<-ifelse(Survey$ID %in% Exploration$ID,1,0)

#######################################
####Creating variable for CEO and CFO
#######################################


CEO<-subset(Survey,str_detect(Position,paste(c("CEO","resident","xecutive"), collapse = '|')),select=ID)
COO<-subset(Survey,str_detect(Position,paste(c("COO","perating"), collapse = '|')),select=ID)
Manager<-subset(Survey,str_detect(Position,"Manag"),select=ID)


Survey$Position<-as.character(Survey$Position)

##Creating the variable CEO, COO or other
Survey$Position2<-ifelse(Survey$ID %in% CEO$ID,"CEO",ifelse(Survey$ID %in% COO$ID,"COO",ifelse(Survey$ID %in% Manager$ID,"Manager","Other")))
table(Survey$Position2)

#######################################
####Creating variable Size
#######################################

table(Survey$Q6.Employee)

#10-50        100-500         50-100       500-1000   Less than 10 More than 1000 
#2             13             13              2              7             23             26 

#creating variable size with 2 categories 

#cut
Survey$size<-ifelse(Survey$Q6.Employee %in% c("10-50","100-500","50-100"),"Small", ifelse(Survey$Q6.Employee == "","","Large"))

#######################################
####Creating variable Mineral
#######################################

Survey$Mineral<-ifelse(Survey$Q4.Gold != "",
                       ifelse(Survey$Q4.Silver != "",ifelse(Survey$Q4.Copper != "", "Gold Silver Copper","Gold Silver"),"Gold"),
                       ifelse(Survey$Q4.Silver != "",ifelse(Survey$Q4.Copper != "","Silver Copper","Silver"),"Other"))

str(Survey$Mineral)
table(Survey$Mineral)

# Gold        Gold Silver Gold Silver Copper              Other             Silver 
# 31                 16                 18                 37                  3 
# Silver Copper 
# 1 

Survey$Mineral<-as.factor(Survey$Mineral)

