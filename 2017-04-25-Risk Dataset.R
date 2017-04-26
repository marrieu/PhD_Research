#######################################
####### ANALYSING SURVEYMONKEY DATA SET
#######################################

# R version 3.2.3 (2015-12-10)
#"/Applications/R/PhD_Research"


source("2017-04-25-Group By.R")

####################################################
####################################################
############# PART 8 - CREATING RISK DATA SET
####################################################
####################################################



Survey_Risk<-select(Survey,c(Company,
                             Q5.Location,
                             Q6.Employee, 
                             size,
                             Mineral, 
                             Position2,
                             Exploration,
                             Q7.Outsourcing,
                             Q12.More.Outsourcing,
                             Q8.Drill.Blast,
                             Q8.Load.Haul,
                             Q11.Coordinate,
                             Q11.Schedule,
                             Q11.Cost.Control,
                             Q11.Quality.Control,
                             Q11.Delays.Disr,
                             Q11.Confidentiality,
                             Q11.Hea.Saf.Env,
                             Q11.Dependency))

colnames(Survey_Risk)<-c("Company",
                         "Location",
                         "Num_Employee", 
                         "Size",
                         "Mineral", 
                         "Position",
                         "Exploration",
                         "Outsourcing",
                         "More_Outsourcing",
                         "Mining_Drill.Blast",
                         "Mining_Load.Haul",
                         "Risk_Coordinate",
                         "Risk_Schedule",
                         "Risk_Cost.Control",
                         "Risk_Quality.Control",
                         "Risk_Delays.Disr",
                         "Risk_Confidentiality",
                         "Risk_Hea.Saf.Env",
                         "Risk_Dependency")


###Transforming "Yes" "No" variable into binary 0 1  
str(Survey_Risk$Outsourcing)
levels(Survey_Risk$Outsourcing)<-c(NA,"0","1")
Survey_Risk$Outsourcing<-as.numeric(as.character(Survey_Risk$Outsourcing))

str(Survey_Risk$More_Outsourcing)
levels(Survey_Risk$More_Outsourcing)<-c(NA,"0","1")
Survey_Risk$More_Outsourcing<-as.numeric(as.character(Survey_Risk$More_Outsourcing))


str(Survey_Risk$Mining_Drill.Blast)
levels(Survey_Risk$Mining_Drill.Blast)<-c("0","1")
Survey_Risk$Mining_Drill.Blast<-as.numeric(as.character(Survey_Risk$Mining_Drill.Blast))

str(Survey_Risk$Mining_Load.Haul)
levels(Survey_Risk$Mining_Load.Haul)<-c("0","1")
Survey_Risk$Mining_Load.Haul<-as.numeric(as.character(Survey_Risk$Mining_Load.Haul))


str(Survey_Risk$Risk_Coordinate)
levels(Survey_Risk$Risk_Coordinate)


O2<-c(NA,"0","1","2","3","4")

Risk_Quality.Control<-Survey_Risk$Risk_Quality.Control
levels(Risk_Quality.Control)<-O2

Risk_Quality.Control<-as.numeric(as.character(Risk_Quality.Control))



Survey_Risk$Risk_Quality.Control<-ordered(Survey_Risk$Risk_Quality.Control,levels=O2)
#checking to see if table still the same
table(Survey_Risk$Risk_Quality.Control)
# Not at all Risky   Slightly Risky Moderately Risky       Very Risky  Extremely Risky 
# 19               14               32               34                6                1 
Survey_Risk$Risk_Schedule       <-ordered(Survey_Risk$Risk_Schedule       ,levels=O)
Survey_Risk$Risk_Hea.Saf.Env    <-ordered(Survey_Risk$Risk_Hea.Saf.Env    ,levels=O)
Survey_Risk$Risk_Cost.Control   <-ordered(Survey_Risk$Risk_Cost.Control   ,levels=O)
Survey_Risk$Risk_Dependency     <-ordered(Survey_Risk$Risk_Dependency     ,levels=O)
Survey_Risk$Risk_Coordinate     <-ordered(Survey_Risk$Risk_Coordinate     ,levels=O)
Survey_Risk$Risk_Confidentiality<-ordered(Survey_Risk$Risk_Confidentiality,levels=O)

##CReating a  numeric vector from 0 (not risk at all) to 5
Survey_Risk$Risk_Schedule_Num       <-Survey_Risk$Risk_Schedule       
Survey_Risk$Risk_Hea.Saf.Env_Num    <-Survey_Risk$Risk_Hea.Saf.Env    
Survey_Risk$Risk_Cost.Control_Num   <-Survey_Risk$Risk_Cost.Control   
Survey_Risk$Risk_Dependency_Num     <-Survey_Risk$Risk_Dependency     
Survey_Risk$Risk_Coordinate_Num     <-Survey_Risk$Risk_Coordinate     
Survey_Risk$Risk_Confidentiality_Num<-Survey_Risk$Risk_Confidentiality

O2<-c(NA,"0","1","2","3","4")

levels(Survey_Risk$Risk_Schedule_Num       )<-O2
levels(Survey_Risk$Risk_Hea.Saf.Env_Num    )<-O2
levels(Survey_Risk$Risk_Cost.Control_Num   )<-O2
levels(Survey_Risk$Risk_Dependency_Num     )<-O2
levels(Survey_Risk$Risk_Coordinate_Num     )<-O2
levels(Survey_Risk$Risk_Confidentiality_Num)<-O2

Survey_Risk$Risk_Schedule_Num       <-as.numeric(as.character(Survey_Risk$Risk_Schedule_Num       ))
Survey_Risk$Risk_Hea.Saf.Env_Num    <-as.numeric(as.character(Survey_Risk$Risk_Hea.Saf.Env_Num    ))
Survey_Risk$Risk_Cost.Control_Num   <-as.numeric(as.character(Survey_Risk$Risk_Cost.Control_Num   ))
Survey_Risk$Risk_Dependency_Num     <-as.numeric(as.character(Survey_Risk$Risk_Dependency_Num     ))
Survey_Risk$Risk_Coordinate_Num     <-as.numeric(as.character(Survey_Risk$Risk_Coordinate_Num     ))
Survey_Risk$Risk_Confidentiality_Num<-as.numeric(as.character(Survey_Risk$Risk_Confidentiality_Num))

levels(Survey_Risk$Num_Employee)

Survey_Risk$Num_Employee<-ordered(Survey_Risk$Num_Employee ,levels=c("","Less than 10","10-50","50-100","100-500","500-1000","More than 1000"))


write.csv(Survey_Risk, file = "Survey_Risk.csv")





