#######################################
####### ANALYSING SURVEYMONKEY DATA SET
#######################################

# R version 3.2.3 (2015-12-10)
#"/Applications/R/PhD_Research"


source("2017-04-25-Creating Variables.R")

##########################
######### PART 4 - ORDER 
##########################

levels(Survey$Mineral)

Survey$Mineral<-ordered(Survey$Mineral, levels=c("Gold", "Silver","Gold Silver","Silver Copper", "Gold Silver Copper", "Other") )



#######Changing the order of the levels (from "not risky" to "extremely risky")
str(Survey$Q11.Quality.Control)
table(Survey$Q11.Quality.Control)
# Extremely Risky Moderately Risky Not at all Risky   Slightly Risky       Very Risky 
# 19                1               34               14               32                6 



ha<-levels(Survey$Q11.Quality.Control)
O<-c("",ha[4],ha[5],ha[3],ha[6],ha[2])

Survey$Q11.Quality.Control<-ordered(Survey$Q11.Quality.Control,levels=O)
#checking to see if table still the same
table(Survey$Q11.Quality.Contro)
# Not at all Risky   Slightly Risky Moderately Risky       Very Risky  Extremely Risky 
# 19               14               32               34                6                1 
Survey$Q11.Schedule      <-ordered(Survey$Q11.Schedule       ,levels=O)
Survey$Q11.Hea.Saf.Env    <-ordered(Survey$Q11.Hea.Saf.Env    ,levels=O)
Survey$Q11.Cost.Control   <-ordered(Survey$Q11.Cost.Control   ,levels=O)
Survey$Q11.Dependency     <-ordered(Survey$Q11.Dependency     ,levels=O)
Survey$Q11.Coordinate     <-ordered(Survey$Q11.Coordinate     ,levels=O)
Survey$Q11.Confidentiality<-ordered(Survey$Q11.Confidentiality,levels=O)


table(Survey$Q10.Skill.Labour)
# Not at all Important   Slightly Important Moderately Important       Very Important 
# 19                   29                   14                   20                   13 
# Extremely Important 
# 11 

OO<-c("","Not at all Important","Slightly Important","Moderately Important","Very Important","Extremely Important")

Survey$Q10.Skill.Labour<-ordered(Survey$Q10.Skill.Labour,levels=OO)
table(Survey$Q10.Skill.Labour)
# Not at all Important   Slightly Important Moderately Important       Very Important 
# 19                   29                   14                   20                   13 
# Extremely Important 
# 11 
Survey$Q10.Flexibility <-ordered(Survey$Q10.Flexibility ,levels=OO)
Survey$Q10.Capit.Exp   <-ordered(Survey$Q10.Capit.Exp   ,levels=OO)
Survey$Q10.Core.Comp   <-ordered(Survey$Q10.Core.Comp   ,levels=OO)
Survey$Q10.Fixed.Cost  <-ordered(Survey$Q10.Fixed.Cost  ,levels=OO)
Survey$Q10.Community   <-ordered(Survey$Q10.Community   ,levels=OO)
Survey$Q10.Innovation  <-ordered(Survey$Q10.Innovation  ,levels=OO)
Survey$Q10.Suppliers   <-ordered(Survey$Q10.Suppliers   ,levels=OO)
Survey$Q10.Unions      <-ordered(Survey$Q10.Unions      ,levels=OO)

