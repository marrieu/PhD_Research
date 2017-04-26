#######################################
####### ANALYSING SURVEYMONKEY DATA SET
#######################################

# R version 3.2.3 (2015-12-10)
#"/Applications/R/PhD_Research"


source("2017-04-25-Exporting Data.R")


##############################
##### PART 6 - GROUP BY
#############################

str(Survey)

Survey_group<-Survey %>%
  group_by(Q7.Outsourcing)%>%
  summarise(mean=mean(Q13.Eff.Mutual.Benef,na.rm =TRUE),count=n()) %>%
  arrange(Q7.Outsourcing)


###Grouping by size and creating data set to see what activities companies are outsourcing 
Survey_group_size<-Survey %>%
  group_by(size)%>%
  summarise(Q8.Expl.Serv =sum(Q8.Expl.Serv != ""), 
            Q8.Const.Serv =sum(Q8.Const.Serv != ""),
            Q8.Drill.Blast =sum(Q8.Drill.Blast != ""),
            Q8.Load.Haul =sum(Q8.Load.Haul != ""),
            Q8.Tunnel =sum(Q8.Tunnel != ""),
            Q8.Crush.Grind.Flot =sum(Q8.Crush.Grind.Flot != ""),
            Q8.Leach =sum(Q8.Leach != ""),
            Q8.Concent =sum(Q8.Concent != ""),
            Q8.Waste.Dump =sum(Q8.Waste.Dump != ""),
            Q8.Tail.Dump =sum(Q8.Tail.Dump != "")) 


