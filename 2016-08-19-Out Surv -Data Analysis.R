
#######################################
####### ANALYSING SURVEYMONKEY DATA SET
#######################################

# R version 3.2.3 (2015-12-10)
# dir "/Applications/R/Outsorcing Survey"

##Library
library(stringr)
library(ggplot2)
library(dplyr)
#pipe %>%

####Getting the data set
Raw_Survey<-read.csv(file.choose(),header=T)


#get the original column name
col<-colnames(Raw_Survey)


#reduce the size of the colum name
col_new<-str_sub(colnames(Raw_Survey),-20)

#rename column with the small sized set
Survey<-Raw_Survey
colnames(Survey) <- col_new

#Drop some useless variable
#Survey<-select(Survey,-c(CollectorID, StartDate,EndDate, IP.Address, Email.Address, First.Name,LastName, Custom.Data))

Survey<-Survey[,-(2:9)]

#Rename all the variables
col_final<-c("ID", 
             "Name", 
             "Company",
             "Position",
             "Q4.Gold",
             "Q4.Silver",
             "Q4.Copper",
             "Q4.Iron",
             "Q4.Coal",
             "Q4.Diamond",
             "Q4.Lead.Zinc",
             "Q4.Potash",
             "Q4.Comments",
             "Q5.Location",
             "Q6.Employee",
             "Q7.Outsourcing",
             "Q8.Expl.Serv",
             "Q8.Const.Serv",
             "Q8.Drill.Blast",
             "Q8.Load.Haul",
             "Q8.Tunnel",
             "Q8.Crush.Grind.Flot",
             "Q8.Leach","Q8.Concent",
             "Q8.Waste.Dump",
             "Q8.Tail.Dump",
             "Q8.Others",
             "Q9.Assets.Equip",
             "Q10.Skill.Labour",
             "Q10.Flexibility",
             "Q10.Capit.Exp",
             "Q10.Core.Comp",
             "Q10.Fixed.Cost",
             "Q10.Community",
             "Q10.Innovation",
             "Q10.Suppliers",
             "Q10.Unions",
             "Q10.Comments",
             "Q11.Quality.Control",
             "Q11.Delays.Disr",
             "Q11.Schedule",
             "Q11.Hea.Saf.Env",
             "Q11.Cost.Control",
             "Q11.Dependency",
             "Q11.Coordinate",
             "Q11.Confidentiality",
             "Q11.Comments",
             "Q12.More.Outsourcing",
             "Q12.Comments",
             "Q13.Eff.Mutual.Benef",
             "Q13.Eff.Incentive",
             "Q13.Eff.Contr.Flex",
             "Q13.Eff.Reporting",
             "Q13.Eff.Stand.Oper.Proc",
             "Q13.Eff.Form.Respons",
             "Q13.Eff.Clear.Goal",
             "Q13.Eff.Contr.Manag",
             "Q13.Eff.Recov.Plan",
             "Q13.Eff.Partnership",
             "Q13.Eff.Communication",
             "Q13.Eff.Sen.Manag.Supp",
             "Q13.Val.Mutual.Benef",
             "Q13.Val.Incentive",
             "Q13.Val.Contr.Flex",
             "Q13.Val.Reporting",
             "Q13.Val.Stand.Oper.Proc",
             "Q13.Val.Form.Respons",
             "Q13.Val.Clear.Goal",
             "Q13.Val.Contr.Manag",
             "Q13.Val.Recov.Plan",
             "Q13.Val.Partnership",
             "Q13.Val.Communication",
             "Q13.Val.Sen.Manag.Supp",
             "Q14.Comments",
             "Q15.Min.Core.Comp",
             "Q16.Intel.or.Physical",
             "Q17.Creat.or.Inhib",
             "Q18.Top.3.Cap",
             "Q19.Tech.Better",
             "Q20.More.Effic",
             "Q21.Comp.Adv",
             "Q22.Peopl.or.Syst",
             "Q23.Comments",
             "Q24.May.Contact", 
             "Phone")

length(col_final)
dim(Survey)

#rename variable
colnames(Survey) <- col_final

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

##########################
######### ORDER ##########
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

  
###exporting data
write.csv(Survey, file = "Survey.csv")

##########Data set with only outsourcing companies
Survey_outs<-subset(Survey,Q7.Outsourcing = "Yes")
##########Data set with only mining companies
Survey_nonExp<-subset(Survey,Exploration != 1)


##############################
#####SQL
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




##############################
#####GRAPH 
#############################

library(ggplot2)

#######################################
######FUNCTION TO CREATE GRAPH BAR
#######################################

create_graph_bar<-function(var_x,var_fill=NA,var_grid=NA,posit="stack",name_label=var_fill){
  
var_aux<-ifelse(is.na(var_fill),var_x,var_fill)  
  
  a<-ggplot(subset(Survey,get(var_aux) != ""))+
    aes_string(x=var_x,fill=var_fill)+
    geom_bar(position=posit)+
    facet_grid(as.formula(paste(". ~", var_grid)))+
    scale_fill_discrete(name=name_label)
  
  print(a)
  
}
#posit defines style of the graph => "stack"= normal, "fill"=scale all same, "dodge"=one bar beside other



#######################################
######FUNCTION TO CREATE GRAPH HISTOGRAN
#######################################
create_graph_hist<-function(var_x,var_fill=NA,var_grid=NA){
  
  a<-ggplot(subset(Survey,get(var_fill) != ""))+
    aes_string(x=as.name(var_x),fill=as.name(var_fill))+
    geom_hist()+
    facet_grid(as.formula(paste(". ~", var_grid)))
  
  print(a)
  
  
}


###############################
####Analysing who is OUTSOURCING
#################################
create_graph_bar("size","Q7.Outsourcing")
create_graph_bar("Q6.Employee","Q7.Outsourcing")
create_graph_bar("Mineral","Q7.Outsourcing")
create_graph_bar("Exploration","Q7.Outsourcing")



create_graph_bar("size","Q7.Outsourcing","Exploration")
create_graph_bar("Mineral","Q7.Outsourcing","size")
create_graph_bar("Mineral","Q7.Outsourcing","Exploration")
create_graph_bar("size","Q7.Outsourcing","Mineral")
create_graph_bar("Q6.Employee","Q7.Outsourcing","Exploration")
create_graph_bar("Q6.Employee","Q7.Outsourcing","Exploration","fill")

#########
#graph of the process being outsourced divided by size
#########

ggplot(subset(Survey,size != ""))+ geom_bar(aes(Q8.Expl.Serv))+
  geom_bar(aes(Q8.Const.Serv))+
  geom_bar(aes(Q8.Drill.Blast))+
  geom_bar(aes(Q8.Load.Haul))+
  geom_bar(aes(Q8.Tunnel))+
  geom_bar(aes(Q8.Crush.Grind.Flot))+
  geom_bar(aes(Q8.Leach))+
  geom_bar(aes(Q8.Concent))+
  geom_bar(aes(Q8.Waste.Dump))+
  geom_bar(aes(Q8.Tail.Dump))+
  facet_grid(.~size)+
  theme(axis.text = element_text(angle = 90))+aes(fill=factor(Exploration))



###### Mains concern when outsourcing 
create_graph_bar("size","Q10.Skill.Labour","Q7.Outsourcing")
create_graph_bar("size","Q10.Flexibility")
create_graph_bar("size","Q10.Capit.Exp")
create_graph_bar("size","Q10.Core.Comp")
create_graph_bar("size","Q10.Fixed.Cost")
create_graph_bar("size","Q10.Community")
create_graph_bar("size","Q10.Innovation")
create_graph_bar("size","Q10.Suppliers")
create_graph_bar("size","Q10.Unions")


create_graph_bar("Q10.Unions")


##############
####Position
##############
###Do you believe that mining and mineral processingare core competencies for your company asowner operated functions?
ggplot(subset(Survey,Q15.Min.Core.Comp != ""))+aes(x=Position2,fill=factor(Q15.Min.Core.Comp))+geom_bar()+scale_fill_manual("Is mining core?",values=c("lightblue","orange"))+labs(x="Position",title="Position x Core")
create_graph_bar("Position2","Q15.Min.Core.Comp",posit="fill")
create_graph_bar("size","Q15.Min.Core.Comp")
create_graph_bar("Position2","Q15.Min.Core.Comp","size")

#Are the mining and mineral processing activities in your company based on (i) Skills sets and intellectual property or (ii) Physical Assets?
create_graph_bar("Position2","Q16.Intel.or.Physical")
create_graph_bar("size","Q16.Intel.or.Physical")
create_graph_bar("Position2","Q16.Intel.or.Physical","size")

#Would you classify your company's mining and mineral processing activities as (i) creating flexibility or (ii) inhibiting flexibility?
create_graph_bar("Position2","Q17.Creat.or.Inhib")
create_graph_bar("size","Q17.Creat.or.Inhib")

#Do you believe that the capability of your company toperform mining and mineral processing activities internally is one of the top 3 corporate capabilities in the company?
create_graph_bar("Position2","Q18.Top.3.Cap")

#Is your company technically better positioned to conduct owner-operated mining and mineral processing activities than potential service providers?
create_graph_bar("Position2","Q19.Tech.Better")

#Is your company more efficient and effective at mining and mineral processing than its industry peers?
create_graph_bar("Position2","Q20.More.Effic")

#Do your company's owner-operated mining and mineral processing activities provide your company with a competitive advantage over its industry peers in the larger commodity markets?
create_graph_bar("Position2","Q21.Comp.Adv")

#Is your company’s technical competence currently predominantly based on (i) people and skills or (ii) systems and processes?
create_graph_bar("Position2","Q22.Peopl.or.Syst","size")




####Risk Perception

create_graph_bar("Position2","Q11.Quality.Control","size","fill")
create_graph_bar("Position2","Q11.Quality.Control","Q7.Outsourcing")

create_graph_bar("Mineral","Q11.Quality.Control","size")
create_graph_bar("Mineral","Q11.Schedule","size")
create_graph_bar("Mineral","Q11.Hea.Saf.Env","size")
create_graph_bar("Mineral","Q11.Cost.Control","size")
create_graph_bar("Mineral","Q11.Dependency","size")
create_graph_bar("Mineral","Q11.Coordinate","size")
create_graph_bar("Mineral","Q11.Confidentiality","size")



#####Size

####RISK
risk=ggplot(subset(Survey,Q11.Quality.Control != ""))
risk+aes(x=size,fill=factor(Q11.Quality.Control))+geom_bar(position="fill")+facet_wrap(~Exploration)+theme_minimal()

risk=ggplot(subset(Survey,Q11.Delays.Disr != ""))
risk+aes(x=size,fill=factor(Q11.Delays.Disr))+geom_bar()+facet_wrap(~Exploration)+theme_minimal()



ggplot(subset(Survey,Q11.Quality.Control != ""))+geom_bar(aes(Q11.Quality.Control,fill=as.factor(Exploration)))


"Q11.Quality.Control"
"Q11.Delays.Disr",
"Q11.Schedule",
"Q11.Hea.Saf.Env",
"Q11.Cost.Control",
"Q11.Dependency",
"Q11.Coordinate",
"Q11.Confidentiality",
"Q11.Comments",




OutTab <- as.data.frame(table(Survey$Q7.Outsourcing))
OutTab$lab <- as.character(round(100 * OutTab$Freq / sum(OutTab$Freq)))



Out<-ggplot(Survey)

Out+geom_bar(aes(x=Exploration,fill=factor(Q7.Outsourcing)))+geom_text(aes(label = lab, x = Var1, y = Freq), data = OutTab,vjust=0)


Out+geom_bar()+scale_x_continuous("weight",breaks=c(0,1),label=c("Mining","Exploration"))


Out+geom_bar()+scale_color_manual("Year",values=c("red","purple"))

Out+geom_bar(color="blue",fill="blue")+facet_wrap(~Exploration)+xlab("Exploration")+ylab("Outsourcing")+ggtitle("This is my plot")

Out+geom_bar(color="blue",fill="blue")+facet_wrap(~Exploration)+labs(x="My x",title="Prince",shape="Quality")

Out+geom_histogram()



##
ggplot(Survey)+aes(x=Q6.Employee,fill=Q7.Outsourcing)+geom_bar()+facet_wrap(~Q4.Gold+Exploration)
###conclusion:small companies that dont mine gold are more likely to not outsource

ggplot(Survey[which(Survey$Exploration==0),])+aes(x=Q11.Quality.Control,fill=Q7.Outsourcing)+geom_bar()+facet_wrap(~Q6.Employee)




################ BOXPLOT SIZE vs Critical Success Factors
ggplot(Survey[which(Survey$size!=""),])+aes(x=factor(size), y=Q13.Eff.Mutual.Benef,fill=factor(size))+geom_boxplot()

ggplot(Survey[which(Survey$size!=""),])+geom_histogram(aes(x=Q13.Eff.Mutual.Benef))+facet_grid(~Mineral)


########################
####Graph for equipment
#########################


Equipment=ggplot(Survey[which(Survey$size!="" & Q7.Outsourcing=="Yes"),])+geom_bar(aes(Q8.Expl.Serv))+geom_bar(aes(Q8.Const.Serv))+geom_bar(aes(Q8.Drill.Blast))+geom_bar(aes(Q8.Load.Haul))+geom_bar(aes(Q8.Tunnel))+geom_bar(aes(Q8.Crush.Grind.Flot))+geom_bar(aes(Q8.Leach))+geom_bar(aes(Q8.Concent))+geom_bar(aes(Q8.Waste.Dump))+geom_bar(aes(Q8.Tail.Dump))

     
Equipment+facet_wrap(~Q6.Employee)+ theme(axis.text = element_text(angle = 90))+aes(fill=factor(Exploration))
  
  
  
####################################################
####################################################
#############CREATING RISK DATA SET#################
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
  
  
  
  
  
