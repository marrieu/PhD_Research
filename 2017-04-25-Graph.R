#######################################
####### ANALYSING SURVEYMONKEY DATA SET
#######################################

# R version 3.2.3 (2015-12-10)
#"/Applications/R/PhD_Research"


source("2017-04-25-Group By.R")


##############################
##### PART 7 - GRAPH 
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








