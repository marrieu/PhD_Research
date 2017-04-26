#######################################
####### ANALYSING SURVEYMONKEY DATA SET
#######################################

# R version 3.2.3 (2015-12-10)
#"/Applications/R/PhD_Research"

source("2017-04-25-Load Library and Data.R")

#######################################
####### PART 2 - RENAME VARIABLES
#######################################

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