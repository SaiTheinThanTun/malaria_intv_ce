#Comparison, NMCP summary Vs HF+CHW WHO data Vs HF+all CHW(including village summaries)+all clinics
#No. of tests, pf, CHW, (and HF but not necessarily)
#most codes have been taken from RDT.R.
setwd("~/R/DataRep_report052015")
#Setting up name vectors for Outcome
pf <- c("Mix","pf","Pf","PF")
npf <- c("Non-Pf","pv","Pv")
neg <- "Neg"
#Reading dataset
rdt_org <- read.csv("marc2013TC_combined_20150513.csv")

#checking names
sum(names(rdt_org) %in% c("Expr1", "SR_Pcode","State_Region","Tsp_Code","Township","Yr","Mth", "Age_Group","Outcome","Diag_method","Number", "Source","Type"))==13
#Changing the name of "SumOfNumber" variable into "Number"
if(sum(names(rdt_org) %in% "SumOfNumber") >0){
  names(rdt_org)[names(rdt_org)=="SumOfNumber"] <- "Number"
}

#cleaning up
rdt <- rdt_org[rdt_org$Outcome %in% c(pf,npf,neg),] #46658 12 -> 66844 12
rdt <- rdt[rdt$Mth!="",] #46132 12
rdt <- rdt[!is.na(rdt$Yr),] #46132 12
rdt <- rdt[rdt$Yr==2013,]
#Other filtering can also be done here, such as filtering for MARC region
#Box link: https://app.box.com/s/4jwka23zsbsukxtng1c8t6jq9vpxayy1
#Dropbox link: https://www.dropbox.com/s/wq1rdkdin9a5c4g/MARC%20PCodes.csv?dl=0

#marc_p <- read.csv("E:\\Box Sync\\MOCRU\\Data\\MARC PCodes.csv") #Windows link
marc_p <- read.csv("MARC PCodes.csv")
marc_p <- marc_p$Tsp_Code #this changes the data.frame into a vector which we can use for subsetting.
rdt <- rdt[rdt$Tsp_Code %in% marc_p,]

rdt$Mth <- toupper(rdt$Mth)
rdt$State_Region <- toupper(rdt$State_Region)
rdt$Township <- toupper(rdt$Township)
rdt$Mth <- factor(rdt$Mth, c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))
table(rdt$Mth)
#rdt$Outcome <- factor(rdt$Outcome)

#Recoding the Outcome variable
rdt$Outcome[rdt$Outcome %in% pf] <- "Pf"
rdt$Outcome[rdt$Outcome %in% npf] <- "Non-Pf"
#rdt$Outcome[rdt$Outcome %in% neg] <- "Neg"
rdt$Outcome <- factor(rdt$Outcome)

#Crosschecking the variables (Expr1 and Type)
table(rdt[rdt$Expr1=="Village",]$Type) #Village summary contains both clinics and Volunteer data
table(rdt[rdt$Expr1=="HF",]$Type) #only HF data
table(rdt[rdt$Expr1=="CHW",]$Type) #contains some uncleaned data from IOM

#Crosschecking the variables (Expr1 and Source)
table(rdt[rdt$Expr1=="Village",]$Source) #village summaries are from MMA and PSI
table(rdt[rdt$Expr1=="HF",]$Source) #HF data is only from NMCP
table(rdt[rdt$Expr1=="CHW",]$Source) #MMA, PSI and NMCP doesn't have CHW data

#Crosschecking the variables (Source and Type|Expr1)
table(rdt[rdt$Source=="WHO",]$Type) #all data from WHO are Volunteer data
table(rdt[rdt$Source=="WHO",]$Expr1) #all data from WHO are of CHW data [Volunteer = CHW of course]

##According to the cross-checks above, all data from Source "WHO" 
#is both Volunteer and CHW (3258) as of 20150514.



hf_chw_who <- rdt[rdt$Expr1=="HF"|rdt$Source=="WHO",] #HF+CHW WHO data
hf_chw_all <- rdt[rdt$Type=="HF"|rdt$Type=="Volunteer"|rdt$Type=="Clinic",] #HF+all CHW(including village summaries)+all clinics

library(reshape2)
#HF+CHW WHO data by States..Divisions
adata <- hf_chw_who
oc <- dcast(adata,State_Region ~ Outcome, sum, na.rm=TRUE, value.var="Number")
oc$Total <- oc$Neg+oc$`Non-Pf`+oc$Pf
write.csv(oc, paste("hf_chw_who_bystates_",Sys.Date(),".csv",sep=""))

#HF+all CHW(including village summaries)+all clinics by States..Divisions
adata <- hf_chw_all
oc <- dcast(adata,State_Region ~ Outcome, sum, na.rm=TRUE, value.var="Number")
oc$Total <- oc$Neg+oc$`Non-Pf`+oc$Pf
write.csv(oc, paste("hf_chw_all_bystates_",Sys.Date(),".csv", sep=""))

#HF+CHW WHO data by Townships
adata <- hf_chw_who
oc <- dcast(adata,Township ~ Outcome, sum, na.rm=TRUE, value.var="Number")
oc$Total <- oc$Neg+oc$`Non-Pf`+oc$Pf
write.csv(oc, paste("hf_chw_who_bytownships_",Sys.Date(),".csv", sep=""))

#HF+all CHW(including village summaries)+all clinics by Townships
adata <- hf_chw_all
oc <- dcast(adata,Township ~ Outcome, sum, na.rm=TRUE, value.var="Number")
oc$Total <- oc$Neg+oc$`Non-Pf`+oc$Pf
write.csv(oc, paste("hf_chw_all_bytownships_",Sys.Date(),".csv",sep=""))