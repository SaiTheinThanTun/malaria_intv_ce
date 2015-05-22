#q1_1_cleaning
#Files need to be present in the working directory:
#1. Q1_All.csv
#2. MARC PCodes.csv #https://app.box.com/s/4jwka23zsbsukxtng1c8t6jq9vpxayy1

#RDT testing and their results/ Positivity, etc
#Analyzed from Access Query: qrtTC_combined +Type
#Outputs: 

#Setting up name vectors for Outcome & Type categories
pf <- c("Mix","pf","Pf","PF")
npf <- c("Non-Pf","pv","Pv")
neg <- "Neg"
type2include <- c("Clinic","HF","Volunteer") #There's no Mobile and Screening points in 2013 data

rdt <- read.csv("marc2013TC_combined_20150522.csv")

#checking names
sum(names(rdt) %in% c("Expr1", "SR_Pcode","State_Region","Tsp_Code","Township","Yr","Mth", "Age_Group", "Gender", "Outcome","Diag_method","Number", "Source","Type"))==14
#Changing the name of "SumOfNumber" variable into "Number"
if(sum(names(rdt) %in% "SumOfNumber") >0){
  names(rdt)[names(rdt)=="SumOfNumber"] <- "Number"
}


#Box link: https://app.box.com/s/4jwka23zsbsukxtng1c8t6jq9vpxayy1
#Dropbox link: https://www.dropbox.com/s/wq1rdkdin9a5c4g/MARC%20PCodes.csv?dl=0
marc_p <- readLines("MARC PCodes.csv")
rdt <- rdt[rdt$Tsp_Code %in% marc_p[-1],]

#cleaning up
rdt <- rdt[rdt$Outcome %in% c(pf,npf,neg),] #removing unknown outcomes
rdt <- rdt[rdt$Mth!="",] #removing records with no month information
rdt <- rdt[rdt$Yr=="2013",] #subsetting for 2013
rdt <- rdt[rdt$Type %in% type2include,]

#Standardizing
rdt$Mth <- toupper(rdt$Mth)
rdt$Mth <- factor(rdt$Mth, c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))
rdt$State_Region <- toupper(rdt$State_Region)
rdt$Township <- toupper(rdt$Township)
#table(rdt$Mth)
#rdt$Outcome <- factor(rdt$Outcome)

#Recoding the Outcome variable
rdt$Outcome[rdt$Outcome %in% pf] <- "Pf"
rdt$Outcome[rdt$Outcome %in% npf] <- "Non-Pf"
#rdt$Outcome[rdt$Outcome %in% neg] <- "Neg"
rdt$Outcome <- factor(rdt$Outcome)