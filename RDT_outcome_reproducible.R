#Files need to be present in the working directory:
#1. marc2013TC_combined_20150518.csv
#2. MARC PCodes.csv
#Data Reading and Cleaning
#RDT testing and their results/ Positivity, etc
#Analyzed from Access Query: qrtTC_combined +Type




#Setting up name vectors for Outcome categories
pf <- c("Mix","pf","Pf","PF")
npf <- c("Non-Pf","pv","Pv")
neg <- "Neg"

#rdt_org <- read.csv("TC_combined.csv") #rdt_org <- read.csv("TC_combined20150512.csv")
#rdt_org <- read.csv("marc2013TC_combined_20150513.csv")
rdt_org <- read.csv("marc2013TC_combined_20150518.csv")

#checking names
sum(names(rdt_org) %in% c("Expr1", "SR_Pcode","State_Region","Tsp_Code","Township","Yr","Mth", "Age_Group","Outcome","Diag_method","Number", "Source","Type"))==13
#Changing the name of "SumOfNumber" variable into "Number"
if(sum(names(rdt_org) %in% "SumOfNumber") >0){
  names(rdt_org)[names(rdt_org)=="SumOfNumber"] <- "Number"
}
#Outcome table for Tom
#ocTom <- dcast(rdt_org, Yr ~ Outcome, sum, na.rm=TRUE, value.var="Number")
#write.csv(ocTom, "outcomes.csv")

#cleaning up
rdt <- rdt_org[rdt_org$Outcome %in% c(pf,npf,neg),] #46658 12 -> 66844 12
rdt <- rdt[rdt$Mth!="",] #46132 12
rdt <- rdt[!is.na(rdt$Yr),] #46132 12
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