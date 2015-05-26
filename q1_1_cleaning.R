getwd() #Check out your working directory by getwd() command. 
#Copy the following files into the working directory:
#1. Q1.csv #Query 1 produced from Access, and then converted to CSV
#2. MARC PCodes.csv ##Link: https://app.box.com/s/4jwka23zsbsukxtng1c8t6jq9vpxayy1

#The outputs will also be produced in the same directory.
#Outputs are for 2013 MARC area: 
#1. Table1: Outcomes per IP
#2. Table2: Outcomes per States, Regions
#3. Table3: Outcomes per Townships
#4. Table4: Xtab Townships against IP (Total tests)
#5. Table5: Xtab Townships against IP (Pf+Pmix)
#6. Plot1: Malaria incidence (CHW)
#7. Plot2: Malaria incidence (Health Facility)
#8. Plot3: Malaria incidence (CHW + Health Facility)
#9. Plot4: Stacked Outcome plot per month
#10. Plot5: Percentage Stacked Outcome plot per month
#11. Plot6: Stacked Outcome plot per States/Regions
#12. Plot7: Percentage Stacked Outcome plot per States/Regions
#13. Plot8: Stacked Outcome plot per Township
#14. Plot9: Percentage Stacked Outcome plot per Township


#q1_1_cleaning
#Setting up name vectors for Outcome & Type categories
pf <- c("Mix","pf","Pf","PF")
npf <- c("Non-Pf","pv","Pv")
neg <- "Neg"
type2include <- c("Clinic","HF","Volunteer") #There's no Mobile and Screening points in 2013 data

rdt <- read.csv("Q1.csv")

#Changing the name of "SumOfNumber" variable into "Number"
if(sum(names(rdt) %in% "SumOfNumber") >0){
  names(rdt)[names(rdt)=="SumOfNumber"] <- "Number"
}
#checking names
sum(names(rdt) %in% c("Expr1", "SR_Pcode","State_Region","Tsp_Code","Township","Yr","Mth", "Age_Group", "Gender", "Outcome","Number", "Source","Type"))==13



#Box link: https://app.box.com/s/4jwka23zsbsukxtng1c8t6jq9vpxayy1
#Dropbox link: https://www.dropbox.com/s/wq1rdkdin9a5c4g/MARC%20PCodes.csv?dl=0
marc_p <- readLines("MARC PCodes.csv")
rdt <- rdt[rdt$Tsp_Code %in% marc_p[-1],]

#cleaning up
rdt <- rdt[rdt$Outcome %in% c(pf,npf,neg),] #removing unknown outcomes
rdt <- rdt[rdt$Mth!="",] #removing records with no month information
rdt <- rdt[!is.na(rdt$Mth),] #removing recodes with NA
rdt <- rdt[rdt$Yr=="2013",] #subsetting for 2013
rdt <- rdt[rdt$Type %in% type2include,]

rdt$Mth[rdt$Mth=="April"] <- "Apr"#This will be removed when Wynn has cleaned the month
rdt$Mth[rdt$Mth=="July"] <- "Jul"

#Standardizing
rdt$Mth <- toupper(rdt$Mth)

sum(!(levels(factor(rdt$Mth)) %in% c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")))
#if the above value is more than 0, there's something wrong with the month values

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

#Recoding IP names
levels(rdt$Source)[levels(rdt$Source)=="WHO"] <- "WHO/NMCP"
levels(rdt$Source)[levels(rdt$Source)=="NMCP"] <- "BHS"
