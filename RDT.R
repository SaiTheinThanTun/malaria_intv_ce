#RDT testing and their results/ Positivity, etc
#Analyzed from Access Query: qrtTC_combined
setwd("~/R/DataRep_report052015")

#Setting up name vectors for Outcome
pf <- c("Mix","pf","Pf","PF")
npf <- c("Non-Pf","pv","Pv")
neg <- "Neg"

rdt_org <- read.csv("TC_combined.csv")

#Outcome table for Tom
ocTom <- dcast(rdt_org, Yr ~ Outcome, sum, na.rm=TRUE, value.var="Number")
write.csv(ocTom, "outcomes.csv")

#cleaning up
rdt <- rdt_org[rdt_org$Outcome %in% c(pf,npf,neg),] #67369 12 -> 66844 12
rdt <- rdt[rdt$Mth!="",] #66841 12
rdt <- rdt[!is.na(rdt$Yr),]
#Other filtering can also be done here, such as filtering for MARC region
#Box link: https://app.box.com/s/4jwka23zsbsukxtng1c8t6jq9vpxayy1
#Dropbox link: https://www.dropbox.com/s/wq1rdkdin9a5c4g/MARC%20PCodes.csv?dl=0

marc_p <- read.csv("E:\\Box Sync\\MOCRU\\Data\\MARC PCodes.csv") #Windows link


library(reshape2)
rdt$Mth <- toupper(rdt$Mth)
rdt$Mth <- factor(rdt$Mth, c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))
table(rdt$Mth)
#rdt$Outcome <- factor(rdt$Outcome)

#Recoding the Outcome variable
rdt$Outcome[rdt$Outcome %in% pf] <- "Pf"
rdt$Outcome[rdt$Outcome %in% npf] <- "Non-Pf"
#rdt$Outcome[rdt$Outcome %in% neg] <- "Neg"
rdt$Outcome <- factor(rdt$Outcome)

#by month
combined <- dcast(rdt, Yr+Mth ~ Outcome, sum, na.rm=TRUE, value.var="Number") #To graph testing per month graphs
comb2013 <- combined[combined$Yr==2013,]

library(zoo)
comb2013$yrmth <- as.yearmon(paste(comb2013$Yr,comb2013$Mth), "%Y %b")
comb2013$pf_npf <- comb2013$Pf+comb2013$`Non-Pf`
comb2013$tested <- comb2013$Pf+comb2013$`Non-Pf`+comb2013$Neg

#Plotting for 2013 
plot(comb2013$Pf ~ comb2013$yrmth, type="l")
plot(comb2013$`Non-Pf` ~ comb2013$yrmth, type="l")
plot(comb2013$Neg ~ comb2013$yrmth, type="l")

#plot(combined$Neg[3:38] ~ combined$yrmth[3:38], type="s")


#testing from year 2012 to date (range is so big, better try with )
#combined <- combined[3:38,]
#plot(combined$Neg ~ combined$yrmth, type="l")

#dcasting whole data
dcast(rdt,Township+Yr ~ Outcome ,sum, value.var="Number")
dcast(rdt, State_Region ~ Outcome, sum, value.var="Number")


#dcasting by CHW and HF
rdt_s <- split(rdt, rdt$Expr1)
dcast(rdt_s$CHW,Township+Yr ~ Outcome ,sum, value.var="Number")
dcast(rdt_s$HF,Township+Yr ~ Outcome ,sum, value.var="Number")
