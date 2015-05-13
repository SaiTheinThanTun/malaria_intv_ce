#RDT testing and their results/ Positivity, etc
#Analyzed from Access Query: qrtTC_combined +Type

setwd("~/R/DataRep_report052015")


#Setting up name vectors for Outcome
pf <- c("Mix","pf","Pf","PF")
npf <- c("Non-Pf","pv","Pv")
neg <- "Neg"

#rdt_org <- read.csv("TC_combined.csv")
#rdt_org <- read.csv("TC_combined20150512.csv")
rdt_org <- read.csv("marc2013TC_combined_20150513.csv")

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


library(reshape2)
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

library(zoo)

#STARTS OF FUNCTION
#how to run: trendplot(rdt=rdt, type="chw"), trendplot(rdt=rdt, type="hf"), trendplot(rdt=rdt, type="all")
trendplot <- function(rdt=rdt, type){
  if(type=="chw"){
    rdt <- rdt[rdt$Expr1=="CHW"|rdt$Expr1=="Village",]
    typep <- "Community Health Worker"
    y_limits <- c(500,2000)
  }
  if(type=="hf"){
    rdt <- rdt[rdt$Expr1=="HF",]
    typep <- "Health Facility"
    y_limits <- c(500,4500)
  }
  if(type=="all") {
    typep <- "Health facility and CHW"
    y_limits <- c(1500,7000)
  }
  
  #Run program as usual!
  combined <- dcast(rdt, Yr+Mth ~ Outcome, sum, na.rm=TRUE, value.var="Number") #To graph testing per month graphs
  comb2013 <- combined[combined$Yr==2013,]
  
  
  comb2013$yrmth <- as.yearmon(paste(comb2013$Yr,comb2013$Mth), "%Y %b")
  comb2013$pf_npf <- comb2013$Pf+comb2013$`Non-Pf`
  comb2013$tested <- comb2013$Pf+comb2013$`Non-Pf`+comb2013$Neg
  
  #Plotting
  png(file=paste(typep,Sys.Date(),".png",sep=""), width=960, height=960)
  plot(comb2013$Pf ~ comb2013$yrmth, type="l", col="blue", ylim=y_limits, main=paste("Malaria incidence (",typep,")\nMARC region, 2013",sep=""), xlab="Months", ylab="No. of Malaria Cases")
  lines(comb2013$`Non-Pf` ~ comb2013$yrmth, type="l", col="red")
  legend("topright", legend=c("Pf+Pmix","Non-Pf"),lty=1,col=c("blue","red"))
  grid()
  dev.off()
}
#trendplot(rdt=rdt, type="chw")+ trendplot(rdt=rdt, type="hf")+ trendplot(rdt=rdt, type="all")

plot(comb2013$tested ~ comb2013$yrmth, type="l", col="purple")
plot(comb2013$`Non-Pf` ~ comb2013$yrmth, type="l")
plot(comb2013$Neg ~ comb2013$yrmth, type="l")

#library(RColorBrewer)
combined <- dcast(rdt, Yr+Mth ~ Outcome, sum, na.rm=TRUE, value.var="Number") #To graph testing per month graphs

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
