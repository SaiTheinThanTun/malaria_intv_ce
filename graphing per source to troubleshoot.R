
setwd("~/R Working Directory/")
q1x2013 <- read.csv("2013 and 2014/Q1_ALL_25-5-2015-Temp_for2013.csv")
q1x2014 <- read.csv("2013 and 2014/Q1_2014 data_23062015_draft.csv")

names(q1x2013)==names(q1x2014)

rdt <- rbind(q1x2013,q1x2014)

#COPIED from q1_20150619_STTT.R
pf <- c("Mix","pf","Pf","PF")
npf <- c("Non-Pf","pv","Pv", "Npf", "NPf")
neg <- "Neg"
type2include <- c("Clinic","HF","Volunteer") #There's no Mobile and Screening points in 2013 data

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
#rdt <- rdt[rdt$Yr=="2014",] #subsetting for 2013
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
levels(rdt$Source)[levels(rdt$Source)=="WHO"] <- "WHO-NMCP"
levels(rdt$Source)[levels(rdt$Source)=="NMCP"] <- "BHS"
selectedSource <- c("CDA","CPI","IOM","MAM","MMA","PSI","SC","WC","WHO-NMCP","WV","BI")

library(reshape2)
library(zoo)
tmp <- rdt  

for(i in 1:length(selectedSource)){ 
  rdt <- tmp
  rdt <- rdt[rdt$Source %in% selectedSource[i],] #Switch for subsetting only "WHO/NMCP" & "BHS"
  setwd(paste("~/R Working Directory/2013 and 2014/",selectedSource[i],sep=""))
  #q1_2_table.R
  
  
  trendplot <- function(rdt=rdt, type){
    if(type=="chw"){
      rdt <- rdt[rdt$Expr1=="CHW"|rdt$Expr1=="Village",]
      typep <- "Community Health Worker"
      y_limits <- c(10,700)
    }
    #if(type=="hf"){
    #  rdt <- rdt[rdt$Expr1=="HF",]
    #  typep <- "Health Facility"
    #  y_limits <- c(10,1000)
    #}
    if(type=="all") {
      typep <- "Health facility and CHW"
      y_limits <- c(10,1000)
    }
    
    combined <- dcast(rdt, Yr+Mth ~ Outcome, sum, na.rm=TRUE, value.var="Number") #To graph testing per month graphs
    #comb2013 <- combined[combined$Yr==2013,]
    
    
    combined$yrmth <- as.yearmon(paste(combined$Yr,combined$Mth), "%Y %b")
    #combined$pf_npf <- combined$Pf+combined$`Non-Pf`
    #combined$tested <- combined$Pf+combined$`Non-Pf`+combined$Neg
    
    #Plotting
    png(file=paste(typep,Sys.Date(),".png",sep=""), width=960, height=960)
    plot(combined$Pf ~ combined$yrmth, type="l", col="coral1", ylim=y_limits, main=paste("Malaria incidence (",typep,")\nMARC region, 2013 and 2014",sep=""), xlab="Months", ylab="No. of Malaria Cases",lwd=3)
    lines(combined$`Non-Pf` ~ combined$yrmth, type="l", col="orange", lwd=3)
    legend("topright", legend=c("Pf+Pmix","Non-Pf"),lty=1, lwd=3,col=c("coral1","orange"))
    grid()
    dev.off()
  }
  trendplot(rdt=rdt, type="chw")+  trendplot(rdt=rdt, type="all") #trendplot(rdt=rdt, type="hf")
  
  
  combined <- dcast(rdt, Yr+Mth ~ Outcome, sum, na.rm=TRUE, value.var="Number") #To graph testing per month graphs
  combined[,1] <- as.yearmon(paste(combined$Yr,combined$Mth), "%Y %b")
  combined <- combined[,-2]
  YearMonth <- combined$Yr
  #9. Plot4: Stacked Outcome plot per month
  tcomb <- t(combined)
  colnames(tcomb) <- tcomb[1,]
  tcomb <- tcomb[-1,]
  png(file=paste("stacked_oc_mnth_",Sys.Date(),".png",sep=""), width=850, height=480)
  barplot(tcomb, col=c("cornflowerblue","orange","coral1"), border="white", main="Malaria Outcomes per Month\n MARC region, 2013 and 2014", ylab="No. of Malaria Outcomes")
  legend("topleft",legend=c("Negative","Non-Pf","Pf+Pmix"),fill=c("cornflowerblue","orange","coral1"), horiz=TRUE)
  dev.off()
  
  #10. Plot5: Percentage Stacked Outcome plot per month
  tcomb_prop <- t(prop.table(as.matrix(combined[,2:4]),1))
  colnames(tcomb_prop) <- as.character(YearMonth)
  png(file=paste("stacked_oc_mnth_percent_",Sys.Date(),".png",sep=""), width=850, height=480)
  barplot(tcomb_prop, col=c("cornflowerblue","orange","coral1"), border="white", main="Percentage of Malaria Outcomes per Month\n MARC region, 2013 and 2014", ylab="Percentage of Malaria Outcomes per Month")
  legend("bottomright",legend=c("Negative","Non-Pf","Pf+Pmix"),fill=c("cornflowerblue","orange","coral1"))
  dev.off()
  
  #barplot of difference between Pf cases and Non-Pf cases
  
  combined$dif <- combined$Pf-combined$`Non-Pf`
  png(file=paste("pf-npf_difference_",Sys.Date(),".png",sep=""))
  barplot(combined$dif, names.arg=combined$Yr)
  dev.off()
  
}
