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

combined <- dcast(rdt, Yr+Mth ~ Outcome, sum, na.rm=TRUE, value.var="Number") #To graph testing per month graphs
#comb2013 <- combined[combined$Yr==2013,]

combined <- combined[,-1] #just after dcast
tcomb <- t(combined)
colnames(tcomb) <- tcomb[1,]
tcomb <- tcomb[-1,]
barplot(tcomb, col=c("cornflowerblue","orange","coral1"), border="white", legend.text=c("Negative","Non-Pf","Pf"))
#plot(combined$Neg[3:38] ~ combined$yrmth[3:38], type="s")

tcomb2 <- tcomb[-1,]
barplot(tcomb2, col=c("cornflowerblue","coral1"), border="white", legend.text=c("Non-Pf","Pf"))

#pf npf comparison between states..divisons
states <- dcast(rdt, State_Region ~ Outcome, sum, na.rm=TRUE, value.var="Number") 
tstates <- t(states)
colnames(tstates) <- states$State_Region
tstates <- tstates[-1,]
barplot(tstates)

#percent-based stacked bars #pf npf comparison between states..divisons
prop <- t(prop.table(as.matrix(states[,2:4]),1))
colnames(prop) <- states$State_Region
barplot(prop)


#pf npf comparison between townships
tsp <- dcast(rdt, Township ~ Outcome, sum, na.rm=TRUE, value.var="Number")
prop_tsp <- t(prop.table(as.matrix(tsp[,2:4]),1))
colnames(prop_tsp) <- tsp$Township
prop_tsp_df <- as.data.frame(prop_tsp)
prop_tsp1 <- prop_tsp_df[,1:10]
prop_tsp2 <- prop_tsp_df[,11:20]
prop_tsp3 <- prop_tsp_df[,21:30]
prop_tsp4 <- prop_tsp_df[,31:40]
prop_tsp5 <- prop_tsp_df[,41:55]

#par(mfrow=c(2,1))
barplot(as.matrix(prop_tsp1), cex.names=.5)
barplot(as.matrix(prop_tsp2), cex.names=.5)
barplot(as.matrix(prop_tsp3), cex.names=.5)
barplot(as.matrix(prop_tsp4), cex.names=.5)
barplot(as.matrix(prop_tsp5), cex.names=.5)