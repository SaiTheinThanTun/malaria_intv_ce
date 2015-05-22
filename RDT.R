#RDT testing and their results/ Positivity, etc
#Analyzed from Access Query: qrtTC_combined +Type
#Data reading and cleaning has to be done with a seperate script: RDT_reading_cleaning.R


#Required libraries for loading the function
library(zoo)
library(reshape2)
#START OF THE FUNCTION
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
  plot(comb2013$Pf ~ comb2013$yrmth, type="l", col="coral1", ylim=y_limits, main=paste("Malaria incidence (",typep,")\nMARC region, 2013",sep=""), xlab="Months", ylab="No. of Malaria Cases",lwd=3)
  lines(comb2013$`Non-Pf` ~ comb2013$yrmth, type="l", col="orange", lwd=3)
  legend("topright", legend=c("Pf+Pmix","Non-Pf"),lty=1, lwd=3,col=c("coral1","orange"))
  grid()
  dev.off()
}
trendplot(rdt=rdt, type="chw")+ trendplot(rdt=rdt, type="hf")+ trendplot(rdt=rdt, type="all")

#Per Source (IP)
perSource2013 <- dcast(rdt, Source ~ Outcome, sum, na.rm=TRUE, value.var="Number")
perSource2013$Total <- perSource2013$Neg+perSource2013$`Non-Pf`+perSource2013$Pf
write.csv(perSource2013, paste("OCperSource2013_",Sys.Date(),".csv",sep=""))

#Per Townships
perTsp2013 <- dcast(rdt, Tsp_Code+Township ~ Outcome, sum, na.rm=TRUE, value.var="Number")
perTsp2013$Total <- perTsp2013$Neg+perTsp2013$`Non-Pf`+perTsp2013$Pf
write.csv(perTsp2013, paste("OCperTsp2013_",Sys.Date(),".csv",sep=""))

#Per States/Divisions
perStates2013 <- dcast(rdt, State_Region ~ Outcome, sum, na.rm=TRUE, value.var="Number")
perStates2013$Total <- perStates2013$Neg+perStates2013$`Non-Pf`+perStates2013$Pf
write.csv(perStates2013, paste("OCperStates2013_",Sys.Date(),".csv",sep=""))

#END#####

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
