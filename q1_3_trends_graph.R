#q1_3_trends_graph

#Required libraries for loading the function
library(zoo)
library(reshape2)

trendplot <- function(rdt=rdt, type){
  if(type=="chw"){
    rdt <- rdt[rdt$Expr1=="CHW"|rdt$Expr1=="Village",]
    typep <- "Community Health Worker"
    y_limits <- c(500,3000)
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

