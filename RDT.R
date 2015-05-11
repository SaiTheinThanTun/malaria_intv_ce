#RDT testing and their results/ Positivity, etc
setwd("~/R/DataRep_report052015")
rdt <- read.csv("TC_combined.csv")
rdtmkn2013 <- rdt[rdt$Township=="Myitkyina"&rdt$Yr==2013,]
attach(rdtmkn2013)
rdtmkn2013chw <- rdtmkn2013[Expr1=="CHW",]
library(reshape2)
dcast(rdt,Township+Yr ~ Outcome ,sum, value.var="Number")
