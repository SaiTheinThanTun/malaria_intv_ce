#q1_2_table.R
#Per Source (IP)
perSource2013 <- dcast(rdt, Source ~ Outcome, sum, na.rm=TRUE, value.var="Number")
perSource2013$Total <- perSource2013$Neg+perSource2013$`Non-Pf`+perSource2013$Pf
write.csv(perSource2013, paste("OCperIP2013_",Sys.Date(),".csv",sep=""))

#Per Townships
perTsp2013 <- dcast(rdt, Tsp_Code+Township ~ Outcome, sum, na.rm=TRUE, value.var="Number")
perTsp2013$Total <- perTsp2013$Neg+perTsp2013$`Non-Pf`+perTsp2013$Pf
write.csv(perTsp2013, paste("OCperTsp2013_",Sys.Date(),".csv",sep=""))

#Per States/Divisions
perStates2013 <- dcast(rdt, State_Region ~ Outcome, sum, na.rm=TRUE, value.var="Number")
perStates2013$Total <- perStates2013$Neg+perStates2013$`Non-Pf`+perStates2013$Pf
write.csv(perStates2013, paste("OCperStates2013_",Sys.Date(),".csv",sep=""))