#q1_4_bargraphs
#RDT testing and their results/ Positivity, etc
#Analyzed from Access Query: qrtTC_combined +Type
#Data reading and cleaning has to be done with a seperate script: RDT_reading_cleaning.R

library(reshape2)
combined <- dcast(rdt, Yr+Mth ~ Outcome, sum, na.rm=TRUE, value.var="Number") #To graph testing per month graphs
#comb2013 <- combined[combined$Yr==2013,]

#Per Month
tcomb <- t(combined[,-1])
colnames(tcomb) <- tcomb[1,]
tcomb <- tcomb[-1,]
png(file=paste("stacked_oc_mnth_",Sys.Date(),".png",sep=""), width=850, height=480)
barplot(tcomb, col=c("cornflowerblue","orange","coral1"), border="white", main="Malaria Outcomes per Month\n MARC region, 2013", ylab="No. of Malaria Outcomes")
legend("topleft",legend=c("Negative","Non-Pf","Pf+Pmix"),fill=c("cornflowerblue","orange","coral1"), horiz=TRUE)
dev.off()
#percentage stacked plot per month

tcomb_prop <- t(prop.table(as.matrix(combined[,3:5]),1))
colnames(tcomb_prop) <- combined$Mth
png(file=paste("stacked_oc_mnth_percent_",Sys.Date(),".png",sep=""), width=850, height=480)
barplot(tcomb_prop, col=c("cornflowerblue","orange","coral1"), border="white", main="Percentage of Malaria Outcomes per Month\n MARC region, 2013", ylab="Percentage of Malaria Outcomes per Month")
legend("bottomright",legend=c("Negative","Non-Pf","Pf+Pmix"),fill=c("cornflowerblue","orange","coral1"))
dev.off()

#plot(combined$Neg[3:38] ~ combined$yrmth[3:38], type="s")

tcomb2 <- tcomb[-1,]
barplot(tcomb2, col=c("cornflowerblue","coral1"), border="white", legend.text=c("Non-Pf","Pf"))

#pf npf comparison between states..divisons
states <- dcast(rdt, State_Region ~ Outcome, sum, na.rm=TRUE, value.var="Number") 
tstates <- t(states)
colnames(tstates) <- states$State_Region
tstates <- tstates[-1,]
png(file=paste("stacked_oc_states_",Sys.Date(),".png",sep=""), width=850, height=480)
barplot(tstates, col=c("cornflowerblue","orange","coral1"), border="white", main="Malaria Outcomes per State/Division\n in MARC region, 2013", ylab="No. of Malaria Outcomes")
legend("topleft",legend=c("Negative","Non-Pf","Pf+Pmix"),fill=c("cornflowerblue","orange","coral1"), horiz=TRUE)
dev.off()

#percent-based stacked bars #pf npf comparison between states..divisons
prop <- t(prop.table(as.matrix(states[,2:4]),1))
colnames(prop) <- states$State_Region
png(file=paste("stacked_oc_states_percent_",Sys.Date(),".png",sep=""), width=850, height=480)
barplot(prop,col=c("cornflowerblue","orange","coral1"), border="white", main="Percentage of Malaria Outcomes in different States/Divisions\n MARC region, 2013", ylab="Percentage of Malaria Outcomes per State/Division")
legend("bottomright",legend=c("Negative","Non-Pf","Pf+Pmix"),fill=c("cornflowerblue","orange","coral1"))
dev.off()

#pf npf percentage comparison between townships
tsp <- dcast(rdt, Township ~ Outcome, sum, na.rm=TRUE, value.var="Number")
prop_tsp <- t(prop.table(as.matrix(tsp[,2:4]),1))
colnames(prop_tsp) <- tsp$Township
prop_tsp_df <- as.data.frame(prop_tsp)
prop_tsp1 <- prop_tsp_df[,1:10]
prop_tsp2 <- prop_tsp_df[,11:20]
prop_tsp3 <- prop_tsp_df[,21:30]
prop_tsp4 <- prop_tsp_df[,31:40]
prop_tsp5 <- prop_tsp_df[,41:52]

#par(mfrow=c(2,1))
barplot(as.matrix(prop_tsp1), cex.names=.5, las=2, col=c("cornflowerblue","orange","coral1"), border="white", main="Percentage of Malaria Outcomes in different Townships\n MARC region, 2013", ylab="Percentage of Malaria Outcomes per Township")

barplot(as.matrix(prop_tsp1), cex.names=.5)
barplot(as.matrix(prop_tsp2), cex.names=.5)
barplot(as.matrix(prop_tsp3), cex.names=.5)
barplot(as.matrix(prop_tsp4), cex.names=.5)
barplot(as.matrix(prop_tsp5), cex.names=.5)