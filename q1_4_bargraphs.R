#q1_4_bargraphs

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

#pf npf comparison between townships
tsp <- dcast(rdt, Township ~ Outcome, sum, na.rm=TRUE, value.var="Number") 
tsp <- tsp[order(tsp$Neg+tsp$`Non-Pf`+tsp$Pf, decreasing=TRUE),]
ttsp <- t(tsp)
colnames(ttsp) <- tsp$Township
ttsp <- ttsp[-1,]
png(file=paste("stacked_oc_tsp_",Sys.Date(),".png",sep=""), width=1000, height=800)
par(mar=c(8,6,4,2))
barplot(ttsp, col=c("cornflowerblue","orange","coral1"), border="white", main="Malaria Outcomes per Township\n in MARC region, 2013", ylab="No. of Malaria Outcomes", las=2, cex.names=.8, mgp=c(4,1,0))
legend("topright",legend=c("Negative","Non-Pf","Pf+Pmix"),fill=c("cornflowerblue","orange","coral1"), horiz=TRUE)
dev.off()

#pf npf percentage comparison between townships
tsp <- dcast(rdt, Township ~ Outcome, sum, na.rm=TRUE, value.var="Number")
prop_tsp <- t(prop.table(as.matrix(tsp[,2:4]),1))
colnames(prop_tsp) <- tsp$Township
prop_tsp_df <- as.data.frame(prop_tsp)

png(file=paste("stacked_oc_tsp_percent_",Sys.Date(),".png",sep=""), width=1000, height=500)
par(mar=c(8,4,4,2))
barplot(as.matrix(prop_tsp_df), cex.names=.8, las=2, col=c("cornflowerblue","orange","coral1"), border="white", main="Percentage of Malaria Outcomes in different Townships\n MARC region, 2013", ylab="Percentage of Malaria Outcomes per Township")
legend("bottomright",legend=c("Negative","Non-Pf","Pf+Pmix"),fill=c("cornflowerblue","orange","coral1"))
dev.off()
