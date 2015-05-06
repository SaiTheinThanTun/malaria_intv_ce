setwd("~/R/")
CHW <- read.csv("MOCRU_org//03CHWcase20150408.csv")

#writing out summary table to edit for a codebook for data repository
write.csv(t(summary(CHW)),"Summary_CHW.csv")

#xtabs of RDT.exam and Outcome
xtabs( ~ Outcome + RDT.Exam, data=CHW)
#            RDT.Exam
#Outcome         Not exam by RDT RDT Exam: Mix RDT Exam: Neg.- not malaria RDT Exam: Neg. - Probable mal. RDT Exam: Non-Pf RDT Exam: Pf
#          239             129             0                           0                              0                0            0
#0         598               0             0                           0                              0                0            0
#Invali     77               0             0                           0                              0                0            0
#Mix      5821               0          1799                           0                              0                0            0
#Negati 507386               0             0                       53576                              0                0            0
#Npf     27832               0             0                           0                              0             6475            0
#Others    526               0             0                           0                              0                0            0
#Pf      32435               0             0                           0                              0                0         7565
#PF         53               0             0                           0                              0                0            0
#Pres        0              71             0                           0                              0                0            0
#Pres_A      0              52             0                           0                              0                0            0
#Pres_C      0              25             0                         114                              0                0            0
#Prob        0               0             0                           0                            126                0            0


summary(CHW$RDT.Exam)

#Written with MacBook Pro
#setwd("Desktop/STTT/CHW analysis")
#read.csv("03CHWcase20150408.csv")
attach(CHW)
chw_name <- names(CHW)
chw_list <- list()
for(i in 1:ncol(CHW)){
  chw_list[[i]] <- CHW[,i]
}

summ_chw <- summary(is.na(CHW))
tmp <- lapply(chw_list, function(x) table(is.na(x)))
names(tmp) <- chw_name
NAinfo <- unlist(tmp)
#barplot(NAinfo[grep("TRUE", names(NAinfo))]/644899)
naPercent <- (NAinfo[grep("TRUE", names(NAinfo))]/644899)*100
naPercentNames <- names(naPercent)
names(naPercent) <- gsub(".TRUE", "", naPercentNames)
naPercent

#
hist(table(is.na(Sr)))

#Creating a subset of CHW
chw <- CHW[]