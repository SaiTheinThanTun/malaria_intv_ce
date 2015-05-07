setwd("~/R/")
CHW <- read.csv("MOCRU_org//03CHWcase20150408.csv")

#Creating a subset of CHW
req_var <- c("State..Division","Township","TS_Pcode","Year","BHS.Volunteer","Volunteer", "Outcome")
#checking if the required variables are in the dataset
sum(names(CHW) %in% req_var)==7

chw <- CHW[names(CHW) %in% req_var]
library(reshape2)
####To include Data Cleaning methods from Stat Holland such as "str_trim", "toupper", etc

#Manually changing the case of the names. There may be a way of shortening this code using lapply, etc
chw$State..Division <- toupper(chw$State..Division)
chw$Township <- toupper(chw$Township)
chw$TS_Pcode <- toupper(chw$TS_Pcode)
chw$Volunteer <- toupper(chw$Volunteer)
chw$Outcome <- toupper(chw$Outcome)

#Cleaning up Outcome variable
chw <- chw[chw$Outcome %in% c("0",)]

#Summarizing
table(chw$Outcome)
table(chw$BHS.Volunteer)
table(chw$BHS.Volunteer,chw$Outcome)
table(chw$Township, chw$Outcome)

#Melting & casting
m_chw <- melt(chw, id=req_var[1:4])
dcast(m_chw, Township ~ variable, length)
dcast(m_chw, Township ~ variable, value.var="BHS.Volunteer", length)

m_chw2 <- melt(chw[c(1:4,6)], id=req_var[1:4])
dcast(m_chw2, Year ~ variable, )
table(m_chw2$value)



###Searching for similar and possibly the same (but spelled differently) names of CHW per township
split_chw <- split(chw, chw$Township)
uniq_chw_perTSP <- lapply(split_chw, function(x) unique(x[5])) #This produces Volunteer as a vector in the lists
uniq_chw_perTSP <- lapply(split_chw, function(x) x[!duplicated(x[5]),]) #This produces the whole data.frame in the lists

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

