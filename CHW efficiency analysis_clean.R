#The source data has to be taken from Ko Wynn Zaw's "qry_histogram_monthly_(2013)"+TS_pcode
#The exported file (.xlsx format) has to be coverted into .csv format either by Excel or Libra's Calc
#Converted csv file will be placed under "~/R/DataRep_reportMay2015" 
#with the name "rdt_longfile.csv"

##Cleared up version of CHW efficiency analysis
#Setting the directory
setwd("~/R/DataRep_report052015")
#Reading the file
rdt <- read.csv("rdt_longfile.csv")

#checking variable names
names(rdt) %in% c("State..Division","Township","TS_Pcode", "Volunteer", "Month","Year", "Source", "X2013")
#if the above name check turns out to include any FALSE, there's something wrong with the dataset

#no. of Invalid IDs (blank Volunteer names...)
rdt <- rdt[rdt$Volunteer!="",]
rdt$Month <- toupper(rdt$Month) #Changing the months into ALLCAPS

#Checking months
table(rdt$Month) #comparing RDTs per month

###RESHAPing before analying (instead of SPLIT). HOWEVER, melting doesn't change anything since the data is already in the long format
library(reshape2)
id_rdt <- names(rdt)
m_rdt <- melt(rdt, id=id_rdt[-7]) #melting rdt into m_rdt

#this Overall monthly average may not be necessary
dcast(m_rdt, Month ~ variable, mean) #Average RDTs per month in 2013

mean((m_rdt$value)) #11.01737 tests per month Total average in 2013

#Township performance per month
TMrdt <- dcast(m_rdt, Township+Month ~ variable, sum) ####FURTHER ANALYSIS REQUIRED!!!
table(TMrdt$Township) #sum(table(TMrdt$Township)>12) to check if any township is overperforming :D

#Mean RDT tests
rdt_uniq_vts_avg <- dcast(m_rdt, Volunteer+Township+Source ~ variable, mean) #Per VHW, the mean RDTs 

median(rdt_uniq_vts_avg$X2013) #Median testing rate of 5 is a more robust measure since there are outstanding CHWs with >200 RDTs 
sd(rdt_uniq_vts_avg$X2013) #15.28

#Bins from the first report (oct 2014)
rdt_uniq_vts_avg$f <- cut(rdt_uniq_vts_avg$X2013, c(0,10,20,30,40,50,250), labels=c("<=10","11-20","21-30","31-40","41-50",">50"))
barplot(table(rdt_uniq_vts_avg$f), main=paste("Average RDT testing rates of CHWs per month \n in 2013 (median=",median(rdt_uniq_vts_avg$X2013),")",sep=""), xlab= "No. of RDTs", ylab= "No. of CHWs")

#experimental bins
#base 2 bin
rdt_uniq_vts_avg$f <- cut(rdt_uniq_vts_avg$X2013, c(0,2^(0:6)[-1],250))
barplot(table(rdt_uniq_vts_avg$f), main=paste("Average RDT testing rates of CHWs per month \n in 2013 (median=",median(rdt_uniq_vts_avg$X2013),")",sep=""), xlab= "No. of RDTs", ylab= "No. of CHWs")

#adding p-codes #if Ko Wynn Zaw can export a datafile with p_codes, this part is not necessary, but some of the codes need to be changed
#A consideration has to be made for where to put this manipulation (at the datacleaning state initially, or after getting the unique CHWs)
for(i in 1:nrow(rdt)){
  if(rdt$Township[i]=="Bago Township") rdt$p_code[i] <- "pcodevalue" ...
  
}