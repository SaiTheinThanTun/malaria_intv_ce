#The source data has to be taken from Ko Wynn Zaw's "qry_histogram_monthly_(2013)"+TS_pcode
#The exported file (.xlsx format) has to be coverted into .csv format either by Excel or Libra's Calc
#Converted csv file will be placed under "~/R/DataRep_reportMay2015" 
#with the name "rdt_longfile.csv"

##Cleared up version of CHW efficiency analysis
#Setting the directory
setwd("~/R/DataRep_report052015")
#Reading the file
rdt <- read.csv("rdt_longfile.csv")
names(rdt) %in% c("State..Division","Township","TS_Pcode", "Volunteer", "Month","Year", "Source", "X2013")
#if the above name check turns out to include any FALSE, there's something wrong with the dataset

#no. of Invalid IDs (blank Volunteer names...)
rdt <- rdt[rdt$Volunteer!="",]
rdt$Month <- toupper(rdt$Month) #Changing the months into ALLCAPS


#adding p-codes #if Ko Wynn Zaw can export a datafile with p_codes, this part is not necessary, but some of the codes need to be changed
#A consideration has to be made for where to put this manipulation (at the datacleaning state initially, or after getting the unique CHWs)
for(i in 1:nrow(rdt)){
  if(rdt$Township[i]=="Bago Township") rdt$p_code[i] <- "pcodevalue" ...
  
}