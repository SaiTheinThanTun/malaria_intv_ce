##Cleared up version of CHW efficiency analysis
#Setting the directory

#Reading the file

#no. of Invalid IDs (blank Volunteer names...)

#adding p-codes #if Ko Wynn Zaw can export a datafile with p_codes, this part is not necessary, but some of the codes need to be changed
#A consideration has to be made for where to put this manipulation (at the datacleaning state initially, or after getting the unique CHWs)
for(i in 1:nrow(rdt)){
  if(rdt$Township[i]=="Bago Township") rdt$p_code[i] <- "pcodevalue" ...
  
}