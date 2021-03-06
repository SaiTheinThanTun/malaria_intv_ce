#Analyzing Community Health Worker's capacity and efficiency #the use of X2013 is now obsolete
setwd("~/R")
#install.packages("xlsx")
#library("xlsx")
#rdt <- read.xlsx("rdt_longfile.xlsx", sheetIndex=1) #very power consuming
rdt <- read.csv("rdt_longfile.csv")
dim(rdt) #12302 7

###Cleaning the dataset
rdt <- rdt[rdt$Volunteer!="",] #dimension changed to 12260 7, by removing CHW with no names
rdt$Month <- toupper(rdt$Month) #Changing the months into ALLCAPS

summary(rdt$X2013) 
#This is also accounting for inactive CHWs, where s/he may have not done any testing
#on one of the months in 2013
#mean 11.29
#median 5
#max 399

###Finding possible outliers
rdt[rdt$X2013==399,] #rows 3910 and 3954 have 399 RDTs
#rdt <- rdt[rdt$X2013!=399,]
a <- rdt[rdt$X2013>200,]
a[order(a$X2013,decreasing=TRUE),]

table(rdt$Month) #comparing RDTs per month
barplot(table(rdt$Month))

###RESHAPing before analying (instead of SPLIT)
library(reshape2)
id_rdt <- names(rdt)
m_rdt <- melt(rdt, id=id_rdt[-7]) #melting rdt into m_rdt

tmpmelt_rdt <- melt(rdt[-7], id=id_rdt[-c(2,7)]) #This is melting with township name as values, excluding no. of rdt tests.
tspVchw <- dcast(tmpmelt_rdt[!duplicated(tmpmelt_rdt[,c(2,7)]),], Volunteer ~ variable) #identifying which CHWs have more than 1 townships
tspVchw[tspVchw[,2]>1,] #Lists the CHWs with no. of townships they're responsible.

dcast(m_rdt, Month ~ variable, mean) #Average RDTs per month in 2013
mean((m_rdt$value)) #11.01737 tests per month Total average in 2013

###Calculating RDT testing
#Total RDT tests
rdt_uniq_vts_total <- dcast(m_rdt, Volunteer+Township+Source ~ variable, sum) #TOTAL RDTs per Unique VHW at particular Source
rdt_uniq_vts_total
mean(rdt_uniq_vts_total$X2013) #RDT rate per VHW per year (this includes VHWs who're not active for the whole year)
#Mean RDT tests
rdt_uniq_vts_avg <- dcast(m_rdt, Volunteer+Township+Source ~ variable, mean) #Per VHW, the mean RDTs 
#2402 (cases increased because CHWs are seperated again by townships)
####how to handle decimal places, test can't be 0.456...
RDTmeanOverall <- mean(rdt_uniq_vts_avg$X2013) #Average testing rate of CHWs per month is 9.409963
median(rdt_uniq_vts_avg$X2013) #Median testing rate of 5 is a more robust measure since there are outstanding CHWs with >200 RDTs 
#This will be the median of the mean

sd(rdt_uniq_vts_avg$X2013) #15.28

hist(rdt_uniq_vts_avg$X2013, main=paste("Average RDT testing rates of CHWs per month (mean=",RDTmeanOverall,")",sep=""), xlab= "No. of RDTs", ylab= "No. of CHWs")
#The histogram has a long tail and thus it is not presentable.
#Try to cut the dataset (rdt_uniq_vts_avg) with bins

#Bins from the first report (oct 2014)
rdt_uniq_vts_avg$f <- cut(rdt_uniq_vts_avg$X2013, c(0,10,20,30,40,50,250), labels=c("<=10","11-20","21-30","31-40","41-50",">50"))
barplot(table(rdt_uniq_vts_avg$f), main=paste("Average RDT testing rates of CHWs per month \n in 2013 (mean=",RDTmeanOverall,")",sep=""), xlab= "No. of RDTs", ylab= "No. of CHWs")
table(rdt_uniq_vts_avg$f)
#<=10 11-20 21-30 31-40 41-50   >50 
#1822   371    76    45    36    52 

#experimental bins
#base 2 bin
rdt_uniq_vts_avg$f <- cut(rdt_uniq_vts_avg$X2013, c(0,2^(0:6)[-1],250))
barplot(table(rdt_uniq_vts_avg$f), main=paste("Average RDT testing rates of CHWs per month \n in 2013 (mean=",RDTmeanOverall,")",sep=""), xlab= "No. of RDTs", ylab= "No. of CHWs")
table(rdt_uniq_vts_avg$f)

#0-5, 5-15, 15-35, 35-75 bins
rdt_uniq_vts_avg$f <- cut(rdt_uniq_vts_avg$X2013, c(0,5,15,35,75,250))
barplot(table(rdt_uniq_vts_avg$f), main=paste("Average RDT testing rates of CHWs per month \n in 2013 (mean=",RDTmeanOverall,")",sep=""), xlab= "No. of RDTs", ylab= "No. of CHWs")
table(rdt_uniq_vts_avg$f)

dcast(m_rdt, Source ~ variable, sum) #RDT per IP per year

#Per IP #These are not utilized currently. The boxplot function 7 lines below is used instead.
cda_m_rdt <- m_rdt[m_rdt$Source=="CDA",]
cpi_m_rdt <- m_rdt[m_rdt$Source=="CPI",]
iom_m_rdt <- m_rdt[m_rdt$Source=="IOM",]
mam_m_rdt <- m_rdt[m_rdt$Source=="MAM",]
who_m_rdt <- m_rdt[m_rdt$Source=="WHO",]

#Per IP, preprocessed above for unique CHW with Volunteer name, Township and Source
boxplot(X2013 ~ Source,rdt_uniq_vts_avg, xlab="Implementing partners", ylab="RDTs", main="RDT rates among Implementing Partners (2013)") #Boxplot comparing testing rates between IPs

#Per township [WRONG APPROACH, because the input rdt_uniq_vts_avg, which is already a dcast summary on unique CHWs]
aggregate(X2013 ~ Township, rdt_uniq_vts_avg, mean) #Data cleaning needed to be done for Township names before excuting this!
avgRDT <- rdt_uniq_vts_avg #For RESET
#Name cleaning #This is done at a post-processed dataset. #This should be done in the initial stage.
rdt_uniq_vts_avg$Township[rdt_uniq_vts_avg$Township=="Bawlakhae"] <- "Bawlakhe"
rdt_uniq_vts_avg$Township[rdt_uniq_vts_avg$Township=="Beelin"] <- "Bilin"
rdt_uniq_vts_avg$Township[rdt_uniq_vts_avg$Township=="Hpaan"] <- "Hpa-an"
rdt_uniq_vts_avg$Township[rdt_uniq_vts_avg$Township=="Kyaikhto"] <- "Kyaikto"
rdt_uniq_vts_avg$Township[rdt_uniq_vts_avg$Township=="Kyarin Seikyi"] <- "Kyainseikgyi"
rdt_uniq_vts_avg$Township[rdt_uniq_vts_avg$Township=="Mae Sae"] <- "Mese"
rdt_uniq_vts_avg$Township[rdt_uniq_vts_avg$Township=="Yae"|rdt_uniq_vts_avg$Township=="YE"] <- "Ye"

#rdt_uniq_vts_avg$Township[rdt_uniq_vts_avg$Township==""] <- ""
aggregate(X2013 ~ Township, rdt_uniq_vts_avg, mean)
#plot(aggregate(X2013 ~ Township, rdt_uniq_vts_avg, mean)) #not informative

#===============================================================================================================================
###rdt_uniq_vs variable is now OBSOLETE!!!=======================SKIP THIS=======================================================
#No. of tests performed per CHW (2013)
hist(rdt_uniq_vs$X2013, main="No. of tests performed per CHW (2013)", xlab="No. of tests", ylab="Total VHW")
#Extremelly right-skewed histogram will obtained w/o any data cleaning
summary(sort(rdt_uniq_vs$X2013, decreasing=TRUE))
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.00    6.00   21.00   58.55   65.00 1454.00
#Retrying with a limit of 100 RDTs
hist(rdt_uniq_vs$X2013[rdt_uniq_vs$X2013<100], main="No. of tests performed per CHW (2013)", xlab="No. of tests", ylab="Total VHW")
#Retrying by binning
hist(rdt_uniq_vs$X2013, breaks=c(0,10,20,30,40,50,1455),main="No. of tests performed per CHW (2013)", xlab="No. of tests", ylab="Total VHW")
#====================================================================================================

###SPLITTING Up the data by respective variable(s). THIS METHOD IS NOT AS GOOD AS RESHAPE2 WHERE YOU CAN IDENTIFY COMBINED KEY VARIABLES
v_rdt <- split(rdt, rdt$Volunteer)
s_rdt <- split(rdt, rdt$Source)
vs_rdt <- split(rdt, list(rdt$Volunteer, rdt$Source)) #This is more accurate way of splitting the dataset
#It produces NULL values for even though there's no case under V+S combination

test <- NA #to find out if there are Volunteers with the same name in different IPs.
for(i in 1:length(s_rdt)){
  test[i] <- sum(!duplicated(s_rdt[[i]]$Source))
}

v_rdt[which(test>1)] #There are 70 CHWs with the same name in different IPs.
head(v_rdt[which(test>1)]) #Arkar Soe is the first case (MAM + WHO)

#checking that out in Volunteer + Source (ID'ed)

> head(sort(names(vs_rdt)))
[1] "Aee Nay War.CDA" "Aee Nay War.CPI" "Aee Nay War.IOM" "Aee Nay War.MAM" "Aee Nay War.WHO"
[6] "Aee Tho.CDA"    

rdt[rdt$Volunteer=="Aee Nay War",]

#Creating new RDT dataset with only the active (12 months active) CHW
tmp_rdt <- vs_rdt[sapply(vs_rdt, function(x) nrow(x)>0)] #2307 CHWs
active_chw <- tmp_rdt[sapply(tmp_rdt, function(x) nrow(x)==12)] #only 249 CHW are active all year
active_chw_test_avg <- sapply(active_chw, function(x) mean(x$X2013))
mean(active_chw_test_avg) #14.17269 RDTs are tested per month among all months ACTIVE CHWs (This no. is different from all CHW data)
summary(active_chw_test_avg) #The best one did 103 per month on Average


#There's possibility to explore how much active each CHW is (ie. how many months active)
chw_active_mnth <- sapply(tmp_rdt, function(x) nrow(x))

###now there're cases of CHW who's responsible for several township
#Multiple lines are contributed by a single CHW.
#Thus, it's tricky to find out how efficient a single CHW is. 
tmp_rdt[sapply(tmp_rdt, function(x) nrow(x)>12)]
length(names(tmp_rdt[sapply(tmp_rdt, function(x) nrow(x)>12)])) ##33 CHW with x'ple townships
names(tmp_rdt[sapply(tmp_rdt, function(x) nrow(x)>12)]) #Names of the CHWs with x'ple townships

#Example Results
$`Su Hlaing Hnin.WHO`
State..Division Township      Volunteer Month Year Source X2013
7442             Mon    Mudon Su Hlaing Hnin   Aug 2013    WHO     1
7443             Mon    Mudon Su Hlaing Hnin   Jan 2013    WHO     1
7446             Mon    Mudon Su Hlaing Hnin   May 2013    WHO     1
8949             Mon   Thaton Su Hlaing Hnin   Apr 2013    WHO     2
8953             Mon   Thaton Su Hlaing Hnin   Mar 2013    WHO     1

#and
$`Win Win Htay.WHO`
State..Division             Township    Volunteer Month Year Source X2013
11826     Tanintharyi Tanintharyi Township Win Win Htay   Aug 2013    WHO    10
11827     Tanintharyi Tanintharyi Township Win Win Htay   Sep 2013    WHO     4
12116     Tanintharyi        Thayet Chaung Win Win Htay   Apr 2013    WHO     6
12117     Tanintharyi        Thayet Chaung Win Win Htay   Aug 2013    WHO     8
12127     Tanintharyi        Thayet Chaung Win Win Htay   Sep 2013    WHO    13
12283     Tanintharyi               Ye Byu Win Win Htay   Aug 2013    WHO     3
12284     Tanintharyi               Ye Byu Win Win Htay   Dec 2013    WHO     8

#testing for equality of means between aggregated data and unaggregated one

a <- tmp_rdt$`Win Win Htay.WHO`
> mean(aggregate(X2013 ~ Month, a, sum)[,2])
[1] 17.16667
> mean(a$X2013)
[1] 12.11765

tmp2_rdt <- lapply(tmp_rdt, function(x) aggregate(X2013 ~ Month, x, sum))
#There's possibility to explore how much active each CHW is (ie. how many months active)
chw_active_mnth <- sapply(tmp2_rdt, function(x) nrow(x))
summary(chw_active_mnth)
hist(chw_active_mnth, main="No. of CHWs with the duration they were active in 2013", xlab="# of Months", ylab="# of CHWs") 
