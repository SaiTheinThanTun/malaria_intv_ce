#Health facility data before and after comparison
setwd("~/R/DataRep_report052015")


before <- read.csv("HF_before.csv", stringsAsFactors=FALSE) #430000 51
after <- read.csv("HF_20150508.csv", stringsAsFactors=FALSE) #392000 51

#subsetting before
before_sub <- before[1:392000,]

difference <- NA
for(i in 1:length(before)){
  difference[i] <- sum(before_sub[i]!=after[i],na.rm=TRUE)
}
names(difference) <- names(after)
difference

#subsetting without very inconsistent variables such as State..Divisions, Township
before_ <- before[-c(4,6)]
after_ <- after[-c(4,6)]
write.csv(before_,"before_.csv")
write.csv(after_,"after_.csv")

#other testings
names(before)==names(after) #all TRUE
b4class <- sapply(before,class)
aftclass <- sapply(after,class)
b4class == aftclass #all TRUE


#Checking the first variable
b4diff <- before_sub[before_sub[1]!=after[1],]
aftdiff <- after[before_sub[1]!=after[1],]
write.csv(b4diff,"b4diff.csv")
write.csv(aftdiff,"aftdiff.csv")

#below doesn't work
before_sub_ <- NA
for(i in 1:ncol(before_sub)){
  before_sub_[i] <- as.character(before_sub[i])
}
after_ <- NA
for(i in 1:ncol(after)){
  after_[i] <- as.character(after[i])
}