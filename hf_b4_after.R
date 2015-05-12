#Health facility data before and after comparison
setwd("~/R/DataRep_report052015")


before <- read.csv("HF_before.csv") #430000 51
after <- read.csv("HF_20150508.csv") #392000 51

names(before)==names(after)

#subsetting before
before_sub <- before[1:392000,]

difference <- NA
for(i in 1:length(before)){
  difference[i] <- sum(before_sub[i]!=after[i])
}