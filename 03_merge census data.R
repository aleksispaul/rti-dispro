
# Notes and to-do list ------------------------------------------------------------------------


# load packages -------------------------------------------------------------------------------
library(tidyverse)

# load data -----------------------------------------------------------------------------------
#618 dataset
df <- read_csv('output datasets/wi and federal combined 618 data.csv')

#aapi paper datasets
sc_est2009_alldata6_all <- read_csv("input aapi paper data and syntax/sc-est2009-alldata6-all.csv")


# paste syntax from AAPI paper ----------------------------------------------------------------

For NHW
data=read.csv("~/Downloads/sc-est2015-alldata6.csv") ##source of file
STATE<-1:56
RACE<-length(unique(data$RACE))
AGE<-6:21 EST<-vector()

7

AAPI Disproportionality Paper Technical Report
RACE.POP<-vector()
TOTAL<-vector()
for(i in 1:56){
  TEMP<-data[which(data$STATE==i), ]
  for(j in 1){
    RACE<-TEMP[which(TEMP$RACE==j), ]
    a<-RACE[which(RACE$AGE&gt;=6&RACE$AGE<=21), ]
    b<-a[which(a$ORIGIN==1), ]
    c<-sum(b$POPESTIMATE2015)
    my.data<-c(b$STATE[1], b$RACE[1], c)
    TOTAL<-rbind(TOTAL, my.data)
  }
}
TOTAL<-as.data.frame(TOTAL)
colnames(TOTAL)<-c("STATE", "RACE", "2015 Population Estimate")
write.csv(TOTAL, "~/Desktop/sc-est2015-alldata6WNH.csv")
For (H+NH AAPI)
data=read.csv("~/Downloads/sc-est2015-alldata6.csv") #source of file
STATE<-1:56
RACE<-length(unique(data$RACE))
AGE<-6:21
RACE.POP<-vector()
TOTAL<-vector()
for(i in 1:56){
  TEMP<-data[which(data$STATE==i), ]
  for(j in 1){
    RACE<-TEMP[which(TEMP$RACE==j), ]
    a<-RACE[which(RACE$AGE&gt;=6&RACE$AGE<=21), ]
    b<-a[which(a$ORIGIN==), ] # first 0, then 1
    c<-sum(b$POPESTIMATE2015)
    my.data<-c(b$STATE[1], b$RACE[4, 5], c)
    TOTAL<-rbind(TOTAL, my.data)
  }
}
TOTAL<-as.data.frame(TOTAL)
colnames(TOTAL)<-c("STATE", "RACE", "2015 Population Estimate")
write.csv(TOTAL, "~/nnnnnn")


