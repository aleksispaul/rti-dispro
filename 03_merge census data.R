
# Notes and to-do list ------------------------------------------------------------------------


# load packages -------------------------------------------------------------------------------
library(tidyverse)

# load data -----------------------------------------------------------------------------------
#618 dataset
df <- read_csv('output datasets/wi and federal combined 618 data.csv')

#aapi paper datasets
data09 <- read_csv("input aapi paper data and syntax/sc-est2009-alldata6-all.csv")
data18 <- read_csv("input aapi paper data and syntax/sc-est2018-alldata6.csv")

# paste syntax from AAPI paper ----------------------------------------------------------------

#For NHW
data = sc_est2009_alldata6_all
data=read.csv("~/Downloads/sc-est2015-alldata6.csv") ##source of file
STATE<-1:56
RACE<-length(unique(data$RACE))
AGE<-6:21 
EST<-vector()

7

#AAPI Disproportionality Paper Technical Report
RACE.POP<-vector()
TOTAL<-vector()
for(i in 1:56){
  TEMP<-data[which(data$STATE==i), ]
  for(j in 1){
    RACE<-TEMP[which(TEMP$RACE==j), ]
    a<-RACE[which(RACE$AGE=6&RACE$AGE<=21), ]
    b<-a[which(a$ORIGIN==1), ]
    c<-sum(b$POPESTIMATE2015)
    my.data<-c(b$STATE[1], b$RACE[1], c)
    TOTAL<-rbind(TOTAL, my.data)
  }
}

TOTAL<-as.data.frame(TOTAL)
colnames(TOTAL)<-c("STATE", "RACE", "2015 Population Estimate")
write.csv(TOTAL, "~/Desktop/sc-est2015-alldata6WNH.csv")

#For (H+NH AAPI)
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




# run 2018 syntax -----------------------------------------------------------------------------

data18c <- data18 %>% 
  mutate( #Creating race_ethnicity variable to merge with 618 data
    race_eth =ifelse(RACE==1, "white",
                       ifelse(RACE==2, "black",
                              ifelse(RACE==3, "aian",
                                     ifelse(RACE>3 & RACE<6, "api",
                                            ifelse(RACE==6, "tworace", "OOOOOOPS"))))),
    race_eth = ifelse(ORIGIN==1, race_eth, #need to add Hispanic separately as it is ethnicity, not race
                      ifelse(ORIGIN==2, "hisp", "TOTAL"))
  ) %>%
  #Grouping sums below by state (state number and NAME of state and race/ethnicity)
  group_by(STATE, NAME, race_eth) %>% 
  filter(AGE>5 & AGE<22, ORIGIN!=0) %>% #Only including 6-21 and NOT totals of races (includes hisp/not hisp)
  summarise(
    pop10 = sum(CENSUS2010POP),
    pop11 = sum(POPESTIMATE2011),
    pop12 = sum(POPESTIMATE2012),
    pop13 = sum(POPESTIMATE2013),
    pop14 = sum(POPESTIMATE2014),
    pop15 = sum(POPESTIMATE2015),
    pop16 = sum(POPESTIMATE2016),
    pop17 = sum(POPESTIMATE2017),
    pop18 = sum(POPESTIMATE2018)
  )


# run 2009 syntax -----------------------------------------------------------------------------

data09c <- data09 %>%
  mutate(
    race_eth =ifelse(RACE==1, "white",
                     ifelse(RACE==2, "black",
                            ifelse(RACE==3, "aian",
                                   ifelse(RACE>3 & RACE<6, "api",
                                          ifelse(RACE==6, "tworace", "OOOOOOPS"))))),
    race_eth = ifelse(ORIGIN==1, race_eth, #need to add Hispanic separately as it is ethnicity, not race
                      ifelse(ORIGIN==2, "hisp", "TOTAL"))
  ) %>%
  #Grouping sums below by state (state number and NAME of state and race/ethnicity)
  group_by(STATE, race_eth) %>% 
  filter(AGE>5 & AGE<22, ORIGIN!=0) %>% #Only including 6-21 and NOT totals of races (includes hisp/not hisp)
  summarise(
   pop04 = sum(POPESTIMATE2004),
   pop05 = sum(POPESTIMATE2005),
   pop06 = sum(POPESTIMATE2006),
   pop07 = sum(POPESTIMATE2007),
   pop08 = sum(POPESTIMATE2008),
   pop09 = sum(POPESTIMATE2009)
  )

pop_data <- full_join(data09c, data18c, by = c("STATE", "race_eth"))


