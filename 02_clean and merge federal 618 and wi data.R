
# Overview ------------------------------------------------------------------------------------

#' This file has syntax for cleaning the merged 2005-2018 federal 618 data AND merging Wisconsin DPI
#' provided 618 data. Specifically, this syntax does the following:
#' 1) Cleans Wisconsin data for merging (naming conventions are diff. from federal 618)
#' 2) Cleans federal 618 data (naming conventions changed across time)
#' 3) Merges Wisconsin and federal 618 data.


# load data -----------------------------------------------------------------------------------


# Wisconsin data were cleaned in excel (only 3-years of data) to match the main dataset.
# Code removes the current Wisconsin data from 2016-2018 (all NAs) and replaces it with Wisconsin
# DPI output provided from state.

df <- read_csv('output datasets/combined 618 data for 2005-2018.csv')
wi <- read_csv('input federal open access datasets/wisconsin 618 data request_clean.csv')


df_no_wi16to18 <- filter(df, state=="Wisconsin" & (year==2016 | year==2017 | year==2018))
#' 42 obs pulled from federal 618 vs 36 for Wisconsin data. Due to the following: 
#' (1) no All Disabilities row for WI and
#' (2) No "Multiple disabilities" category reported from Wisconsin.

#Fix naming so it's consistent with 2016-2018 federal 618 data
wi <- wi %>% mutate(
  disability = str_to_sentence(disability),
  disability = ifelse(disability=="Deaf-blind", "Deaf-blindness",
                      ifelse(disability=="Significant developmental delay", "Developmental delay",
                             ifelse(disability=="Emotional behavioral disability", "Emotional disturbance",
                                    ifelse(disability=="Speech language impairment", "Speech or language impairment",
                                           disability))))
)




