
# NOTES -------------------------------------------------------------------

#' This section lists issues, questions, a log of the logic behind computation, 
#' and TO-DOs. 

###' ISSUES
#' 1) In 2008, 7 race/ethnicity (r/e) categories are introduced. States decide which N of categories
#'    they report using 618 data. This introduces issues with linking data across time for r/e.
#'    In addition, we will need to determine a defensible reduction to 5 race/ethnicity categories
#'    to compare trends across time (pre/post 2008).

###' TO-DO
#' 1) Create consistent vector naming convention for merging across datasets. Reduce name length
#'    for easier analysis.
#' 2) Determine how to merge 5r/e states with 7r/e states.
#' 
#' 3) Merge 5r/e with 7r/e state data.

# load packages -----------------------------------------------------------

library(tidyverse)

# load original data ------------------------------------------------------
#' Starting in 2008, the dataset introduces 7 race/ethnicity categories in place of 5. However,
#' the original 5 are also maintained. 
df2005 <- read_csv("federal open access datasets/bchildcount2005.csv", skip = 4)
df2006 <- read_csv("federal open access datasets/bchildcount2006.csv", skip = 4) 
df2007 <- read_csv("federal open access datasets/bchildcount2007.csv", skip = 3) 
df2008 <- read_csv("federal open access datasets/bchildcount2008.csv", skip = 3)
df2009 <- read_csv("federal open access datasets/bchildcount2009.csv", skip = 3)
df2010 <- read_csv("federal open access datasets/bchildcount2010.csv", skip = 3)


# create skinny datasets of original data -----------------------------------------------------

#' The function below only works for 05 and 06. It selects the variables needed and then converts
#' the appropriate vectors that need to be converted to numeric and accomplishes this task.
skinny05to06 <- function(x) {
  x = x[1:826, ] %>% select('Year':'Disability', 'Age 6 to 21':'Age 6-21 Ethnicity Totals')
}

dfs2005 <- skinny05to06(df2005)
dfs2006 <- skinny05to06(df2006)
rm(skinny05to06)

#' In 2007, the ethnicity total was excluded. Beginning in 2008, the ethnicity total is different than
#' the overall total. I will create a variable that totals the ethnicity totals for 2007.
dfs2007 <- df2007[1:826, ] %>% select('Year':'Disability', 'Age 6 to 21':'White (not Hispanic) Age 6 to 21')

#' Starting in 2008, the dataset introduces 7 race/ethnicity categories in place of 5. 
#' States differ on their use of 5 or 7 categories, so I will maintain both for now. We will need to
#' combine these for analysis in order to maintain an ability to track enrollment across time. 

dfs2008 <- df2008[1:826, ] %>% select('Year':'Disability', 'Age 6 to 21', 
                                      'American Indian or Alaska Native Age 6 to 21\r\n(5 r/e \r\ncategories)':
                                        'Age 6-21\r\n(7 r/e \r\ncategories) \r\ntotals')

#2009 introduced a column that indicates whether the state used 5 or 7 r/e indicators.
dfs2009 <- df2009[1:826, ] %>% select('Year':'Disability', 'Age 6 to 21', 
                                      'American Indian or Alaska Native Age 6 to 21\r\n(5 r/e \r\ncategories)':
                                        'Age 6 to 21\r\n(7 r/e \r\ncategories) \r\ntotals')

#In 2010, all states report 7 ethnicity categories, so the r/e indicator column is removed and the
# dataset more closely resembles the 2005-06 datasets.
dfs2010 <- df2010[1:826, ] %>% select('Year':'Disability', 'Age 6 to 21',
                                      'Latino or Hispanic\r\rAge 6 to 21\r\r':'Age 6 to 21 R/E\r\rtotal')

#removing large datasets to clean up global environment.
rm(df2005, df2006, df2007, df2008, df2009, df2010)


# simplify variable names for 2005-2010 -------------------------------------------------------

dfs2005 <- rename(dfs2005, 
                  year = Year, 
                  state = State, 
                  disability = Disability,
                  total_6to21 = 'Age 6 to 21',
                  aian5 = 'American Indian or Alaska Native Age 6 to 21',
                  api5 = 'Asian or Pacific Islander Age 6 to 21',
                  black5 = 'Black (not Hispanic) Age 6 to 21',
                  hisp5 = 'Hispanic Age 6 to 21',
                  white5 = 'White (not Hispanic) Age 6 to 21',
                  total_ethnicity5 = 'Age 6-21 Ethnicity Totals')

dfs2006 <- rename(dfs2006, 
                  year = Year, 
                  state = State, 
                  disability = Disability,
                  total_6to21 = 'Age 6 to 21',
                  aian5 = 'American Indian or Alaska Native Age 6 to 21',
                  api5 = 'Asian or Pacific Islander Age 6 to 21',
                  black5 = 'Black (not Hispanic) Age 6 to 21',
                  hisp5 = 'Hispanic Age 6 to 21',
                  white5 = 'White (not Hispanic) Age 6 to 21',
                  total_ethnicity5 = 'Age 6-21 Ethnicity Totals')

dfs2007 <- rename(dfs2007, 
                  year = Year, 
                  state = State, 
                  disability = Disability,
                  total_6to21 = 'Age 6 to 21',
                  aian5 = 'American Indian or Alaska Native Age 6 to 21',
                  api5 = 'Asian or Pacific Islander Age 6 to 21',
                  black5 = 'Black (not Hispanic) Age 6 to 21',
                  hisp5 = 'Hispanic Age 6 to 21',
                  white5 = 'White (not Hispanic) Age 6 to 21')


dfs2008 <- rename(dfs2008,
                  year = Year,
                  state = State,
                  disability = Disability,
                  total_6to21 = 'Age 6 to 21',
                  total_ethnicity5 = 'Age 6-21\r\n(5 r/e categories)\r\n totals',
                  total_ethnicity7 = 'Age 6-21\r\n(7 r/e \r\ncategories) \r\ntotals',
                  aian5 = 'American Indian or Alaska Native Age 6 to 21\r\n(5 r/e \r\ncategories)',
                  aian7 = '\r\n\r\nAmerican Indian \r\nor Alaska Native \r\nAge 6 to 21 \r\n(7 R/E\r\n categories)',
                  asian7 = 'Asian \r\nAge 6 to 21 \r\n(7 R/E categories)',
                  api5 = 'Asian or Pacific Islander \r\nAge 6 to 21\r\n(5 r/e categories)',
                  black5 = 'Black (not Hispanic) \r\nAge 6 to 21\r\n(5 r/e categories)',
                  black7 = 'Black or \r\nAfrican American\r\nAge 6 to 21 \r\n(7 R/E \r\ncategories)',
                  hisp5 = 'Hispanic \r\nAge 6 to 21\r\n(5 r/e categories)',
                  hisp7 = 'Latino or Hispanic \r\nAge 6 to 21\r\n(7 R/E categories)',
                  nhpi7 = 'Native Hawaiian \r\nor Pacific Islander\r\nAge 6 to 21\r\n(7 R/E \r\ncategories)',
                  tworace7 = 'Two or more races\r\nAge 6 to 21 \r\n(7 R/E categories)',
                  white5 = 'White (not Hispanic)\r\n Age 6 to 21\r\n(5 r/e categories)',
                  white7 = 'White\r\nAge 6 to 21\r\n(7 R/E \r\ncategories)')

dfs2009 <- rename(dfs2009,
                  year = Year,
                  state = State,
                  disability = Disability,
                  total_6to21 = 'Age 6 to 21',
                  total_ethnicity5 = 'Age 6-21\r\n(5 r/e categories)\r\n totals',
                  total_ethnicity7 = 'Age 6 to 21\r\n(7 r/e \r\ncategories) \r\ntotals',
                  aian5 = 'American Indian or Alaska Native Age 6 to 21\r\n(5 r/e \r\ncategories)',
                  aian7 = '\r\n\r\nAmerican Indian \r\nor Alaska Native \r\nAge 6 to 21 \r\n(7 r/e\r\n categories)',
                  asian7 = 'Asian \r\nAge 6 to 21 \r\n(7 r/e categories)',
                  api5 = 'Asian or Pacific Islander \r\nAge 6 to 21\r\n(5 r/e categories)',
                  black5 = 'Black (not Hispanic) \r\nAge 6 to 21\r\n(5 r/e categories)',
                  black7 = 'Black or \r\nAfrican American\r\nAge 6 to 21 \r\n(7 r/e \r\ncategories)',
                  hisp5 = 'Hispanic \r\nAge 6 to 21\r\n(5 r/e categories)',
                  hisp7 = 'Latino or Hispanic \r\nAge 6 to 21\r\n(7 r/e categories)',
                  nhpi7 = 'Native Hawaiian \r\nor Pacific Islander\r\nAge 6 to 21\r\n(7 r/e \r\ncategories)',
                  tworace7 = 'Two or more races\r\nAge 6 to 21 \r\n(7 r/e categories)',
                  white5 = 'White (not Hispanic)\r\n Age 6 to 21\r\n(5 r/e categories)',
                  white7 = 'White\r\nAge 6 to 21\r\n(7 r/e \r\ncategories)')            
                
dfs2010 <- rename(dfs2010,
                  year = Year,
                  state = State,
                  disability = Disability,
                  total_6to21 = 'Age 6 to 21',
                  total_ethnicity7 = 'Age 6 to 21 R/E\r\rtotal',
                  aian7 = '\r\rAmerican Indian \r\ror Alaska Native \r\rAge 6 to 21',
                  asian7 = 'Asian \r\rAge 6 to 21',
                  black7 = 'Black or \r\rAfrican American\r\rAge 6 to 21',
                  hisp7 = 'Latino or Hispanic\r\rAge 6 to 21\r\r',
                  nhpi7 = 'Native Hawaiian\r\ror Other Pacific Islander\r\rAge 6 to 21',
                  white7 = 'White\r\rAge 6 to 21',
                  tworace7 = 'Two or more races\r\rAge 6 to 21')


# adjust variable types and preprocess for merge 2005-2010 ------------------------------------

#' Function that converts a range of columns from character to numeric by 
#' removing the comma and then converting. NAs are removing the "x" that 618 data has for missing
#' values.

col2num <- function(x, cols) {
  x = x
  x[,cols] = lapply(x[,cols], function(y) {as.numeric(gsub(',','', y))})
  x
}

#Apply the above function to datasets from 2005-2010
dfs2005 <- col2num(dfs2005, 4:10)
dfs2006 <- col2num(dfs2006, 4:10)
dfs2007 <- col2num(dfs2007, 4:9)
dfs2008 <- col2num(dfs2008, 4:18)
dfs2009 <- col2num(dfs2009, 5:19)
dfs2010 <- col2num(dfs2010, 4:12)

dfs2007$total_ethnicity5 <- rowSums(dfs2007[,5:9], na.rm=TRUE)

write_csv(dfs2005, 'output datasets/section 618 data for 2005.csv')
write_csv(dfs2006, 'output datasets/section 618 data for 2006.csv')
write_csv(dfs2007, 'output datasets/section 618 data for 2007.csv')
write_csv(dfs2008, 'output datasets/section 618 data for 2008.csv')
write_csv(dfs2009, 'output datasets/section 618 data for 2009.csv')
write_csv(dfs2010, 'output datasets/section 618 data for 2010.csv')

df0506 <- bind_rows(dfs2005, dfs2006)
df0507 <- bind_rows(df0506, dfs2007)  
df0508 <- bind_rows(df0507, dfs2008)
df0509 <- bind_rows(df0508, dfs2009)
df0510 <- bind_rows(df0509, dfs2010)

rm(df0506, df0507, df0508, df0509)

