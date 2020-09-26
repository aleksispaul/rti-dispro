
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
#'    for easier typing/analysis.
#' 2) Determine how to merge 5r/e states with 7r/e states.
#' 
#' 3) Merge 5r/e with 7r/e state data.

# load packages -----------------------------------------------------------

library(tidyverse)
library(readxl)

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


