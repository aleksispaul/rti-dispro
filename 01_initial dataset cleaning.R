
# NOTES -------------------------------------------------------------------

#' This section lists issues, questions, a log of the logic behind computation, 
#' and TO-DOs. 

###' ISSUES
#' 1) In 2008, 7 race/ethnicity (r/e) categories are introduced. States decide which N of categories
#'    they report using 618 data. This introduces issues with linking data across time for r/e.
#'    In addition, we will need to determine a defensible reduction to 5 race/ethnicity categories
#'    to compare trends across time (pre/post 2008).

###' TO-DO
#' 1) COMPLETED. Create consistent vector naming convention for merging across datasets. Reduce name length
#'    for easier analysis.
#' 2) Determine how to merge 5r/e states with 7r/e states.
#' 
#' 3) Merge 5r/e with 7r/e state data.
#' 
#' 4) Load 2011+ CSV files and clean/merge with older data.

# load packages and functions -----------------------------------------------------------

library(tidyverse)

#Function to change columns to numeric for datasets 2005-2011.
col2num <- function(x, cols) {
  x = x
  x[,cols] = lapply(x[,cols], function(y) {as.numeric(gsub(',','', y))})
  x
}

# load original data between 2005 to 2011 ------------------------------------------------------
#' Starting in 2008, the dataset introduces 7 race/ethnicity categories in place of 5. However,
#' the original 5 are also maintained. 
df2005 <- read_csv("input federal open access datasets/bchildcount2005.csv", skip = 4)
df2006 <- read_csv("input federal open access datasets/bchildcount2006.csv", skip = 4) 
df2007 <- read_csv("input federal open access datasets/bchildcount2007.csv", skip = 3) 
df2008 <- read_csv("input federal open access datasets/bchildcount2008.csv", skip = 3)
df2009 <- read_csv("input federal open access datasets/bchildcount2009.csv", skip = 3)
df2010 <- read_csv("input federal open access datasets/bchildcount2010.csv", skip = 3)
df2011 <- read_csv("input federal open access datasets/bchildcount2011.csv", skip = 3)


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

dfs2011 <- df2011[1:826, ] %>% select('Year':'Disability' ,'Age 6 to 21',
                                      'Hispanic/Latino\r\nAge 6 to 21\r\n':'Age 6 to 21\r\n R/E\r\ntotal')


#removing large datasets to clean up global environment.
rm(df2005, df2006, df2007, df2008, df2009, df2010, df2011)


# simplify variable names for 2005-2010 -------------------------------------------------------

dfs2005 <- rename(dfs2005, 
                  year = Year, 
                  state = State, 
                  disability = Disability,
                  total_6to21 = 'Age 6 to 21',
                  aian = 'American Indian or Alaska Native Age 6 to 21',
                  api = 'Asian or Pacific Islander Age 6 to 21',
                  black = 'Black (not Hispanic) Age 6 to 21',
                  hisp = 'Hispanic Age 6 to 21',
                  white = 'White (not Hispanic) Age 6 to 21',
                  total_ethnicity = 'Age 6-21 Ethnicity Totals')

dfs2006 <- rename(dfs2006, 
                  year = Year, 
                  state = State, 
                  disability = Disability,
                  total_6to21 = 'Age 6 to 21',
                  aian = 'American Indian or Alaska Native Age 6 to 21',
                  api = 'Asian or Pacific Islander Age 6 to 21',
                  black = 'Black (not Hispanic) Age 6 to 21',
                  hisp = 'Hispanic Age 6 to 21',
                  white = 'White (not Hispanic) Age 6 to 21',
                  total_ethnicity = 'Age 6-21 Ethnicity Totals')

dfs2007 <- rename(dfs2007, 
                  year = Year, 
                  state = State, 
                  disability = Disability,
                  total_6to21 = 'Age 6 to 21',
                  aian = 'American Indian or Alaska Native Age 6 to 21',
                  api = 'Asian or Pacific Islander Age 6 to 21',
                  black = 'Black (not Hispanic) Age 6 to 21',
                  hisp = 'Hispanic Age 6 to 21',
                  white = 'White (not Hispanic) Age 6 to 21')


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
                  total_ethnicity = 'Age 6 to 21 R/E\r\rtotal',
                  aian = '\r\rAmerican Indian \r\ror Alaska Native \r\rAge 6 to 21',
                  asian7 = 'Asian \r\rAge 6 to 21',
                  black = 'Black or \r\rAfrican American\r\rAge 6 to 21',
                  hisp = 'Latino or Hispanic\r\rAge 6 to 21\r\r',
                  nhpi7 = 'Native Hawaiian\r\ror Other Pacific Islander\r\rAge 6 to 21',
                  white = 'White\r\rAge 6 to 21',
                  tworace = 'Two or more races\r\rAge 6 to 21')

dfs2011 <- rename(dfs2011,
                  year = Year,
                  state = State,
                  disability = Disability,
                  total_6to21 = 'Age 6 to 21',
                  total_ethnicity = 'Age 6 to 21\r\n R/E\r\ntotal',
                  aian = '\r\n\r\nAmerican Indian \r\nor Alaska Native \r\nAge 6 to 21 \r\n',
                  asian7 = 'Asian \r\nAge 6 to 21 \r\n',
                  black = 'Black or \r\nAfrican American\r\nAge 6 to 21 \r\n',
                  hisp = 'Hispanic/Latino\r\nAge 6 to 21\r\n',
                  nhpi7 = 'Native Hawaiian \r\nor Other Pacific Islander\r\nAge 6 to 21\r\n',
                  white = 'White\r\nAge 6 to 21\r\n',
                  tworace = 'Two or more races\r\nAge 6 to 21 \r\n')


# adjust variable types and preprocess for merge 2005-2010 ------------------------------------

#' Function that converts a range of columns from character to numeric by 
#' removing the comma and then converting. NAs are removing the "x" that 618 data has for missing
#' values.

#This function moved to the top of syntax.
# col2num <- function(x, cols) {
#   x = x
#   x[,cols] = lapply(x[,cols], function(y) {as.numeric(gsub(',','', y))})
#   x
# }

#Apply the above function to datasets from 2005-2010
dfs2005 <- col2num(dfs2005, 4:10)
dfs2006 <- col2num(dfs2006, 4:10)
dfs2007 <- col2num(dfs2007, 4:9)
dfs2008 <- col2num(dfs2008, 4:18)
dfs2009 <- col2num(dfs2009, 5:19)
dfs2010 <- col2num(dfs2010, 4:12)
dfs2011 <- col2num(dfs2011, 4:12)

dfs2007$total_ethnicity <- rowSums(dfs2007[,5:9], na.rm=TRUE)



# merge 5r/e with 7r/e categories -------------------------------------------------------------
sum_re_cat <- function(x1, x2){
  rowSums(cbind(x1, x2), na.rm=T) + ifelse(is.na(x1) & is.na(x2), NA, 0)
}
  

dfs2008 <- dfs2008 %>% mutate(
  aian = sum_re_cat(aian5, aian7),
  api = rowSums(cbind(api5, asian7, nhpi7), na.rm=T) + 
    ifelse(is.na(api5) & is.na(asian7) & is.na(nhpi7), NA, 0),
  black = sum_re_cat(black5, black7),
  hisp = sum_re_cat(hisp5, hisp7),
  white = sum_re_cat(white5, white7),
  tworace = tworace7,
  total_ethnicity = sum_re_cat(total_ethnicity5, total_ethnicity7)
)

dfs2009 <- dfs2009 %>% mutate(
  aian = sum_re_cat(aian5, aian7),
  api = rowSums(cbind(api5, asian7, nhpi7), na.rm=T) + 
    ifelse(is.na(api5) & is.na(asian7) & is.na(nhpi7), NA, 0),
  black = sum_re_cat(black5, black7),
  hisp = sum_re_cat(hisp5, hisp7),
  white = sum_re_cat(white5, white7),
  tworace = tworace7,
  total_ethnicity = sum_re_cat(total_ethnicity5, total_ethnicity7)
)

dfs2010 <- dfs2010 %>% mutate(
  api = sum_re_cat(asian7, nhpi7)
)

dfs2011 <- dfs2011 %>% mutate(
  api = sum_re_cat(asian7, nhpi7)
)

write_csv(dfs2005, 'output datasets/section 618 data for 2005.csv')
write_csv(dfs2006, 'output datasets/section 618 data for 2006.csv')
write_csv(dfs2007, 'output datasets/section 618 data for 2007.csv')
write_csv(dfs2008, 'output datasets/section 618 data for 2008.csv')
write_csv(dfs2009, 'output datasets/section 618 data for 2009.csv')
write_csv(dfs2010, 'output datasets/section 618 data for 2010.csv')
write_csv(dfs2011, 'output datasets/section 618 data for 2011.csv')

# Remove extraneous 5re and 7re categories prior to merge for 2008-2011
df08 <- select(dfs2008, year, state, disability, total_6to21, aian, api, black, hisp, white,
               tworace, total_ethnicity)

df09 <- select(dfs2009, year, state, disability, total_6to21, aian, api, black, hisp, white,
               tworace, total_ethnicity)

df10 <- select(dfs2010, year, state, disability, total_6to21, aian, api, black, hisp, white,
               tworace, total_ethnicity)

df11 <- select(dfs2011, year, state, disability, total_6to21, aian, api, black, hisp, white,
               tworace, total_ethnicity)


df0506 <- bind_rows(dfs2005, dfs2006)
df0507 <- bind_rows(df0506, dfs2007)  
df0508 <- bind_rows(df0507, df08)
df0509 <- bind_rows(df0508, df09)
df0510 <- bind_rows(df0509, df10)
df0511 <- bind_rows(df0510, df11)

rm(dfs2005, dfs2006, dfs2007, dfs2008, dfs2009, dfs2010, dfs2011,
   df0506, df0507, df0508, df0509, df0510, df08, df09, df10, df11)



# load original data from 2012+  ------------------------------------------------------------

df2012 <- read_csv("input federal open access datasets/bchildcountandedenvironments2012.csv", 
                   skip = 4)
df2013 <- read_csv("input federal open access datasets/bchildcountandedenvironments2013.csv", 
                   skip = 4)
df2014 <- read_csv("input federal open access datasets/bchildcountandedenvironments2014.csv", 
                   skip = 4)
df2015 <- read_csv("input federal open access datasets/bchildcountandedenvironments2015.csv", 
                   skip = 4)
df2016 <- read_csv("input federal open access datasets/bchildcountandedenvironments2016.csv", 
                   skip = 3)
df2017 <- read_csv("input federal open access datasets/bchildcountandedenvironments2017-18.csv", 
                   skip = 4)
df2018 <- read_csv("input federal open access datasets/bchildcountandedenvironments2018-19.csv", 
                   skip = 4)


# restrict dataframes to columns and rows needed for analysis ---------------------------------

#' The code below will select the Age 6 to 21 totals overall, by ethnicity, and by total disability.
#' The additional rows for environment code will be removed, preserving only the 6 to 21 counts by
#' each state.

filter_and_select <- function(x) {
  x %>% 
    filter(`SEA Education Environment` == "Total, Age 6-21") %>%
              # The below code (if activated) will remove developmental delay from counts.
               # `SEA Disability Category` != 
               #   "Developmental delay (valid only for children ages 3-9 when defined by state)") %>%
    select(Year,
           'State Name',
           'SEA Disability Category',
           'Ages 6-21':'LEP No Age 6 to 21',
           'American Indian or Alaska Native Age 6 to21':'White Age 6 to21')
}

dfs2012 <- filter_and_select(df2012)
dfs2013 <- filter_and_select(df2013)
dfs2014 <- filter_and_select(df2014)
dfs2015 <- filter_and_select(df2015)
dfs2016 <- filter_and_select(df2016)
dfs2017 <- filter_and_select(df2017)




#In 2018, LEP was switched to EL.
dfs2018 <- df2018 %>% filter(`SEA Education Environment` == "Total, Age 6-21") %>%
  select(Year,
         'State Name',
         'SEA Disability Category',
         'Ages 6-21':'EL No Age 6 to 21',
         'American Indian or Alaska Native Age 6 to21':'White Age 6 to21')

# Change character.vars to numeric.vars where appropriate

#Removed gsub argument, as the 2012-2018 datasets did not have commas.

col2num_v2 <- function(x, cols) {
  x = x
  x[,cols] = lapply(x[,cols], function(y) {as.numeric(y)})
  x
}

#NAs introduced are of 1 of 3 varieties:
#' "-" for data that were not available
#' "x" for data that were suppressed
#' "*" for data that were deemed unreliable

dfs2012 <- col2num_v2(dfs2012, 4:13) 
dfs2013 <- col2num_v2(dfs2013, 4:13)
dfs2014 <- col2num_v2(dfs2014, 4:13)
dfs2015 <- col2num_v2(dfs2015, 4:13)
dfs2016 <- col2num_v2(dfs2016, 4:13)
dfs2017 <- col2num_v2(dfs2017, 4:13)
dfs2018 <- col2num_v2(dfs2018, 4:13)


# adjust variable types and preprocess for merge ----------------------------------------------

#' Function to cleanup variable names. 2012-2018 had consistent naming conventions!
clean_var_names <- function(x) {
  rename(x,
         year = Year,
         state = `State Name`,
         disability = 'SEA Disability Category',
         total_6to21 = 'Ages 6-21',
         el_yes = 'LEP Yes Age 6 to 21',
         el_no = 'LEP No Age 6 to 21',
         aian = "American Indian or Alaska Native Age 6 to21",
         asian7 = "Asian Age 6 to21",
         black = "Black or African American Age 6 to21",
         hisp = "Hispanic/Latino Age 6 to21",
         nhpi7 = "Native Hawaiian or Other Pacific Islander Age 6 to21",
         white = "White Age 6 to21",
         tworace = "Two or more races Age 6 to21"
         ) %>% 
    mutate(api = rowSums(cbind(asian7, nhpi7), na.rm=T) + 
             ifelse(is.na(asian7) & is.na(nhpi7), NA, 0),
           total_ethnicity = rowSums(cbind(aian, api, black, hisp, white, tworace), na.rm=T)) %>%
    select(-asian7, -nhpi7)
}


dfs2012 <- clean_var_names(dfs2012)
dfs2013 <- clean_var_names(dfs2013)
dfs2014 <- clean_var_names(dfs2014)
dfs2015 <- clean_var_names(dfs2015)
dfs2016 <- clean_var_names(dfs2016)
dfs2017 <- clean_var_names(dfs2017)



# 2018 data treated separately because of change from LEP to EL in variable.
dfs2018 <-  rename(dfs2018,
                   year = Year,
                   state = `State Name`,
                   disability = 'SEA Disability Category',
                   total_6to21 = 'Ages 6-21',
                   el_yes = 'EL Yes Age 6 to 21',
                   el_no = 'EL No Age 6 to 21',
                   aian = "American Indian or Alaska Native Age 6 to21",
                   asian7 = "Asian Age 6 to21",
                   black = "Black or African American Age 6 to21",
                   hisp = "Hispanic/Latino Age 6 to21",
                   nhpi7 = "Native Hawaiian or Other Pacific Islander Age 6 to21",
                   white = "White Age 6 to21",
                   tworace = "Two or more races Age 6 to21") %>% 
  mutate(api = rowSums(cbind(asian7, nhpi7), na.rm=T) + 
      ifelse(is.na(asian7) & is.na(nhpi7), NA, 0),
      total_ethnicity = rowSums(cbind(aian, api, black, hisp, white, tworace), na.rm=T)) %>% 
  select(-asian7, -nhpi7)

#Pull DD category into separate dataset to determine if included or excluded from later analysis.

dd_state_data <- function() {
  x  <- bind_rows(dfs2012, dfs2013) 
  x2 <- bind_rows(dfs2014, dfs2015)
  x3 <- bind_rows(dfs2016, dfs2017)
  x_combined <- bind_rows(x, x2)
  x_combined <- bind_rows(x_combined, x3)
  x_combined <- bind_rows(x_combined, dfs2018) %>%
  filter(disability %in% c("Developmental delay", 
           "Developmental delay (valid only for children ages 3-9 when defined by state)")) %>%
  select(year, state, disability, total_6to21)

}

dd_state <- dd_state_data()
#write_csv(dd_state, 'output datasets/developmental delay data for 2012-18.csv')





write_csv(dfs2012, 'output datasets/section 618 data for 2012.csv')
write_csv(dfs2013, 'output datasets/section 618 data for 2013.csv')
write_csv(dfs2014, 'output datasets/section 618 data for 2014.csv')
write_csv(dfs2015, 'output datasets/section 618 data for 2015.csv')
write_csv(dfs2016, 'output datasets/section 618 data for 2016.csv')
write_csv(dfs2017, 'output datasets/section 618 data for 2017.csv')
write_csv(dfs2018, 'output datasets/section 618 data for 2018.csv')

#Merge 2012 through 2018 datasets
dfs1213 <- bind_rows(dfs2012, dfs2013)
dfs1214 <- bind_rows(dfs1213, dfs2014)
dfs1215 <- bind_rows(dfs1214, dfs2015)
dfs1216 <- bind_rows(dfs1215, dfs2016)
dfs1217 <- bind_rows(dfs1216, dfs2017)
dfs1218 <- bind_rows(dfs1217, dfs2018)



# merge 2005 through 2018 datasets ------------------------------------------------------------

#Switch year variabe to numeric for 2005-2011 for merging with 2012-18.
df0511$year <- as.numeric(df0511$year)

#merge 2005 through 2018
df2005_to_2018 <- bind_rows(df0511, dfs1218) 

#Change states to titlecase for consistency and sorting
df2005_to_2018$state <- str_to_title(df2005_to_2018$state)

#Arrange dataset so it is by state and each successive year.
df2005_to_2018 <- df2005_to_2018 %>% arrange(state, year)

#Pull aggregate state data to separate dataset
state_aggregate <- df2005_to_2018[grepl("50 States",df2005_to_2018$state), ]

#Create new dataframe without the aggregate information.
df05_to_18 <- df2005_to_2018[!grepl("50 States",df2005_to_2018$state), ]


write_csv(state_aggregate, 'output datasets/aggregate national 618 data 2005-2018.csv')
write_csv(df05_to_18, 'output datasets/combined 618 data for 2005-2018.csv')


