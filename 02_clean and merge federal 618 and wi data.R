
# Overview ------------------------------------------------------------------------------------

#' This file has syntax for cleaning the merged 2005-2018 federal 618 data AND merging Wisconsin DPI
#' provided 618 data. Specifically, this syntax does the following:
#' 1) Cleans Wisconsin data for merging (naming conventions are diff. from federal 618) = DONE
#' 2) Cleans federal 618 data (disab naming conventions changed across time) = In PROG
#' 3) Merges Wisconsin and federal 618 data. = DONE


# load data -----------------------------------------------------------------------------------


# Wisconsin data were cleaned in excel (only 3-years of data) to match the main dataset. Changes were:
#' (1) Addition of "All Disabilities" row for each year (2016, 2017, 2018) that was equal to sum of 
#' the categories under it for that year.
#' (2) Merging of nhpi and asian categories to api (faster in excel than R code, with limited room 
#' for error due to the small N of rows).
# Code removes the current Wisconsin data from 2016-2018 (all NAs) and replaces it with Wisconsin
# DPI output provided from state.

df <- read_csv('output datasets/combined 618 data for 2005-2018.csv')
wi <- read_csv('input federal open access datasets/wisconsin 618 data request_clean.csv')


df_only_wi16to18 <- filter(df, state=="Wisconsin" & (year==2016 | year==2017 | year==2018))
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

#Exclude the 42 rows of Wisconsin data to prep for merging in new WI data (39 rows, no multi-disab)
df_no_wi16to18 <- df %>% filter(state!="Wisconsin" | year!=2016, 
                                state!="Wisconsin" | year!=2017,
                                state!="Wisconsin" | year!=2018)

df_all <- bind_rows(df_no_wi16to18, wi) #Difference of 3-rows from original data. Merge successful.


# clean disability categories -----------------------------------------------------------------

df_all <- df_all %>% mutate(
  disability = str_to_sentence(disability),
  disability = ifelse(
    grepl("mental retar", disability, ignore.case=T) | 
      grepl("intellectual disab", disability, ignore.case = T), "ID",
    ifelse(
      grepl("hearing", disability, ignore.case=T), "HI",
      ifelse(
        grepl("Speech", disability, ignore.case=T), "SLI",
        ifelse(
          grepl("Visual", disability, ignore.case=T), "VI",
          ifelse(
            grepl("ortho", disability, ignore.case=T), "OI",
            ifelse(
              grepl("Other health", disability, ignore.case=T), "OHI",
              ifelse(
                grepl("Specific learning", disability, ignore.case=T), "SLD",
                ifelse(
                  grepl("Deaf-bli", disability, ignore.case=T), "DB",
                  ifelse(
                    grepl("Multiple", disability, ignore.case=T), "MI",
                    ifelse(
                      grepl("Developmental", disability, ignore.case=T), "DD",
                      ifelse(
                        grepl("All disab", disability, ignore.case=T), "All disabilities", 
                        ifelse(disability=="Emotional disturbance", "ED",
                               ifelse(disability=="Autism", "Aut",
                                      ifelse(disability=="Traumatic brain injury", "TBI", disability
                                      )
                               )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

write_csv(df_all,"output datasets/wi and federal combined 618 data.csv")
