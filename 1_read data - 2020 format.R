#
#     Summary of West Basin trawl data - Ohio and Ontario, years 1987 - present
#       for Forage TG reporting 
#
#     Much of the summarization code is based on SAS code written by Jeff Tyson (I think)
#     This code written by Zak Slagle, ODNR-SARE, Started November 2018
#     This particular code written Jan 2021 to read in data from an old database (Interagency Trawl XP)
#     that has species data and a new format database that has OH and ON combine (Trawl Database through 2020,
#     makes some of "Read Trawl Data.R" irrelevant).


### Load libraries   ##################################
options(warn = -1) # remove unhelpful warnings about libraries being built under old R version

library(magrittr)
library(forcats)
require(RODBC)
require(lubridate)
require(stringr)
#require(here)
#require(xlsx)
require(readxl)
library(tidyverse)

options(warn = 0) #warnings back on


this_year = year(now())-1 #get this year
windowsFonts(Times=windowsFont("Times New Roman"))

#read in species codes/common names and grouping (diff files b/c need to split some age-0 fish into forage groups)
spp_codes <- read_csv("data sources/spp_codes.csv")
forage_groups <- read_csv("data sources/forage fish groups.csv")


### Get newer (2020) data - different database with less data   #####################

trawl_database <- odbcConnectAccess2007("data sources/Interagency Database 2021.accdb")

#list tables in trawl database
sqlTables(trawl_database)

#read in required data from trawl - note that if you don't use as_tibble, sqlFetch() returns an odd
#     tbl with sqlFetch(trawl_database, "OH Lengths") as the one obs
all_lengths_raw <- as_tibble(sqlFetch(trawl_database, "LENGTH"))
all_catch_raw <- as_tibble(sqlFetch(trawl_database, "CATCH"))
all_effort_raw <- as_tibble(sqlFetch(trawl_database, "EFFORT"))

#close connection to DB
odbcCloseAll()


### Reformat data    ##############################

#rename everything to lowercase so col headers match each other
names(all_lengths_raw) %<>% tolower
names(all_catch_raw) %<>% tolower
names(all_effort_raw) %<>% tolower

#reformat effort data to match
all_effort <- all_effort_raw %>%
            mutate(crn = as.character(crn),
                   projcode = as.character(projcode))

#reformat catch data to match
all_catch <- all_catch_raw %>%
              mutate(crn = as.character(crn)) %>%
              rename(agecode = age)


#reformat length data to match 
all_lengths <- all_lengths_raw %>%
                mutate(crn = as.character(crn))

### Fix errors in each dataset ###########################
#
# MAJOR NOTE - ON data does NOT contain uniques CRN's in their crn column. Rather, their "yrsam" appear to be unique crns similar to ours
#                (i.e., YEAR + CRN). Here I merge their yrsam with our crn column, so we have a column of unique IDs by catch record
#
# ALSO NOTE - effstat is trawl status. Status = 1 means the trawl was normal, status != 1 means something got
#             messed up (hung, torn, etc.), so it needs to be excluded. BUT, first we merge effort with
#             catch, so we can also filter out catches from trawls with effstat != 1
#

# fix errors in effort
all_effort <- all_effort %>% mutate(projcode = case_when(projcode %in% c("LEA-IF00_001","LEA_IF00-001") ~ "LEA_IF00_001",
                              T ~ projcode))#fix mismatch between underscores and dashes)

# fix errors in catch
all_catch <- all_catch %>%
  mutate(projcode = case_when(projcode %in% c("LEA-IF00_001","LEA_IF00-001") ~ "LEA_IF00_001",
                              T ~ projcode)) %>% #fix mismatch between underscores and dashes
  left_join(all_effort %>% 
              select(projcode, year, agency, yrsam, crn, depth, effdur, effdist, effstat) %>% 
              unique) %>% #THIS MUST BE UNIQUE VALUES
  filter(year != 1987|effstat == 1|!is.na(effdist))
  

#fix bad projcodes and remove bad trawls (1987, effstat/effdist)
all_lengths <- all_lengths %>% mutate(projcode = case_when(projcode %in% c("LEA-IF00_001","LEA_IF00-001") ~ "LEA_IF00_001",
                                             T ~ projcode)) %>%
  left_join(all_effort %>% select(projcode,  yrsam, crn, year, effdist, effstat) %>% unique) %>% #THIS MUST BE UNIQUE VALUES
  filter(year != 1987, effstat == 1, !is.na(effdist)) %>%
  select(-year, -effdist, -effstat) %>%
  arrange(projcode, crn, species, agecode)



all_effort %>%
  group_by(year, crn) %>%
  tally() %>%
  filter(n>1)

all_effort %>% filter(year == 2021)
