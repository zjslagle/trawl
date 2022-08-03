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
require(readxl)
library(tidyverse)

options(warn = 0) #warnings back on


this_year = year(now())-1 #get this year
windowsFonts(Times=windowsFont("Times New Roman"))

#read in species codes/common names and grouping (diff files b/c need to split some age-0 fish into forage groups)
spp_codes <- read_csv("data sources/spp_codes.csv")
forage_groups <- read_csv("data sources/forage fish groups.csv")


### Get newer (2020) data - different database with less data   #####################
# Need to use the "parsed" database - OMNRF initially classifies many of their intermediate-sized fish as agecode 5, then
# parses/allocates these fish afterwards into agecode 1 or 3 based on... length I guess? Need to ask Andy Cook.  
trawl_database <- odbcConnectAccess2007("data sources/ON21parse_OH21_ERIEWEST_TRAWL_IN_OH_FMT_Nov23.accdb")

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

####### Error Catching - Do keys (i.e., YRSAM and CRN) align across tables?    #################
# are CRNs unique?
all_effort %>%
  group_by(year, crn) %>%
  tally() %>%
  filter(n>1)

#mismatches b/t effort and catch - these will be filtered out/not used in summary!
# these samples appear in catch but NOT effort
all_catch %>% 
  select(year, crn, projcode, agency, yrsam) %>% 
  anti_join(all_effort) %>% 
  select(yrsam) %>% unique %>% view

#vice versa
all_effort %>% 
  filter(effstat == 1) %>% # there are many zero catches for effstat != 1
  select(year, crn, projcode, agency, yrsam) %>% 
  anti_join(all_catch) %>% 
  group_by(yrsam) %>% summarize(count = n())

all_lengths %>% 
  select(crn, projcode, agency, yrsam) %>% 
  anti_join(all_effort) %>% 
  group_by(yrsam) %>% summarize(count = n())

### Fix errors and make joins in each dataset ###########################
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
              select(projcode, year, agency, yrsam, crn, depth, effdur, effdist, effstat, botoxy) %>% 
              unique) %>%                                              #THESE MUST BE UNIQUE VALUES
  mutate(effstat = if_else(botoxy < 2 & year > 2008, 0, effstat)) %>%  #SAS note: TEMPORARY MEASURE TO HANDLE LOW DO 2009-2011
  filter(year != 1987|effstat == 1|!is.na(effdist)) %>%
  mutate(agecode = case_when(species == 366 ~ 5, #manually fix mismatches b/t OH and ON agecodes
                             species == 342 ~ 5,
                             T ~ agecode)) 
  

#fix bad projcodes and remove bad trawls (1987, effstat/effdist)
all_lengths <- all_lengths %>% mutate(projcode = case_when(projcode %in% c("LEA-IF00_001","LEA_IF00-001") ~ "LEA_IF00_001",
                                             T ~ projcode)) %>%
  left_join(all_effort %>% select(projcode,  yrsam, crn, year, effdist, effstat, botoxy) %>% unique) %>% #THIS MUST BE UNIQUE VALUES
  mutate(effstat = if_else(botoxy < 2 & year > 2008, 0, effstat)) %>%  #SAS note: TEMPORARY MEASURE TO HANDLE LOW DO 2009-2011
  filter(year != 1987, effstat == 1, !is.na(effdist)) %>%
  #left_join(spp_codes) %>%
  select(-year, -effdist, -effstat) %>%
  arrange(projcode, crn, species, agecode)%>%
  mutate(agecode = case_when(species == 366 ~ 5, #manually fix mismatches b/t OH and ON agecodes
                             species == 342 ~ 5,
                             T ~ agecode)) 


# calc number of good trawls
n_good_trawls = all_effort %>% filter(effstat == 1) %>% select(yrsam) %>% n_distinct()

### Look at age class mismatches - count(not sums) of occurrence
all_catch %>%
  #filter(year == 2021) %>%
  xtabs(~species+agecode+agency, data = .)

# as of 2021, looks like only real difference is logperch (342) is counted as AC 5, while we split into AC 1 and 3
# this has now been fixed (manual mismatches, above)
