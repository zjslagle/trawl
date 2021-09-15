#
#     Summary of West Basin trawl data - Ohio and Ontario, years 1987 - present
#       for Forage TG reporting 
#
#     Much of the processing is based on SAS code written by Jeff Tyson (I think)
#     This code written by Zak Slagle, ODNR-SARE, Started November 2018
#yet another test for github

### Load libraries   ##################################
library(magrittr)
library(forcats)
require(RODBC)
require(lubridate)
require(stringr)
require(here)
#require(xlsx)
require(readxl)
library(tidyverse)


this_year = year(now())-1 #get this year
windowsFonts(Times=windowsFont("Times New Roman"))

### Connect to Trawl MSAccess database, download required data  ############################
#open connection to trawl database
#trawl_database <- odbcConnectAccess2007("//s.dnr.isi.oitfs.ohio.gov/DNRShares/Sandusky Fisheries Research/DuFour/1_Projects/1_Trawling/3_Database/Interagency Trawl XP.mdb")

# work from home:
trawl_database <- odbcConnectAccess2007(here("data", "Interagency Trawl XP.mdb"))

#list tables in trawl database
sqlTables(trawl_database)

#read in required data from trawl - note that if you don't use as_tibble, sqlFetch() returns an odd
#     tbl with sqlFetch(trawl_database, "OH Lengths") as the one obs
oh_lengths_raw <- as_tibble(sqlFetch(trawl_database, "OH LENGTH"))
oh_catch_raw <- as_tibble(sqlFetch(trawl_database, "OH Catch"))
oh_effort_raw <- as_tibble(sqlFetch(trawl_database, "OH Effort"))
on_lengths_raw <- as_tibble(sqlFetch(trawl_database, "ON LENGTH PARSED"))
on_catch_raw <- as_tibble(sqlFetch(trawl_database, "ON CATCH PARSED"))
on_effort_raw <- as_tibble(sqlFetch(trawl_database, "ON EFFORT PARSED"))
spp_codes <- as_tibble(sqlFetch(trawl_database, "1_Interagency Species Codes"))

#close connection to DB
odbcCloseAll()

# Is Ontario data in the database yet?
on_effort_raw %>%
  filter(year == this_year)

### Reformat and join OH and ON Trawl datasets    ##############################

#rename everything to lowercase so col headers match each other
names(oh_lengths_raw) %<>% tolower
names(oh_catch_raw) %<>% tolower
names(oh_effort_raw) %<>% tolower
names(on_lengths_raw) %<>% tolower
names(on_catch_raw) %<>% tolower
names(on_effort_raw) %<>% tolower

#reformat effort data to match
oh_effort_raw %>%
  mutate(crn = as.character(crn),
         agency = "OH",
         projcode = as.character(projcode)) -> oh_effort_fixed

on_effort_raw %>%
  mutate(crn = as.character(yrsam),
         agency = "ON",
         projcode = as.character(projcode)) -> on_effort_fixed

#reformat catch data to match
oh_catch_raw %>%
  mutate(crn = as.character(crn),
         agency = "OH",
         agecode = age,
         projcode = as.character(projcode)) %>%
  select(-age) -> oh_catch_fixed

on_catch_raw %>%
  mutate(crn = as.character(yrsam),
         projcode = as.character(projcode), 
         agecode = age,
         agency = "ON") %>%
  select(-yrsam, -year, -age) -> on_catch_fixed

#reformat length data to match 
on_lengths_raw %>%
  mutate(crn = as.character(yrsam),
         agency = as.character(agency),
         projcode = as.character(projcode)) %>%
  select(-yrsam, -year) -> on_lengths_fixed

oh_lengths_raw %>%
  mutate(crn = as.character(crn),
         projcode = as.character(projcode),
         agency = "OH") -> oh_lengths_fixed






#full joins across all similar dataframes
all_effort <- full_join(oh_effort_fixed, on_effort_fixed)
all_catch <- full_join(oh_catch_fixed, on_catch_fixed)
all_lengths <- full_join(oh_lengths_fixed, on_lengths_fixed)

#use this to check for data errors
#all_catch$projcode %>% unique %>% sort 



### reformat species codes ###############################################
names(spp_codes) %<>% tolower %<>% make.names
#split "species" into common and latin names
spp_codes %>%
  mutate(common_name = trimws(gsub("\\([^()]*\\)", "", species)),
         latin_name = gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", species, perl=T)) %>%
  select(common_name, latin_name, code = interagency.species.code) %>%
  mutate(latin_name = case_when(common_name == "Sand shiner" ~ "Notropis stramineus", #fix old latin name
                                T ~ latin_name))  -> spp_codes

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
all_effort %>%
  mutate(projcode = case_when(projcode %in% c("LEA-IF00_001","LEA_IF00-001") ~ "LEA_IF00_001",
                              T ~ projcode),#fix mismatch between underscores and dashes
         effstat = case_when(year > 2009 & botoxy < 2 ~ 0,
                              T ~ effstat),#temporary measure to handle low DO 2009-present
         year = case_when(year == 12 ~ 2012,
                          T ~ year),
         longitude = case_when(longitude > 0 ~ longitude * -1,
                               T ~ longitude)) -> all_effort

# fix errors in catch
all_catch %<>%
  mutate(projcode = case_when(projcode %in% c("LEA-IF00_001","LEA_IF00-001") ~ "LEA_IF00_001",
                              T ~ projcode)) %>% #fix mismatch between underscores and dashes
  left_join(all_effort %>% select(projcode, year, agency, crn, depth, effdur, effdist, effstat) %>% unique) %>% #THIS MUST BE UNIQUE VALUES
  filter(year != 1987|effstat == 1|!is.na(effdist))


#fix bad projcodes and remove bad trawls (1987, effstat/effdist)
all_lengths %<>% mutate(projcode = case_when(projcode %in% c("LEA-IF00_001","LEA_IF00-001") ~ "LEA_IF00_001",
                                             T ~ projcode)) %>%
  left_join(all_effort %>% select(projcode, crn, year, effdist, effstat) %>% unique) %>% #THIS MUST BE UNIQUE VALUES
  filter(year != 1987, effstat == 1, !is.na(effdist)) %>%
  select(-year, -effdist, -effstat) %>%
  arrange(projcode, crn, species, agecode)

