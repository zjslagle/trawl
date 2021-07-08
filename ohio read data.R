#     OHIO ONLY
#
#     Summary of West Basin trawl data - Ohio ONLY, years 1987 - present
#       for Forage TG reporting 
#
#     Much of the processing is based on SAS code written by Jeff Tyson (I think)
#     This code written by Zak Slagle, ODNR-SARE, code started May 2020


### Load libraries   ##################################
library(magrittr)
library(forcats)
require(RODBC)
require(lubridate)
require(stringr)
require(here)
require(readxl)
library(tidyverse)


this_year = year(now())-1 #get this year
windowsFonts(Times=windowsFont("Times New Roman"))

### Connect to Trawl MSAccess database, download required data  ############################

#open connection to trawl database
#trawl_database <- odbcConnectAccess2007("//s.dnr.isi.oitfs.ohio.gov/DNRShares/Sandusky Fisheries Research/DuFour/1_Projects/1_Trawling/3_Database/Interagency Trawl XP.mdb")
# work from home:
oh_trawl_database <- odbcConnectAccess2007("data sources/Trawling Database.mdb")
#list tables in trawl database
sqlTables(oh_trawl_database)
#read in required data from trawl - note that if you don't use as_tibble, sqlFetch() returns an odd
#     tbl with sqlFetch(trawl_database, "OH Lengths") as the one obs
oh_only_effort_raw <- as_tibble(sqlFetch(oh_trawl_database, "Trawl Effort"))
oh_only_catch_raw <- as_tibble(sqlFetch(oh_trawl_database, "Trawl Catch"))
#close connection to DB
odbcCloseAll()

# read in trawl lengths from separate database
length_database <-  odbcConnectAccess2007("data sources/Trawl lengths xp")   #specifies the file path

sqlTables(length_database)

LengthsA <- as_tibble(sqlFetch(length_database, "TRAWL LENGTHS 1969-74")) 
LengthsB <- as_tibble(sqlFetch(length_database, "TRAWL LENGTHS 1975-79")) 
LengthsC <- as_tibble(sqlFetch(length_database, "TRAWL LENGTHS 1980-84"))
Lengths1 <- as_tibble(sqlFetch(length_database, "TRAWL LENGTHS 1985-89")) 
Lengths2 <- as_tibble(sqlFetch(length_database, "TRAWL LENGTHS 1990-94")) 
Lengths3 <- as_tibble(sqlFetch(length_database, "TRAWL LENGTHS 1995-99")) 
Lengths4 <- as_tibble(sqlFetch(length_database, "TRAWL LENGTHS 2000-04")) 
Lengths5 <- as_tibble(sqlFetch(length_database, "TRAWL LENGTHS 2005-09")) 
Lengths6 <- as_tibble(sqlFetch(length_database, "TRAWL LENGTHS 2010-14")) 
Lengths7 <- as_tibble(sqlFetch(length_database, "TRAWL LENGTHS 2015-19")) 
odbcCloseAll()

oh_only_length_data_raw <-list(LengthsA, LengthsB, LengthsC, Lengths1, Lengths2, Lengths3, Lengths4, Lengths5, Lengths6, Lengths7) %>% reduce(full_join)

### Species codes and names   ##########################################
trawl_database <- odbcConnectAccess2007("data sources/Interagency Trawl XP.mdb")
spp_codes <- as_tibble(sqlFetch(trawl_database, "1_Interagency Species Codes"))
odbcCloseAll()

names(spp_codes) %<>% tolower %<>% make.names
#split "species" into common and latin names
spp_codes %>%
  mutate(common_name = trimws(gsub("\\([^()]*\\)", "", species)),
         latin_name = gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", species, perl=T)) %>%
  select(common_name, latin_name, code = interagency.species.code) %>%
  rename("species" = code) %>%
  mutate(latin_name = case_when(common_name == "Sand shiner" ~ "Notropis stramineus", #fix old latin name
                                T ~ latin_name))  -> spp_codes

### Reformat OH Trawl dataset    ##############################

#rename everything to lowercase so col headers match each other
names(oh_only_effort_raw) %<>% tolower
names(oh_only_catch_raw) %<>% tolower
names(oh_only_length_data_raw) %<>% tolower

#reformat effort data to match
oh_only_effort_raw %>%
  mutate(crn = as.character(crn),
         projcode = as.character(projcode)) %>%
  arrange(crn) -> oh_all_effort

#reformat catch data to match
oh_only_catch_raw %>% 
  mutate(crn = as.character(crn),
         projcode = as.character(projcode),
         agecode = age) %>% #ifelse(age == 5, 4, age)) %>%
  select(-age) %>%
  arrange(crn, species) -> oh_all_catch

  #which species have agecode == 5? (5 = all ages, no longer used except for goby) - DuFour code treats age 5 -> age 4
  oh_only_length_data_raw %>% filter(agecode == 5) %>% select(species) %>% unique() %>% left_join(spp_codes)


# reformat length data
oh_only_length_data_raw %>%
  #filter(year > 1991) %>%
  mutate(crn = as.character(crn)) %>%
  select(-year, -month, -day, -grid) %>%
  arrange(crn) -> oh_all_lengths


### Trim trawl effort    ###############
oh_all_effort %>%
  mutate(projcode = case_when(projcode %in% c("LEA-IF00_001","LEA_IF00-001") ~ "LEA_IF00_001",
                              T ~ projcode),#fix mismatch between underscores and dashes
         effstat = case_when(year > 2009 & botoxy < 2 ~ 0,
                             T ~ as.double(effstat)), #temporary measure to handle low DO 2009-present
         year = case_when(year == 12 ~ 2012,
                          T ~ as.double(year)),
         longitude = case_when(longitude > 0.0 ~ longitude * -1,
                               T ~ as.double(longitude))) -> oh_all_effort



#### Do catches in catch data match lengths in length data? hint - NO   #################

oh_all_lengths %>%
  group_by(crn, species, agecode) %>%
  summarize(catch_from_length_data = sum(!is.na(length))) %>% 
  ungroup() %>%
  left_join(oh_all_catch %>% select(crn, species, agecode, "catch_from_catch_data" = catch)) %>%
  mutate(diff = catch_from_catch_data - catch_from_length_data) %>%
  filter(diff < 0) %>%
  left_join(oh_all_effort %>% select(crn, year)) %>%
  group_by(year) %>%
  summarize(n_neg_numbers = n()) %>% view
