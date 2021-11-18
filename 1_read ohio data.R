### Read Ohio Data ######################################
library(magrittr)
library(forcats)
require(RODBC)
require(lubridate)
require(stringr)
require(readxl)
library(tidyverse)

this_year = year(now())-1 #get this year
spp_codes <- read_csv("data sources/spp_codes.csv")

# NOT THE SAME AS THE INTERAGENCY DB - Ohio has all months, vs Aug-only in Interagency
#ohio_trawl_database <- odbcConnectAccess2007("//s.dnr.isi.oitfs.ohio.gov/DNRShares/Sandusky Fisheries Research/DuFour/1_Projects/1_Trawling/3_Database/Trawling Database.mdb")
ohio_trawl_database <- odbcConnectAccess2007("S:/DuFour/1_Projects/1_Trawl/data/Trawl_Database.mdb")

#list tables in trawl database
#sqlTables(ohio_trawl_database)

#read in required data from trawl - note that if you don't use as_tibble, sqlFetch() returns an odd
#     tbl with sqlFetch(trawl_database, "OH Lengths") as the one obs
ohio_only_catch_raw <- as_tibble(sqlFetch(ohio_trawl_database, "Trawl Catch"))
ohio_only_effort_raw <- as_tibble(sqlFetch(ohio_trawl_database, "Trawl Effort"))

#close connection to DB
odbcCloseAll()

names(ohio_only_catch_raw) %<>% tolower
names(ohio_only_effort_raw) %<>% tolower


### Simple Ohio species lookup    ######
ohio_only_catch_raw %>%
  left_join(ohio_only_effort_raw) %>%
  left_join(spp_codes) %>%
  filter(common_name == "Tubenose goby") %>%
  arrange(year, month, crn) %>%
  select(crn, year, month, common_name, catch)

