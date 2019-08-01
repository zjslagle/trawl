source("Read Trawl Data.R")


# NOT THE SAME AS THE INTERAGENCY DB - Ohio has all months, vs Aug-only in Interagency
ohio_trawl_database <- odbcConnectAccess2007("//s.dnr.isi.oitfs.ohio.gov/DNRShares/Sandusky Fisheries Research/DuFour/1_Projects/1_Trawling/3_Database/Trawling Database.mdb")

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


####  PK silver chub data request - 3/6/19   ############################
ohio_only_catch_raw %>%
  left_join(ohio_only_effort_raw) %>%
  filter(year %in% 2017:2018, species == 191, age == 5) #error checking - old data has ACs = 2,4,5 (should be just 1 or 3)

#make zero's, join effort data, filter out bad trawls
ohio_only_catch_raw %>%
  complete(nesting(projcode, crn), species, age, fill = list(catch = 0)) %>%
  left_join(ohio_only_effort_raw) %>%
  filter(species == 191, year %in% 2017:2018, age %in% c(1,3), effstat == 1) %>% #complete() adds in age classes, we only need AC = 1|3
  select(year, month, day, crn, "age_code" = age, catch, stemp, btemp, latitude, longitude, "eff_mins" = effdur) %>%
  mutate(longitude = case_when(longitude < 0 ~ longitude,
                               T ~ longitude *-1)) %>%
  write_csv("odw silver chub trawl data 2017-18.csv")


ohio_only_catch_raw %>%
  complete(nesting(projcode, crn), species, age, fill = list(catch = 0)) %>%
  left_join(ohio_only_effort_raw) %>%
  #filter(species == 191, year %in% 2017:2018, age %in% c(1,3), effstat == 1) %>%
  filter(crn %in% c(217604, 2176026)) %>% View

ohio_only_catch_raw %>%
  complete(nesting(projcode, crn), species, age, fill = list(catch = 0)) %>%
  left_join(ohio_only_effort_raw) %>%
  filter(species == 191, year %in% 2017:2018, age %in% c(1,3), effstat == 1) %>%
  filter(grid == 931) %>%
  mutate(longitude = case_when(longitude < 0 ~ longitude,
                               T ~ longitude *-1)) %>%
  summarize(min_lon = min(longitude),
            max_lon = max(longitude),
            min_lat = min(latitude),
            max_lat = max(latitude)) %>% View




###   FTG Yearly Asian Carp Effort summary   ###########################
oh_effort_raw %>%
  filter(year == this_year) %>%
  mutate(survey = "Trawl",
         office = "Sandusky") %>%
select(year, agency, latitude, longitude, survey, office) %>% View

### Species list for West Basin #################################
all_catch %>%
  left_join(spp_codes, by = c(species = "code")) %>%
  summarize(total = sum(catch)) -> total_catch


all_catch %>%
  left_join(spp_codes, by = c(species = "code")) %>%
  group_by(common_name, latin_name) %>%
  summarize(catch = sum(catch)) %>%
  ungroup %>%
  mutate(percent_catch = round((catch/total_catch$total) * 100,3)) %>%
  arrange(latin_name) %>%
  write_csv(here("data", "WB trawl spp list.csv"))


### All Ohio trawl data to Excel   ############################################

# filter catch first, have to join effstat != 1 from effort to filter catches
ohio_only_catch_raw %<>%
  left_join(ohio_only_effort_raw %>% select(projcode, crn, effstat) %>% unique)
  filter(effstat == 1, year != 1987)

ohio_only_effort_raw %<>%
  filter(effstat == 1, year != 1987)
  
  
write_csv(ohio_only_effort_raw, path = here("data", "data requests", "Ohio WB trawl effort data request.csv"))
write_csv(ohio_only_catch_raw, path = here("data", "data requests", "Ohio WB trawl catch data request.csv"))
