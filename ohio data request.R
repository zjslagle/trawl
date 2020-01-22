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
  group_by(species, common_name, latin_name) %>%
  summarize(catch = sum(catch)) %>%
  ungroup %>%
  mutate(percent_catch = round((catch/total_catch$total) * 100,3)) %>%
  arrange(latin_name) %>%
  write_csv(here("data", "WB trawl spp list.csv"))


### All Ohio trawl data to Excel   ############################################

# filter catch first, have to join effstat != 1 from effort to filter catches
ohio_only_catch_raw %<>%
  left_join(ohio_only_effort_raw %>% select(projcode, crn, effstat, year) %>% unique) %>%
  filter(effstat == 1, year != 1987)

ohio_only_effort_raw %<>%
  filter(effstat == 1, year != 1987)
  
  
write_csv(ohio_only_effort_raw, path = here("data", "data requests", "Ohio WB trawl effort data request.csv"))
write_csv(ohio_only_catch_raw, path = here("data", "data requests", "Ohio WB trawl catch data request.csv"))

### OSU Museum data    #############################################
ohio_only_catch_raw %>%
  left_join(spp_codes, by = c("species" = "code")) %>%
  left_join(ohio_only_effort_raw %>% select(projcode, crn, effstat, depth, year, stemp, btemp, depth, latitude, longitude, comments) %>% unique) %>%
  arrange(crn) %>%
  filter(effstat == 1, year %in% 2000:2019) %>%
  filter(crn %in% c(2198026, 2198036))
  #write_csv(path = here("data", "data requests", "OSU Museum Collection Data.csv"))


### Absolute abundance for Ohio data #################################################
ohio_only_effort_raw %>%
  mutate(projcode = as.character(projcode),
         botoxy = as.double(botoxy),
         year = as.double(year),
         effstat = as.double(effstat)) %>%
  mutate(projcode = case_when(projcode %in% c("LEA-IF00_001","LEA_IF00-001") ~ "LEA_IF00_001",
                              T ~ projcode),#fix mismatch between underscores and dashes
         effstat = case_when(is.na(effstat) ~  NA_real_,
                             is.na(botoxy) ~  NA_real_,
                             year > 2009 & botoxy < 2 ~ 0,
                             T ~ effstat),#temporary measure to handle low DO 2009-present
         year = case_when(year == 12 ~ 2012,
                          T ~ year),
         longitude = case_when(longitude > 0 ~ longitude * -1,
                               T ~ longitude)) -> ohio_only_effort

ohio_only_catch_raw %>%
  rename(agecode = "age") %>%
  mutate(projcode = as.character(projcode)) %>%
  mutate(projcode = case_when(projcode %in% c("LEA-IF00_001","LEA_IF00-001") ~ "LEA_IF00_001",
                              T ~ projcode)) %>% #fix mismatch between underscores and dashes
  group_by(projcode, crn, species, agecode) %>% 
  summarize(sum_catch = sum(catch)) %>% ungroup() %>%
  left_join(ohio_only_effort %>% select(projcode, year, agency, crn, depth, effdur, effdist, effstat) %>% unique) %>% #THIS MUST BE UNIQUE VALUES
  filter(year != 1987|effstat == 1|!is.na(effdist)) -> ohio_only_catch


ohio_only_catch %>%
  complete(nesting(projcode, year, agency, crn), nesting(species, agecode), fill = list(sum_catch = 0)) %>% # explicit zeros
  left_join(ohio_only_effort %>% select(projcode, crn, effdur, effdist, effstat, depth, totdist, longitude, latitude)) %>% #join effort data
  filter(effstat == 1, year != 1987, !is.na(effdist)) -> ohio_catch_summary


# now, calc catch per m^2 - replicates code in "abundance summary.R"
ohio_catch_summary %>%
  mutate(depth_strata = case_when(depth > 6 ~ "deep", #called DEPT = 1 in old code
                                  T ~ "shallow"),
         effdist = effdist * 1852,
         totdist = totdist * 1852,
         deploy_dist = totdist - effdist,
         hypot = (depth^2) + (deploy_dist^2),
         dep_dist = hypot^(1/2),
         total_dist = dep_dist + effdist,
         #next section applies correction for OH trawl before 2002:
         sum_catch = case_when((agency == "OH" & year < 2002) ~ case_when(species == 63 & agecode == 1   ~ sum_catch*1.216,
                                                                          species == 196 & agecode == 1  ~ sum_catch*2.170,
                                                                          species == 196 & agecode == 3  ~ sum_catch*2.170,
                                                                          species == 302 & agecode == 1  ~ sum_catch*1.644,
                                                                          species == 331 & agecode == 1  ~ sum_catch*1.321,
                                                                          species == 331                 ~ sum_catch*1.185,
                                                                          species == 334 & agecode == 1  ~ sum_catch*1.520,
                                                                          species == 371 & agecode == 3  ~ sum_catch*1.505,
                                                                          T                              ~ sum_catch), #now OH 2002 - present:
                               (agency == "OH" & year > 2001) ~ case_when(species == 63 & agecode == 1   ~ sum_catch*2.362,
                                                                          species == 196 & agecode == 1  ~ sum_catch*1.494,
                                                                          species == 196 & agecode == 3  ~ sum_catch*1.494,
                                                                          species == 291 & agecode == 1  ~ sum_catch*0.704,
                                                                          species == 291 & agecode == 3  ~ sum_catch*0.704,
                                                                          species == 301 & agecode == 1  ~ sum_catch*1.121,
                                                                          species == 302 & agecode == 1  ~ sum_catch*3.203,
                                                                          species == 334 & agecode == 1  ~ sum_catch*1.561,
                                                                          species == 366 & agecode == 1  ~ sum_catch*0.423,
                                                                          species == 366                 ~ sum_catch*0.423,
                                                                          species == 371 & agecode == 3  ~ sum_catch*0.598,
                                                                          T                              ~ sum_catch),
                               T ~ sum_catch),
         #COMMENT FROM SAS CODE: *OLD BOAT TRAWL OPENING (<2002) - SEE FTG REPORT FOR METHODS AND ANALYSIS
         #            NEW BOAT TRAWL OPENING (PROBABLY NEED A MORE RIGOROUS SCANMAR EST.)
         gape = case_when(agency == "OH" ~ case_when(year < 2002 ~ 1.5,
                                                     year > 2001 ~ 2.4,
                                                     T ~ NA_real_), #must used NA_real_, else we get a matching error
                          agency == "ON" ~ 1.16),
         wings = case_when(agency == "OH" ~ case_when(year < 2002 ~ 4.83,
                                                      year > 2001 ~ 5.0,
                                                      T ~ NA_real_),
                           agency == "ON" ~ 5.02),
         state = case_when((agency == "ON" & longitude > -83.2900) ~ "ON", # not sure why, but we're setting far west sites to state = OH
                           agency == "ON" ~ "OH",
                           T ~ "OH"),
         area_swept = effdist * wings,
         vol_swept = area_swept * gape,
         fish_per_area = sum_catch/area_swept) %>%
  group_by(projcode, year, crn, species, agecode, agency, state, area_swept, depth_strata) %>%
  summarize(fish_per_area = sum(fish_per_area)) %>%
  ungroup-> ohio_catch_summary_by_area

ohio_catch_summary_by_area %<>%
  select(projcode, year, agency, state, crn, species, agecode, "fish_per_m2" = fish_per_area) %>%
  arrange(year, agency, crn)

# join effort data back so we can get locations and filter out just fall trawl
ohio_catch_summary_by_area %>%
  left_join(spp_codes, by = c(species = "code")) %>%
  left_join(ohio_only_effort %>% select(projcode, crn, month, depth, longitude, latitude)) %>% #join effort data
  filter(month %in% c(9,10)) -> ohio_only_abs_abundance

write_csv(ohio_only_abs_abundance,  
          here("data", "data requests", "8_13_19 absolute abundance by crn_spp_agecode ohio_fall_trawl.csv"))



### Ohio darter catches ########################

ohio_only_catch_raw %>%
  left_join(spp_codes, by = c("species" = "code")) %>%
  left_join(ohio_only_effort_raw) %>%
  filter(str_detect(latin_name, "Etheostoma|Percina")|str_detect(common_name, "Etheostoma|Percina"), 
         species != 342) %>% # exclude Logperch
  group_by(year, month, common_name) %>%
  summarize(total_catch = sum(catch)) %>% view

ohio_only_effort_raw %>%
  filter(year == 2018, siteno %in% c(29,33)) %>%
  select(siteno, latitude, longitude) %>% view
