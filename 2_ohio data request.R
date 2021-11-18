source("1_read ohio data.R")


### Rarest species    ####################################
# show the most rare species from last three years; you can tinker with the last filter
#     line to show more/fewer years and rarity

ohio_only_catch_raw %>%
  left_join(ohio_only_effort_raw) %>%
  left_join(spp_codes) %>%
  arrange(year, month) %>%
  select(crn, year, month, common_name, catch) %>%
  group_by(year, common_name) %>%
  summarize(total_catch = sum(catch)) %>%
  ungroup() %>%
  mutate(rank = rank(total_catch, na.last = NA),
         percent_rank = round(rank(total_catch, na.last = NA)/length(total_catch) * 100,1)) %>%
  filter(year %in% (this_year-2):this_year, percent_rank < 10)



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
  #filter(year > 1999) %>%
  left_join(spp_codes, by = c(species = "code")) %>%
  group_by(species, common_name, latin_name) %>%
  summarize(catch = sum(catch)) %>%
  ungroup %>%
  mutate(percent_catch = round((catch/total_catch$total) * 100,3)) %>%
  arrange(latin_name) %>%
  write_csv(here("data", "WB trawl spp list.csv"))

# use this to look at suspicious catches, e.g. Finescale Dace (species code == 183)
all_catch %>%
  #filter(year > 1999) %>%
  left_join(spp_codes, by = c(species = "code")) %>%
  filter(species == 183)


### Raw trawl data to Excel   ############################################

# filter catch first, have to join effstat != 1 from effort to filter catches
catch_data_request <- ohio_only_catch_raw %>%
  left_join(ohio_only_effort_raw %>% select(projcode, crn, effstat, year) %>% unique) %>%
  filter(effstat == 1, year == 2020) %>%
  rename("agecode" = age) %>%
  left_join(spp_codes) %>%
  arrange(crn, species, agecode)

effort_data_request <- ohio_only_effort_raw %>%
  filter(effstat == 1, year == 2020) %>%
  arrange(crn)


write_csv(effort_data_request, "data derived/data requests/2020 Ohio WB trawl effort.csv")
write_csv(catch_data_request, "data derived/data requests/2020 Ohio WB trawl catch.csv")

### OSU Museum data    #############################################
ohio_only_catch_raw %>%
  left_join(spp_codes, by = c("species" = "code")) %>%
  left_join(ohio_only_effort_raw %>% select(projcode, crn, effstat, depth, year, stemp, btemp, depth, latitude, longitude, comments) %>% unique) %>%
  arrange(crn) %>%
  filter(effstat == 1, year %in% 2000:2019) %>%
  filter(crn %in% c(2198026, 2198036))
#write_csv(path = here("data", "data requests", "OSU Museum Collection Data.csv"))

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