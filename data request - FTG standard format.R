source("2_abundance summary.R")

### New "FTG Standard" Data request (built off of MSU QFC overhaul of YEP models request) #######################
#get ha swept from catch_per_area
area_join <- catch_per_area %>%
  select(yrsam, area_swept) %>%
  mutate(ha_swept = area_swept/10000) %>%
  unique

# effort data
effort_export <- all_effort %>%
  left_join(area_join) %>%
  filter(year > 1987, effstat == 1) %>%
  mutate(gear = "Trawl",
         gear_spec = "Bottom",
         mu = 1,
         t_mon = 8,
         statime = paste0(if_else(statime < 999, c(str_sub(statime, 0,1)), c(str_sub(statime, 0,2))),
                          ":", str_sub(statime, -2)),
         endtime = paste0(if_else(endtime < 999, c(str_sub(endtime, 0,1)), c(str_sub(endtime, 0,2))),
                          ":", str_sub(endtime, -2)),
         eff_unit = "hectares",
         lat_end = "", lon_end = "") %>%
  select(yrsam,
         agency, gear, gear_spec, mu, grid, t_mon, year, month, day, statime, endtime, "effort" = ha_swept,
         eff_unit, latitude, longitude, lat_end, lon_end, depth, stemp, btemp, botoxy) %>%
  arrange(yrsam)

yrsams_to_get <- effort_export %>% select(yrsam) %>% as_vector()

### catch data 
#create FPC categories
older_fpc_cats = c("63_1",	"196_1",	"196_3",	"302_1", "331_1", "331_2", "331_3", "331_4", "334_1", "371_3")
newer_fpc_cats = c("63_1",	"196_1",	"196_3",	"291_1",	"291_3",	"301_1",	"302_1",	"334_1",	"366_1",	"366_5")

catch_export <- catch_summary %>% #already has explicit zeros
  filter(yrsam %in% yrsams_to_get, sum_catch > 0) %>%
  mutate(fpc_applied = case_when(agency == "OH" & year %in% 1988:2001 & # add where FPCs have been applied
                                   spp_agegroup %in% older_fpc_cats ~ TRUE,
                                 agency == "OH" & year %in% 2002:this_year & 
                                   spp_agegroup %in% newer_fpc_cats ~ TRUE,
                                 T ~ FALSE),
         weight = "") %>%
  select(yrsam, species, agecode, sum_catch, weight, fpc_applied) %>%
  arrange(yrsam, agecode) 


### length data     #################
# qfc_length <- all_lengths %>%
#   filter(species == 331, yrsam %in% yrsams_to_get) %>%
#   select(yrsam, species, agecode, length) %>%
#   mutate(agetype_expanded = 0)

# have to get age-1+ perch from ohio-only database since it's not in the IA db
### write to XLSX   ##############################################
writexl::write_xlsx(x = list(effort_export, catch_export),
                    path = "data derived/data requests/FTG Standard Export - Interagency trawl data.xlsx")

