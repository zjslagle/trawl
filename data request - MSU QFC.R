source("2_abundance summary.R")

### Data request for MSU QFC overhaul of YEP models - 2025     #######################
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
         mu = 1, fpc = "",
         t_mon = 8,
         statime = paste0(if_else(statime < 999, c(str_sub(statime, 0,1)), c(str_sub(statime, 0,2))),
                          ":", str_sub(statime, -2)),
         endtime = paste0(if_else(endtime < 999, c(str_sub(endtime, 0,1)), c(str_sub(endtime, 0,2))),
                          ":", str_sub(endtime, -2)),
         eff_unit = "hectares") %>%
  select(yrsam,
         agency, gear, gear_spec, mu, grid, t_mon, year, month, day, statime, endtime, fpc, "effort" = ha_swept,
         eff_unit, latitude, longitude, depth, stemp, btemp, botoxy) %>%
  arrange(yrsam)

yrsams_to_get <- effort_export %>% select(yrsam) %>% as_vector()

### catch data 
catch_export <- catch_summary %>% #already has explicit zeros
  filter(species == 331, yrsam %in% yrsams_to_get) %>%
  select(yrsam, species, agecode, sum_catch) %>%
  arrange(yrsam, agecode)


### length data     #################
qfc_length <- all_lengths %>%
  filter(species == 331, yrsam %in% yrsams_to_get) %>%
  select(yrsam, species, agecode, length) %>%
  mutate(agetype_expanded = 0)

# have to get age-1+ perch from ohio-only database since it's not in the IA db
### write to XLSX   ##############################################
writexl::write_xlsx(x = list(effort_export, catch_export, qfc_length),
                    path = "data derived/data requests/QFC - Interagency trawl data.xlsx")


### export yrsam-crn for joining w/ ohio-only data ######
all_effort %>%
  filter(agency == "OH") %>%
  select(crn, yrsam) %>%
  arrange(crn) %>%
  write_csv("data derived/crn-yrsam pairing.csv")
