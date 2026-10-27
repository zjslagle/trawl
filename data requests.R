
source("3_biomass summary.R")
require(xlsx)

### write all_tibbles to excel workbook  ########################################################

write_excel_csv(all_effort %>%
                  arrange(year, month, day) %>%
                  select(projcode, yrsam, agency, year, trldate, crn, effdur, effdist, effstat, statime, endtime, btemp, stemp, secchi, botoxy, botoxy2, totdist, speed, depth, latitude, longitude, grid, comments), 
                file = "data derived/West Basin interagency trawl effort data.csv")
write_excel_csv(all_catch %>%
                  arrange(year, agency, crn), 
                file = "data derived/West Basin interagency trawl catch data.csv")

# lengths too long for xls
write_csv(all_lengths %>% 
            left_join(all_effort %>% select(crn, yrsam, year)) %>%
            left_join(spp_codes) %>%
            select(year, common_name, species, agecode, length) %>%
            arrange(year, species, agecode), 
          file = "data derived/data requests/Interagency WB trawl lengths data request.csv")
write_csv(spp_codes, file = "data derived/spp_codes.csv")

### White Bass catch by site - Jackson work   #########################
all_catch %>%
  select(yrsam, year, crn, species, agecode, catch) %>%
  complete(nesting(yrsam, year, crn), 
           nesting(species, agecode), fill = list(catch = 0)) %>%
  left_join(all_effort %>% select(yrsam, year, effstat, crn, agency)) %>%
  filter(species == 302, agecode == 1, effstat == 1, year > 1987) %>% 
  write_csv("data derived/interagency 302_1 catches.csv")

### FTG - species-specific catches   #########################
all_catch %>%
  left_join(spp_codes) %>%
  arrange(year, agency) %>%
  filter(common_name == "Tubenose goby") 

### Hotspot analysis paper - Oct 2021    ########################
catch_per_ha %>%
  left_join(all_effort %>% select(year, yrsam, crn, agency, month, day, latitude, longitude)) %>%
  mutate(date = ymd(paste(year, month, day))) %>%
  filter(spp_agegroup %in% c("63_1", "196_1", "331_1", "334_1"),
         year > 1991) %>%
  pivot_wider(names_from = spp_agegroup,
              values_from = cpha) %>%
  select(crn, year, date, latitude, longitude, 
         "SHAD_per_ha" = `63_1`, 
         "ESHIN_per_ha" = `196_1`, 
         "YP_per_ha" = `331_1`, 
         "WAE_per_ha" = `334_1`) %>%
  write_csv("data derived/data requests/Interagency CPHA by site - Hotspot analysis 10-13-21.csv")

### Nov 2021 Biomass by year and spp/ageclass - Hongyan Zhang      ###############################

biomass_summary_all_spp %>%
  group_by(year, common_name, species, agecode) %>%
  summarize(total_biomass_mt = sum(total_biomass_mt)) %>%
  filter(total_biomass_mt > 0) %>%
  mutate(total_biomass_kg_per_ha = total_biomass_mt*1000/375320) %>% #convert to kg per hectare
  select(-total_biomass_mt) %>%
  write_csv("data derived/data requests/1_3_22 West Basin biomass by year.csv")

### Forage length data  ###########################
all_lengths %>%
  write_csv("data derived/data requests/1_11_22 West Basin forage fish lengths.csv")

### 2022 Touhue & DuFour forage data    #########################################
# forage available to WAE
yearly_abundance_tall %>%
  filter(species %in% c(61, 196, 121, 201, 291, 366)|
           species %in% c(63, 301, 302, 331, 371) & agecode == 1) %>%
  left_join(agecodes) %>%
  select(-agecode) %>%
  write_csv("data derived/data requests/22_09_29 Lake Erie Western Basin Interagency Forage abundances.csv")





### Cornell/Taylor Brown - drivers of Great Lakes coregonine recruitment  #####################
taylor_data <- cpha_by_site %>%
  mutate(vessel = case_when(state == "CA" ~ "Keenosay",
                            state == "US" & year < 2002 ~ "Old Explorer",
                            T ~ "New Explorer")) %>%
  select(year, yrsam, vessel, species, agecode, cpha) %>%
  filter(species %in% c(121, 61, 301, 366)) %>%
  left_join(biomass_summary_by_site %>% select(yrsam, species, agecode, "biomass_g_per_m2" = biomass_per_area)) %>% 
  mutate(agecode = case_when(agecode == 2 ~ 3,
                             agecode == 4 ~ 3,
                             T ~ agecode)) %>% 
  group_by(year, yrsam, vessel, species, agecode) %>%
  summarize(cpha = sum(cpha),
            biomass_g_per_m2 = sum(biomass_g_per_m2)) %>% ungroup %>%
  left_join(all_effort %>% select(yrsam, month, day, year, latitude, longitude)) %>%
  mutate(date = ymd(paste(year, month, day, sep = "_"))) %>%
  select(-month, -day) %>%
  mutate(biomass_kg_per_ha = (biomass_g_per_m2/1000)/375320) %>%
  left_join(spp_codes) %>% left_join(agecodes) %>% 
  select(year, yrsam, vessel, date, latitude, longitude, species, common_name, agecode, ages, "catch_per_hectare" = cpha,
         biomass_g_per_m2, biomass_kg_per_ha) 

taylor_data %>%
  write_csv("data derived/data requests/25-03-25 West Basin Lake Erie Interagency Trawl Data Request.csv")

### weird species  ############
weirdos <- all_catch %>%
  left_join(spp_codes) %>%
  group_by(common_name) %>%
  tally %>% filter(n < 10) %>%
  select(common_name) %>% as_vector

all_lengths %>%
  left_join(spp_codes) %>%
  filter(common_name %in% weirdos) %>% view

### 2025 - Stu Ludsin basinwide biomass   #############################

### 2025 - P2F Project (for Willie)     ######################################################
biomass_summary_by_site %>%
  left_join(all_effort) %>%
  # mutate(date_new = paste(year, month, day, sep = "-")) %>%
  # select(year, month, day, date_new)
  left_join(agecodes) %>%
  left_join(spp_codes) %>%
  left_join(catch_summary_by_site %>% select(yrsam, species, agecode, fish_per_area)) %>%
  filter(biomass_kg_ha > 0) %>%
  mutate(survey = "Erie Western Basin Interagency Trawl",
         agency_full = if_else(agency == "OH", "Ohio DNR", "Ontario MNRF"),
         sampling_id = yrsam,
         port = "",
         gear_type = case_when(agency == "ON" ~ "Bottom trawl - R/V Keenosay",
                               year < 2003 ~ "Bottom trawl - R/V Gibraltar (Old Explorer)",
                               T ~ "Bottom trawl - R/V Explorer"),
         date = paste(year, month, day, sep = "-"),
         date_set = "", lat_end = "", lon_end = "", sam_depth = "", sam_depth2 = "",
         depth_units = "meters",
         biomass_units = "grams",
         total_catch = "", # not providing this, want FPC-applied CPUE
         cpue_units = "catch (n) per hectare",
         bpue_units = "kg per hectare",
         temp_units = "Degrees Celcius",
         sampling_temp = btemp,
         secchi_units = "meters") %>%
  select(survey, agency_full, sampling_id, port, gear_type, date, date_set, statime, endtime,
         latitude, longitude, lat_end, lon_end, depth_units, depth, depth2, sam_depth, sam_depth2, latin_name, ages,
         total_catch, total_weight, biomass_units, fish_per_area, cpue_units, biomass_kg_ha, bpue_units, temp_units, 
         stemp, btemp, sampling_temp, secchi, secchi_units, comments) %>% 
  write_csv("data derived/data requests/25_10_08 Erie Western Basin Trawl Biomass Data - P2F.csv",
            na = "")

### Emerald shiner lengths (for Liz, OSU)    #######################
all_lengths %>%
  left_join(all_effort %>% select(yrsam, "date" = trldate)) %>%
  filter(species == 196, agency == "ON") %>%
  left_join(agecodes) %>%
  mutate(source = "Ontario IA trawl",
         date = as_date(date),
         t_mon = 8) %>%
  select(yrsam, species, agecode, length, year, t_mon, date, ages, source) %>%
  write_csv("data derived/data requests/26_10_05 Emerald Shiner lengths.csv")
