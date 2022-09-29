
source("2_abundance summary.R")
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
  filter(common_name == "Tubenose goby") %>% view

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
