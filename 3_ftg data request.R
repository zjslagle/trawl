
source("2_cpha and abundance summary.R")
require(xlsx)

# write all_tibbles to excel workbook

write_excel_csv(all_effort %>%
                  arrange(year, month, day) %>%
                  select(projcode, yrsam, agency, year, trldate, crn, effdur, effdist, effstat, statime, endtime, btemp, stemp, secchi, botoxy, botoxy2, totdist, speed, depth, latitude, longitude, grid, comments), 
                file = "data derived/West Basin interagency trawl effort data.csv")
write_excel_csv(all_catch %>%
                  arrange(year, agency, crn), 
                file = "data derived/West Basin interagency trawl catch data.csv")

# lengths too long for xls
write_csv(all_lengths, path = "data derived/data requests/Interagency WB trawl lengths data request.csv")
write_csv(spp_codes, path = "data derived/spp_codes.csv")

write_excel_csv()


### FTG - species-specific catches   #########################
all_catch %>%
  left_join(spp_codes, by = c("species" = "code")) %>%
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
