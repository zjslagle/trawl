source("Read Trawl Data.R")
source("abundance summary.R")


# write all_tibbles to excel workbook

write.xlsx(as.data.frame(all_effort), file = here("data", "FTG trawl data.xlsx"), 
           sheetName = "effort", append = FALSE, row.names = FALSE)
write.xlsx(as.data.frame(all_catch), file = here("data", "FTG trawl data.xlsx"), 
           sheetName = "catch", append = TRUE, row.names = FALSE)

# lengths too long for xls
write_csv(all_lengths, path = here("data", "data requests", "Interagency WB trawl lengths data request.csv"))
write_csv(spp_codes, path = here("data", "spp_codes.csv"))

write_csv(all_effort %>% arrange(year, crn), path = here("data", "data requests", "Interagency WB trawl effort.csv"))



### FTG data - fish per m2 by site (with common names) AND site data
catch_summary_all_spp_by_area %>% 
  select(-depth_strata, -area_swept, -state, -projcode) %>%
  filter(fish_per_m2 > 0) %>%
  left_join(spp_codes, by = c(species = "code")) %>%
  select(crn, year, month, day, agency, species, common_name, latin_name, agecode, fish_per_m2) %>%
  arrange(crn) -> ftg_request_data



write.xlsx(as.data.frame(all_effort %>% select(-projcode, -gear, -effdur, -effdist, -statime, -endtime, -totdist, -speed, -yrsam, -trldate, -botoxy2)), 
           file = here("data", "data requests", "3_3_20 FTG trawl data request.xlsx"), 
           sheetName = "effort", append = FALSE, row.names = FALSE)
write.xlsx(as.data.frame(ftg_request_data), file = here("data", "data requests", "3_3_20 FTG trawl data request.xlsx"), 
           sheetName = "catch", append = TRUE, row.names = FALSE)



