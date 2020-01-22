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
