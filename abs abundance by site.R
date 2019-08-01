source("Read Trawl Data.R")
source("abundance summary.R")


draft_catch_summary %>%
  group_by(projcode, year, agency, crn, species, agecode) %>%
    summarize(sum_catch = sum(sum_catch)) %>% # sum over crn/spp/age code - may not be necessary
  ungroup() %>%
  complete(nesting(projcode, year, agency, crn), nesting(species, agecode), fill = list(sum_catch = 0)) %>% # explicit zeros
  left_join(all_effort %>% select(projcode, crn, effdur, effdist, effstat, depth, totdist, longitude, latitude)) %>% #join effort data
  filter(effstat == 1, year != 1987, !is.na(effdist)) -> catch_summary_all_spp


# now, calc catch per m^2 - replicates code in "abundance summary.R"
catch_summary_all_spp %>%
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
  ungroup-> catch_summary_all_spp_by_area

catch_summary_all_spp_by_area %<>%
  select(projcode, year, agency, state, crn, species, agecode, "fish_per_m2" = fish_per_area, depth_strata, area_swept) %>%
  arrange(year, agency, crn)

write_csv(catch_summary_all_spp_by_area %>% select(-depth_strata, -area_swept, -state), 
          here("data", "data requests", "7_31_19 absolute abundance by crn_spp_agecode.csv"))
          
### Absolute abundance (per ha) basinwide for ALL species ##################################

# Average catch/m2 over year, agency, depth strata, and spp_agegroup
catch_summary_all_spp_by_area %>%
  group_by(year, state, depth_strata, species, agecode) %>%
  summarize(mean_fish = mean(fish_per_m2),
            sd_fish = sd(fish_per_m2)) %>%
  ungroup-> yearly_abundance_by_agency_all_spp

# Expand catch/m2 to absolute abundance by agency based on estimated area across each depth strata;
#   Estimates of area by depth strata are from Arcview exercise; SAS code claims "Summary attached" but it's not
#     Then, calc fish/ha
yearly_abundance_by_agency_all_spp %>%
  mutate(total_abundance = case_when(state == "ON" ~ case_when(depth_strata == "shallow" ~ mean_fish * 1.681E+8,
                                                               depth_strata == "deep" ~ mean_fish * 1.244E+9),
                                     state == "OH" ~ case_when(depth_strata == "shallow" ~ mean_fish * 9.281E+8,
                                                               depth_strata == "deep" ~ mean_fish * 1.413E+9)),
         abundance_per_ha = total_abundance/375320) -> yearly_abundance_all_spp # number per hectare


# now create West Basin summary, calc abs abundance and fish/ha, then saves totals
yearly_abundance_all_spp %>%
  group_by(year, species, agecode) %>%
  summarize(total_abundance = sum(total_abundance),
            abundance_per_ha = total_abundance/375320) %>%
  ungroup -> yearly_abundance_all_spp

write_csv(yearly_abundance_all_spp, here("data", "data requests", "7_31_19 absolute abundance by year_spp_agecode.csv"))

