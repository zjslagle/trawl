source("1_read data - 2020 format.R")


 ### Abundance Summary    ##################################
# this R code is an attempt to replicate existing SAS code (written by Jeff Tyson) 
# that takes OH and ON trawl data, calc's fish numbers, and extrapolates up to basinwide
# estimates.
 
# all_effort %<>% filter(!(year == 2021 & crn == 253))  #outlier site from 2021

### Create Catch Summary   #############################################################
# sum over CRN (only summarizes ~10 rows), join effort, filter out bad trawls
draft_catch_summary <- all_catch %>%
  group_by(projcode, year, agency, yrsam, crn, species, agecode) %>% 
  summarize(sum_catch = sum(catch)) %>% 
  ungroup() %>% 
  left_join(all_effort %>% 
              select(projcode, year, agency, yrsam, crn, depth, effdur, effdist, effstat, totdist, longitude, latitude)) %>%
  arrange(year, crn, projcode) %>% 
  filter(effstat == 1, year != 1987, !is.na(effdist)) #last part filters out one site in '93

#filter only spp-age groups that we care about, sum, then add explicit zero catches
catch_summary <- draft_catch_summary %>%
  mutate(spp_agegroup = paste0(species, "_", agecode)) %>%
  group_by(projcode, year, agency, yrsam, species, agecode, spp_agegroup) %>%
    summarize(sum_catch = sum(sum_catch)) %>%
  ungroup() %>%
  complete(nesting(projcode, year, agency, yrsam), 
           nesting(species, agecode, spp_agegroup), 
           fill = list(sum_catch = 0))

#join in effort stats
catch_summary <- catch_summary %>%
  left_join(all_effort %>% 
              select(projcode, year, agency, yrsam, crn, depth, effdist, longitude)) %>%
  arrange(year, crn, projcode)

# OLD SAS NOTE - Calc Vol sampled and catch/meter^2 for each spp-age with Fishing Power Correction (FPC) applied   ##################################

### Apply Fishing Power Corrections and calculate abundance per area   ###################################
#Use  nested case_when calls to find cases of Ohio trawl prior to 2002 and applying one
# fishing correction to them, then finding Ohio trawls 2002-present and applying another. We are
# adjusting 2 diff OH vessels (RVs Explorer/Gibraltar and Explorer II) to the fishing power of Ontario's vessel (R/V Keenosay)
 
catch_per_area <- catch_summary %>%
  mutate(depth_strata = case_when(depth > 6 ~ "deep", #called DEPT = 2 in old code
                                  T ~ "shallow"),
         effdist = effdist * 1852, #convert nautical miles to meters
         wings = case_when(agency == "OH" ~ case_when(year < 2002 ~ 4.83,
                                                      year > 2001 ~ 5.0,
                                                      T ~ NA_real_),#must use NA_real_, else we get a matching error
                           agency == "ON" ~ 5.02),
         state = case_when((agency == "ON" & longitude < -83.2900) ~ "US", # ON-fished sites in MI waters 1988-95; these get summarized in US waters
                           agency == "ON" ~ "CA",
                           T ~ "US"),
         area_swept = effdist * wings,
         fish_per_area = sum_catch/area_swept) %>% #catch per m^2
  #apply Fishing Power Corrections (see Tyson et al. 2006)
  mutate(fish_per_area = case_when((agency == "OH" & year < 2002) ~ case_when(spp_agegroup == "63_1" ~ fish_per_area*1.216,
                                                              spp_agegroup %in% c("196_1", "196_3") ~ fish_per_area*2.170,
                                                              spp_agegroup == "302_1"  ~ fish_per_area*1.644,
                                                              spp_agegroup == "331_1"  ~ fish_per_area*1.321,
                                                              spp_agegroup == "331_3"  ~ fish_per_area*1.185,
                                                              spp_agegroup == "334_1"  ~ fish_per_area*1.520,
                                                              spp_agegroup == "371_3"  ~ fish_per_area*1.505,
                                                             T ~ fish_per_area), 
                                   #now OH 2002 - present:
                        (agency == "OH" & year > 2001) ~ case_when(spp_agegroup == "63_1"   ~ fish_per_area*2.362,
                                                             spp_agegroup == "196_1"  ~ fish_per_area*1.494,
                                                             spp_agegroup == "196_3"  ~ fish_per_area*1.494,
                                                             spp_agegroup == "291_1"  ~ fish_per_area*0.704,
                                                             spp_agegroup == "291_3"  ~ fish_per_area*0.704,
                                                             spp_agegroup == "301_1"  ~ fish_per_area*1.121,
                                                             spp_agegroup == "302_1"  ~ fish_per_area*3.203,
                                                             spp_agegroup == "334_1"  ~ fish_per_area*1.561,
                                                             spp_agegroup == "366_1"  ~ fish_per_area*0.423,
                                                             spp_agegroup == "366_5"~ fish_per_area*0.423,
                                                             spp_agegroup == "371_3"  ~ fish_per_area*0.598,
                                                             T ~ fish_per_area),
                        T ~ fish_per_area))

# Tyson, J.T., Johnson, T.B., Knight, C.T., Bur, M.T., 2006. 
#       Intercalibration of Research Survey Vessels on Lake Erie. 
#       North Am. J. Fish. Manag. 26, 559–570. https://doi.org/10.1577/m05-027.1

cpha_by_site <- catch_per_area %>%
  group_by(year, yrsam, crn, state, depth_strata, spp_agegroup, species, agecode) %>%
  summarize(cpha = fish_per_area*10000) %>% #convert meters^2 to hectares
  ungroup

#doublecheck that we didn't lose a bunch of sites somehow!
cpha_by_site %>% 
  select(year, state, yrsam) %>% unique %>% 
  xtabs(~year+state, data = .)

#write to CSV
#cpha_by_site %>% write_csv("data derived/Interagency CPHA by site.csv")

# quick Age-0 walleye (or whatever) graph
# cpha_by_site %>%
#   group_by(year, spp_agegroup) %>%
#   summarize(mean_cpha = mean(cpha)) %>%
#   filter(spp_agegroup == "334_1") %>%
#   ggplot(aes(x = year, y = mean_cpha))+
#     geom_col()

### Create functional groupings (spiny-rayed, clupeid, soft-rayed, and total) in new tibble   ################
fish_group_summary <- catch_per_area %>%
  group_by(year, crn, yrsam, state, depth_strata) %>%
  summarize(clupeid = sum(fish_per_area[spp_agegroup %in% c("61_1", "61_3", "63_1")]),
            shiner = sum(fish_per_area[spp_agegroup %in% c("196_1", "196_3", "201_1", "201_3",
                                                  "180_1", "180_3", "204_1", "204_3", "206_1", "206_3")]),
            soft_ray = shiner + sum(fish_per_area[spp_agegroup %in% c("191_1", "191_3", "121_1", "121_3",
                                                             "291_1", "291_3", "366_5")]),
            spiny_ray = sum(fish_per_area[spp_agegroup %in% c("301_1", "302_1", "331_1", "334_1", "371_1")]),
            all_forage = clupeid + soft_ray + spiny_ray) %>%
  ungroup %>%
  gather(key = "spp_agegroup", # make into tall data so we can add to summary
         value = "fish_per_area", 
         c(clupeid, shiner, soft_ray, spiny_ray, all_forage))

#merge groupings into existing catch summary
catch_summary_by_site <- catch_per_area %>%
  bind_rows(fish_group_summary) %>%
  arrange(yrsam)

### Extrapolate abundance per area up to basinwide estimates   #############################################
# Average catch/m2 over year, agency, depth strata, and spp_agegroup
mean_cpha_by_strata <- catch_summary_by_site %>%
  group_by(year, state, depth_strata, spp_agegroup, species, agecode) %>%
  summarize(mean_fish = mean(fish_per_area),
            sd_fish = sd(fish_per_area)) %>%
  ungroup



# Expand catch/ha to absolute abundance by agency based on estimated area across each depth strata;
#   Estimates of area by depth strata are from Arcview exercise; SAS code claims "Summary attached" but it's not
#     Then, calc fish/ha
yearly_abundance_by_agency <- mean_cpha_by_strata %>%
  mutate(total_abundance = case_when(state == "CA" ~ case_when(depth_strata == "shallow" ~ mean_fish * 1.681E+8,
                                                                depth_strata == "deep" ~ mean_fish * 1.244E+9),
                                     state == "US" ~ case_when(depth_strata == "shallow" ~ mean_fish * 9.281E+8,
                                                                depth_strata == "deep" ~ mean_fish * 1.413E+9)),
         abundance_per_ha = total_abundance/375320) # number per hectare

# now create West Basin summary, calc abs abundance and fish/ha, then saves totals; use this for graphing
# Average catch/m2 over year, agency, depth strata, and spp_agegroup; first, filter out only "important" or
#   classic forage fish groupings, i.e.
yearly_abundance_tall <- yearly_abundance_by_agency %>%
  group_by(year, spp_agegroup, species, agecode) %>%
  summarize(total_abundance = sum(total_abundance),
            abundance_per_ha = total_abundance/375320) %>%
  ungroup %>% 
  left_join(spp_codes %>% select(-latin_name)) %>%
  filter(spp_agegroup %in% c("61_1", "61_3", "63_1", "196_1", "196_3", "201_1", # filter "forage fish" only
                             "201_3", "180_1", "180_3", "204_1", "204_3", "206_1",
                             "206_3", "191_1", "191_3", "121_1", "121_3", "291_1",
                             "291_3", "366_5","301_1", "302_1", "331_1", "334_1",
                             "331_2", "334_2",
                             "371_1", "clupeid", "soft_ray", "spiny_ray", "all_forage")) %>%
  select(-total_abundance)

write_csv(yearly_abundance_tall, "data derived/West Basin absolute abundance tall format.csv")


# spread data and print - similar to older FTG SAS output
yearly_abundance_total <- yearly_abundance_tall %>%
  select(-species, -agecode, -common_name) %>%
  spread(key = spp_agegroup, value = abundance_per_ha) %>%
  select(year, "371_1", "196_3", "196_1", "63_1", "366_5", "191_3", "191_1", "204_1",
         "291_3", "291_1", "201_3", "201_1", "291_3", "291_1", "302_1",
         "301_1", "334_1", "331_1", "331_2", "334_2")

write_csv(yearly_abundance_total, "data derived/West Basin absolute abundance.csv")


# write CSV with same but split by state
yearly_abundance_by_state <- yearly_abundance_by_agency %>%
  group_by(year, state, spp_agegroup) %>%
  summarize(total_abundance = sum(total_abundance),
            abundance_per_ha = total_abundance/375320) %>%
  ungroup %>% 
  filter(spp_agegroup %in% c("61_1", "61_3", "63_1", "196_1", "196_3", "201_1",
                             "201_3", "180_1", "180_3", "204_1", "204_3", "206_1",
                             "206_3", "191_1", "191_3", "121_1", "121_3", "291_1",
                             "291_3", "366_5","301_1", "302_1", "331_1", "334_1",
                             "371_1", "clupeid", "soft_ray", "spiny_ray", "all_forage")) %>%      
  select(-total_abundance) %>% #remove this and next line for tall data
  spread(key = spp_agegroup, value = abundance_per_ha)

write_csv(yearly_abundance_by_state, "data derived/West Basin absolute abundance by agency.csv")


### calc percent difference between this years abundance and last 10 years   #####################
abund_percent_diff <- yearly_abundance_tall %>%
  #filter(!spp_agegroup %in% c("all_forage", "clupeid", "soft_ray", "spiny_ray")) %>%
  group_by(spp_agegroup, common_name, agecode) %>%
  summarize(ten_year_mean = round(mean(abundance_per_ha[which(year %in% seq(this_year-11,this_year-1))])+.001, 1),
            last_year_abundance = round(abundance_per_ha[which(year == this_year-1)]+.001, 1),
            this_year_abundance = round(abundance_per_ha[which(year == this_year)]+.001, 1),
            percent_diff_last_yr = 100*round(((this_year_abundance+.0000001)-last_year_abundance+.0000001)/last_year_abundance+.000001, 3),
            percent_diff_10_yr = 100*round(((this_year_abundance+.0000001)-ten_year_mean+.0000001)/ten_year_mean, 3), #percentage change from 10 yr mean
            percent_diff_last_yr = if_else(is.finite(percent_diff_last_yr), percent_diff_last_yr, 0), #percentage change from 10 yr mean
            percent_diff_10_yr = if_else(is.finite(percent_diff_10_yr), percent_diff_10_yr, 0)) %>% 
  ungroup() %>%
  arrange(common_name, agecode)

write_csv(abund_percent_diff, "data derived/percent change.csv")

beep(5)

