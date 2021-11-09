#source("read data - 2020 format.R")

### OLD Abundance summary  - for reference only, use 2_cpha and abundance summary.R for actual summaries

# sum over CRN (only summarizes ~10 rows), join effort, filter (not sure if last step is necc)
draft_catch_summary <- all_catch %>%
      group_by(projcode, year, agency, yrsam, crn, species, agecode) %>% 
      summarize(sum_catch = sum(catch)) %>% ungroup() %>%
      left_join(all_effort %>% 
                  select(projcode, year, agency, yrsam, crn, depth, effdur, effdist, effstat, totdist, longitude, latitude)) %>%
      arrange(year, crn, projcode) %>%
      filter(effstat == 1, year != 1987, !is.na(effdist))

#spp_codes %>% filter(code == 177) #look up species
#spp_codes %>% filter(grepl("redhorse",common_name)) #look up species groups (case sensitive)

# create spp/age groups for summaries - no easy way to do it
draft_catch_summary %>%
  filter(!(species %in% c(282, 31))) %>% #remove stickleback and sturgeon
  mutate(species = case_when(species == 206 ~ 204, #group mimic and sand shiners
                             species == 210 ~ 180, # blacknose dace -> 180 (180 is a placeholder)
                             species == 184 ~ 180, # redside dace -> 180
                             T ~ species),
         spp_agegroup = case_when(species == 61 ~ "alewife_all",
                                  species == 63 & agecode == 1 ~ "giz_shad_yoy",
                                  species == 91 & agecode == 1 ~ "whitefish_yoy",
                                  species == 91 & agecode > 1 ~ "whitefish_adult",
                                  species == 121 & agecode == 1 ~ "smelt_yoy",
                                  species == 121 & agecode > 1 ~ "smelt_adult",
                                  species == 161 ~ "quillback_all",
                                  species == 163 & agecode == 1 ~ "w_sucker_yoy",
                                  species == 163 & agecode > 1 ~ "w_sucker_adult",
                                  species == 166 ~ "buffalo_all",
                                  species %in% c(168, 169, 170, 171, 172, 173, 177) ~ "redhorse_all", #ZJS changed this from SAS script to include all redhorse
                                  species == 180 & agecode == 1 ~ "notropis_yoy",
                                  species == 180 & agecode > 1 ~ "notropis_adult",
                                  species == 181 ~ "goldfish_all",
                                  species == 186 & agecode > 1 ~ "carp_adult",
                                  species == 191 & agecode == 1 ~ "s_chub_yoy",
                                  species == 191 & agecode > 1 ~ "s_chub_adult",
                                  species == 196 & agecode == 1 ~ "em_shiner_yoy",
                                  species == 196 & agecode > 1 ~ "em_shiner_adult",
                                  species == 201 & agecode == 1 ~ "spot_shiner_yoy",
                                  species == 201 & agecode > 1 ~ "spot_shiner_adult",
                                  species == 204 & agecode == 1 ~ "sand_shiner_yoy",
                                  species == 208 ~ "bluntnose_all",
                                  species == 209 ~ "fathead_all",
                                  species == 216 ~ "stoneroller_all",
                                  species == 232 ~ "y_bullhead_all", #BUTTERCAT
                                  species == 233 ~ "b_bullhead_all",
                                  species == 234 & agecode == 1 ~ "channel_cat_yoy",
                                  species == 234 & agecode > 1 ~ "channel_cat_adult",
                                  species == 235 & agecode == 1 ~ "stonecat_all",
                                  species == 291 & agecode == 1 ~ "troutperch_yoy",
                                  species == 291 & agecode > 1 ~ "troutperch_adult",
                                  species == 301 & agecode == 1 ~ "w_perch_yoy",
                                  species == 301 & agecode == 2 ~ "w_perch_yrg",
                                  species == 301 & agecode > 2 ~ "w_perch_adult",
                                  species == 302 & agecode == 1 ~ "w_bass_yoy",
                                  species == 311 ~ "rock_bass_all",
                                  species == 313 ~ "pumpkin_all",
                                  species == 314 ~ "bluegill_all",
                                  species == 316 & agecode == 1 ~ "smb_yoy",
                                  species == 316 & agecode > 1 ~ "smb_adult",
                                  species == 317 ~ "lmb_yoy",
                                  species == 318 ~ "w_crappie_all",
                                  species == 319 ~ "b_crappie_all",
                                  species == 331 & agecode == 1 ~ "y_perch_yoy",
                                  species == 331 & agecode == 2 ~ "y_perch_yrg",
                                  species == 331 & agecode > 2 ~ "y_perch_adult",
                                  species == 334 & agecode == 1 ~ "walleye_yoy",
                                  species == 334 & agecode == 2 ~ "walleye_yrg",
                                  species == 334 & agecode > 2 ~ "walleye_adult",
                                  species == 342 & agecode == 1 ~ "logperch_yoy",
                                  species == 342 & agecode > 1 ~ "logperch_adult",
                                  species == 344 ~ "b_darter_all",
                                  species == 361 ~ "brook_sil_all",
                                  species == 366 ~ "goby_adult", #all gobies thrown into adult
                                  species == 371 & agecode == 1 ~ "drum_yoy",
                                  species == 371 & agecode > 1 ~ "drum_adult",
                                  species == 31 ~ "sturgeon_all",
                                  species == 81 ~ "lake_trout_all",
                                  T ~ "dont_care")) -> draft_catch_summary

#error check/examine what we're leaving out. poor sand shiners.
#also use this code to find new AIS spp summaries (e.g. tubenose)
draft_catch_summary %>%
  left_join(spp_codes %>% select(species, common_name)) %>%
  filter(spp_agegroup == "dont_care") 

#filter only spp-age groups that we care about, sum, then fill in zeros for zero catches
catch_summary <- draft_catch_summary %>%
  filter(spp_agegroup != "dont_care") %>%
  group_by(projcode, year, agency, yrsam, spp_agegroup) %>%
  summarize(sum_catch = sum(sum_catch)) %>%
  ungroup() %>%
  complete(nesting(projcode, year, agency, yrsam), spp_agegroup, fill = list(sum_catch = 0))

#join in effort stats
catch_summary <- catch_summary %>%
  left_join(all_effort %>% 
              select(projcode, year, month, day, agency, yrsam, crn, depth, effdur, effdist, effstat, totdist, longitude, latitude)) %>%
  arrange(year, crn, projcode) %>%
  filter(effstat == 1, year != 1987, !is.na(effdist)) #added effdist filter - without distance, we can't calc volume sampled so NA's introduced


### OLD SAS NOTE - Calc Vol sampled and catch/meter^2 for each spp-age with Fishing Power Correction (FPC) applied   ##################################

#Use  nested case_when calls to find cases of Ohio trawl prior to 2002 and applying one
# fishing correction to them, then finding Ohio trawls 2002-present and applying another. 
catch_summary_by_area <- catch_summary %>%
  mutate(depth_strata = case_when(depth > 6 ~ "deep", #called DEPT = 1 in old code
                                  T ~ "shallow"),
         effdist = effdist * 1852,
         totdist = totdist * 1852,
         deploy_dist = totdist - effdist,
         hypot = (depth^2) + (deploy_dist^2),
         dep_dist = hypot^(1/2),
         total_dist = dep_dist + effdist,
         #next section applies correction for OH trawl before 2002:
         sum_catch = case_when((agency == "OH" & year < 2002) ~ case_when(spp_agegroup == "giz_shad_yoy"    ~ sum_catch*1.216,
                                                                          spp_agegroup == "em_shiner_yoy"   ~ sum_catch*2.170,
                                                                          spp_agegroup == "em_shiner_adult" ~ sum_catch*2.170,
                                                                          spp_agegroup == "w_bass_yoy"      ~ sum_catch*1.644,
                                                                          spp_agegroup == "y_perch_yoy"     ~ sum_catch*1.321,
                                                                          spp_agegroup == "y_perch_yrg"     ~ sum_catch*1.185,
                                                                          spp_agegroup == "y_perch_adult"   ~ sum_catch*1.185,
                                                                          spp_agegroup == "walleye_yoy"     ~ sum_catch*1.520,
                                                                          spp_agegroup == "drum_adult"      ~ sum_catch*1.505,
                                                                          T                                 ~ sum_catch), #now OH 2002 - present:
                               (agency == "OH" & year > 2001) ~ case_when(spp_agegroup == "giz_shad_yoy"     ~ sum_catch*2.362,
                                                                          spp_agegroup == "em_shiner_yoy"    ~ sum_catch*1.494,
                                                                          spp_agegroup == "em_shiner_adult"  ~ sum_catch*1.494,
                                                                          spp_agegroup == "troutperch_yoy"   ~ sum_catch*0.704,
                                                                          spp_agegroup == "troutperch_adult" ~ sum_catch*0.704,
                                                                          spp_agegroup == "w_perch_yoy"      ~ sum_catch*1.121,
                                                                          spp_agegroup == "w_bass_yoy"       ~ sum_catch*3.203,
                                                                          spp_agegroup == "walleye_yoy"      ~ sum_catch*1.561,
                                                                          spp_agegroup == "goby_yoy"         ~ sum_catch*0.423,
                                                                          spp_agegroup == "goby_adult"       ~ sum_catch*0.423,
                                                                          spp_agegroup == "drum_adult"       ~ sum_catch*0.598,
                                                                          T                                  ~ sum_catch),
                               T ~ sum_catch),
         #OLD COMM: *OLD BOAT TRAWL OPENING (<2002) - SEE FTG REPORT FOR METHODS AND ANALYSIS
         #            NEW BOAT TRAWL OPENING (PROBABLY NEED A MORE RIGOROUS SCANMAR EST.)
          gape = case_when(agency == "OH" ~ case_when(year < 2002 ~ 1.5,
                                                      year > 2001 ~ 2.4,
                                                      T ~ NA_real_), #must used NA_real_, else we get a matching error
                           agency == "ON" ~ 1.16),
          wings = case_when(agency == "OH" ~ case_when(year < 2002 ~ 4.83,
                                                     year > 2001 ~ 5.0,
                                                     T ~ NA_real_),
                            agency == "ON" ~ 5.02),
          state = case_when((agency == "ON" & longitude > -83.2900) ~ "ON", # setting Michigan sites to state = OH; see note from J. Tyson below
                            agency == "ON" ~ "OH",
                            T ~ "OH"),
          area_swept = effdist * wings,
          vol_swept = area_swept * gape,
          fish_per_area = sum_catch/area_swept) %>% #catch per m^2
  group_by(year, month, day, crn, yrsam, spp_agegroup, agency, state, area_swept, depth_strata) %>%
    summarize(fish_per_area = sum(fish_per_area)) %>%
  ungroup

# RE: state == MI sites (from above):
# Email from Jeff Tyson, Aug 2019: 
# In the original IA trawling program MI sites were included in the initial site selections. OMNR agreed to conduct trawling there to assess 
#   those sites because of MDNR constraints.  If I recall, OMNR ran into operational constraints in 1995, so we agreed to pick up a couple of the sites 
#   on our west run to try and at least partially cover off on these.  Given logistic challenges with the west run and adding two additional sites, 
#   we had discussions with MDNR to see if they could start picking them up, but I’m not sure it ever came to fruition.  
#   So, in a nutshell, these sites were a part of the IA trawl survey, OMNR picked them up from 88-94, and we picked up a subset 
#   in 95 and then I don’t think MI, ON, or OH sampled them for a while (until recently I assume).  This was not a part of any trawl comparison work that I am aware of.
#



### Create functional groupings (spiny-rayed, clupeid, soft-rayed, and total) in new tibble   ################
fish_group_summary <- catch_summary_by_area %>%
  group_by(year, month, day, crn, yrsam, agency, state, area_swept, depth_strata) %>%
  summarize(clupeid = sum(fish_per_area[spp_agegroup %in% c("alewife_all", "giz_shad_yoy")]),
            shiner = sum(fish_per_area[spp_agegroup %in% c("em_shiner_adult", "em_shiner_yoy", "spot_shiner_adult", "spot_shiner_yoy",
                                                           "notropis_yoy", "notropis_adult", "sand_shiner_yoy")]),
            soft_ray = shiner + sum(fish_per_area[spp_agegroup %in% c("s_chub_yoy", "s_chub_adult", "smelt_yoy", "smelt_adult",
                                                                      "troutperch_yoy", "troutperch_adult", "goby_yoy", "goby_adult")]),
            spiny_ray = sum(fish_per_area[spp_agegroup %in% c("w_bass_yoy", "w_perch_yoy", "y_perch_yoy", "walleye_yoy", "drum_yoy")]),
            all_fish = clupeid + soft_ray + spiny_ray) %>%
  ungroup %>%
  gather(key = "spp_agegroup", value = "fish_per_area", c(clupeid, shiner, soft_ray, spiny_ray, all_fish)) %>%
  select(year, month, day, crn, yrsam, spp_agegroup, agency, state, area_swept, depth_strata, fish_per_area)

#merge into existing catch summary
catch_summary_by_area %>%
  bind_rows(fish_group_summary) %>%
  arrange(yrsam) -> catch_summary_by_area

### Continue with absolute abundance summary   #############################################

# Average catch/m2 over year, agency, depth strata, and spp_agegroup
yearly_abundance_by_agency <- catch_summary_by_area %>%
  group_by(year, state, depth_strata, spp_agegroup) %>%
  summarize(mean_fish = mean(fish_per_area),
            sd_fish = sd(fish_per_area)) %>%
  ungroup

# Expand catch/m2 to absolute abundance by agency based on estimated area across each depth strata;
#   Estimates of area by depth strata are from Arcview exercise; SAS code claims "Summary attached" but it's not
#     Then, calc fish/ha
yearly_abundance_by_agency <- yearly_abundance_by_agency %>%
  mutate(total_abundance = case_when(state == "ON" ~ case_when(depth_strata == "shallow" ~ mean_fish * 1.681E+8,
                                                           depth_strata == "deep" ~ mean_fish * 1.244E+9),
                                 state == "OH" ~ case_when(depth_strata == "shallow" ~ mean_fish * 9.281E+8,
                                                           depth_strata == "deep" ~ mean_fish * 1.413E+9)),
         abundance_per_ha = total_abundance/375320) # number per hectare


# now create West Basin summary, calc abs abundance and fish/ha, then saves totals; use this for graphing
yearly_abundance_tall <- yearly_abundance_by_agency %>%
  group_by(year, spp_agegroup) %>%
  summarize(total_abundance = sum(total_abundance),
            abundance_per_ha = total_abundance/375320) %>%
  ungroup %>%
  mutate(forage_fish = case_when(spp_agegroup %in% c("em_shiner_adult", "em_shiner_yoy", "spot_shiner_adult", "spot_shiner_yoy", "giz_shad_yoy",
                                                      "notropis_yoy", "notropis_adult", "sand_shiner_yoy", "s_chub_yoy", "s_chub_adult", 
                                                      "smelt_yoy", "smelt_adult", "troutperch_yoy", "troutperch_adult", "goby_yoy", 
                                                     "goby_adult", "w_bass_yoy", "w_perch_yoy", "y_perch_yoy", "walleye_yoy", "drum_yoy",
                                                     "clupeid", "soft_ray", "spiny_ray", "all_fish") ~ TRUE,
                                 T ~ FALSE)) %>%
  filter(forage_fish == TRUE) %>%        
  select(-total_abundance, -forage_fish)

write_csv(yearly_abundance_tall, "data derived/West Basin absolute abundance tall format.csv")


# spread data and print - similar to older FTG SAS output
yearly_abundance_total <- yearly_abundance_tall %>%
  spread(key = spp_agegroup, value = abundance_per_ha) %>%
  select(year, drum_yoy, em_shiner_adult, em_shiner_yoy, giz_shad_yoy, goby_adult, notropis_adult, notropis_yoy, s_chub_adult, s_chub_yoy, sand_shiner_yoy,
         smelt_adult, smelt_yoy, spot_shiner_adult, spot_shiner_yoy, troutperch_adult, troutperch_yoy, w_bass_yoy,
         w_perch_yoy, walleye_yoy, y_perch_yoy, clupeid, soft_ray, spiny_ray, all_fish)

write_csv(yearly_abundance_total, "data derived/West Basin absolute abundance.csv")


# write CSV with same but split by state
yearly_abundance_by_state <- yearly_abundance_by_agency %>%
  group_by(year, state, spp_agegroup) %>%
  summarize(total_abundance = sum(total_abundance),
            rel_abundance = total_abundance/375320) %>%
  ungroup %>%
  mutate(forage_fish = case_when(spp_agegroup %in% c("em_shiner_adult", "em_shiner_yoy", "spot_shiner_adult", "spot_shiner_yoy",
                                                     "notropis_yoy", "notropis_adult", "sand_shiner_yoy", "s_chub_yoy", "s_chub_adult", 
                                                     "smelt_yoy", "smelt_adult", "troutperch_yoy", "troutperch_adult", "goby_yoy", 
                                                     "goby_adult", "w_bass_yoy", "w_perch_yoy", "y_perch_yoy", "walleye_yoy", "drum_yoy",
                                                     "clupeid", "soft_ray", "spiny_ray", "all_fish") ~ TRUE,
                                 T ~ FALSE)) %>%
  filter(forage_fish == TRUE) %>%        
  select(-total_abundance) %>% #remove this and next line for tall data
  spread(key = spp_agegroup, value = rel_abundance)

write_csv(yearly_abundance_by_state, "data derived/West Basin absolute abundance by agency.csv")


### calc percent difference between this years abundance and last 10 years   #####################
abund_percent_diff <- yearly_abundance_tall %>% 
  filter(!str_detect(spp_agegroup, "notropis")) %>% # get rid of "notropis" category, we ID fish now 
  filter(!spp_agegroup %in% c("all_fish", "clupeid", "soft_ray", "spiny_ray")) %>%
  filter(!spp_agegroup %in% c("s_chub_adult", "s_chub_yoy", "sand_shiner_yoy", "troutperch_yoy", 
                              "troutperch_adult", "spot_shiner_adult", "spot_shiner_yoy")) %>%
  group_by(spp_agegroup) %>%
  summarize(ten_year_mean = mean(abundance_per_ha[which(year %in% seq(this_year-11,this_year-1))]),
            this_year_abundance = abundance_per_ha[which(year == this_year)],
            percent_diff = (((this_year_abundance+.0000001)-ten_year_mean)/ten_year_mean)*100) %>% #percentage change from 10 yr mean
  ungroup()  %>% write.csv("data derived/percent change.csv")

