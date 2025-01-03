source("1_read data.R")

### Abundance Summary, Adjusted with hypoxic nearest neighbor    ##################################
# Following Kraus, R.T., Knight, C.T., Farmer, T.M., Gorman, A.M., Collingsworth, P.D., Warren, G.J., 
# Kocovsky, P.M., Conroy, J.D., 2015. Dynamic hypoxic zones in Lake Erie compress fish habitat, 
# altering vulnerability to fishing gears. Can. J. Fish. Aquat. Sci. 72, 797–806. 
# https://doi.org/10.1139/cjfas-2014-0517
#
# Implement hypoxia adjustments to the abundance summary
# should source "spatial_2 - nearest neighbor.R" every year to rerun this analysis, 
# but it takes awhile. Here, we read in the nearest-neighbor data
neighbor_data <- readRDS(file = "data sources/neighbor_data.RDS")

# join nearest neighbor data to all_effort and create criteria for whether to apply to apply the correction
# neighbor_dist is in meters.
effort_hypoxia <- all_effort %>%
  left_join(neighbor_data) %>% 
  mutate(do_diff  = botoxy - neighbor_botoxy,
         apply_do_rule = if_else(botoxy > 2 & neighbor_botoxy < 2 & neighbor_dist < 5600, 
                                 TRUE, FALSE))

# table showing when/what agency has botoxy2 (2nd O2 reading at end of trawl transect)
table(effort_hypoxia$year, effort_hypoxia$agency, is.na(effort_hypoxia$botoxy2))

### Create Catch Summary   #############################################################
# sum over CRN (only summarizes ~10 rows), join effort, filter out bad trawls
draft_catch_summary <- all_catch %>%
  group_by(year, agency, yrsam, crn, species, agecode) %>% 
  summarize(sum_catch = sum(catch)) %>% 
  ungroup() %>% 
  left_join(effort_hypoxia %>% 
              select(year, agency, yrsam, effdist, effstat)) %>%
  arrange(year, yrsam) %>% 
  filter(effstat == 1, year != 1987, !is.na(effdist)) #last part filters out one site in '93

#filter only spp-age groups that we care about, sum, then add explicit zero catches
catch_summary <- draft_catch_summary %>%
  mutate(spp_agegroup = paste0(species, "_", agecode)) %>%
  group_by(year, agency, yrsam, species, agecode, spp_agegroup) %>%
  summarize(sum_catch = sum(sum_catch)) %>%
  ungroup() %>%
  complete(nesting(year, agency, yrsam), 
           nesting(species, agecode, spp_agegroup), 
           fill = list(sum_catch = 0))

#join in effort stats
catch_summary <- catch_summary %>%
  left_join(effort_hypoxia %>% 
              select(year, agency, yrsam, depth, effdist, longitude, botoxy, neighbor_dist, neighbor_botoxy, apply_do_rule, do_diff)) %>%
  arrange(year, yrsam)


### Apply Fishing Power Corrections and calculate abundance per area   ###################################

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
         sum_catch = if_else(apply_do_rule == T, sum_catch/100, sum_catch), # <<<< HYPOXIA CORRECTION    <<<<<<<<<<<<<
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

cpha_by_site <- catch_per_area %>%
  group_by(year, yrsam, state, depth_strata, spp_agegroup, species, agecode) %>%
  summarize(cpha = fish_per_area*10000) %>% #convert meters^2 to hectares
  ungroup

#doublecheck that we didn't lose a bunch of sites somehow!
cpha_by_site %>% 
  select(year, state, yrsam) %>% unique %>% 
  xtabs(~year+state, data = .)

#write to CSV
# cpha_by_site %>% write_csv("data derived/HYPOXIA - Interagency CPHA by site.csv")
