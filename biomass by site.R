source("abs abundance by site.R")

### Calculate Biomass by site, so we can estimate biomass for species-years   ###########################
#read Length-weight regression data (need to redo these at some point, who knows how old these are!)
l_w_reg <- read_excel(here("data", "forlenwt.xls")) %>% janitor::clean_names()

# total catches by species and age code
catch_summary_all_spp

### Make new Tibble with simulated TL for unmeasured fish and estimated wts (from L-W reg) ##########################################
#need to count how many actually measured lengths we have, then add rows for "unknown length" fish to estimate TL
all_lengths %>%
  filter(agency == "OH") %>%
  group_by(crn, species, agecode) %>%
  summarize(n_lengths = n(),
            mean_length = mean(length, na.rm = T), # one spp-agegroup entry has NA's (crn = 1908593); ignore since n=1
            sd_length = if_else(n_lengths > 1, sd(length, na.rm = T), 0)) -> length_summary

# join to catches, calc # of lengths to create (with mean and sd)
catch_summary_all_spp %>%
  arrange(crn) %>%
  left_join(length_summary) %>% 
  mutate(n_lengths = if_else(is.na(n_lengths), 0, as.double(n_lengths)),
         n_lengths_to_make = sum_catch - n_lengths) %>%                   # calc N lengths to make
  filter(n_lengths_to_make > 0, n_lengths > 0) %>%                  #remove species/age classes that we don't measure (e.g. adult sportfish)
  select(crn, year, agency, species, agecode, mean_length, sd_length, n_lengths_to_make) -> lengths_to_make
  
sum(lengths_to_make$n_lengths_to_make) # new dataframe needs this many rows

# new data frame with empty lengths
new_lengths <- lengths_to_make[rep(row.names(lengths_to_make),lengths_to_make$n_lengths_to_make), 1:7] 

# did we do it right?
sum(lengths_to_make$n_lengths_to_make) == nrow(new_lengths) # Yay!

# draw simulated lengths from a normal dist
new_lengths %>%
  mutate(length = rnorm(n(), mean = mean_length, sd = sd_length),
         length_type = "simulated") %>%
  select(-mean_length, -sd_length, -year) -> new_lengths

# join measured and simulated lengths into one
all_lengths %>%
  select(-projcode) %>%
  filter(agency == "OH") %>%
  mutate(length_type = "measured") %>%
  full_join(new_lengths) %>%
  arrange(crn, species, agecode) -> full_lengths

# generate weights based on old L-W regression
full_lengths %>%
  left_join(l_w_reg) %>%
  mutate(weight = int*(length^slope)) %>%
  select(crn, species, agecode, length, weight) -> all_fish_data

### Estimate biomass per m^2 ###############################
# Much of this code duplicates that found in "abs abundance by site.R"
# Apply FPCs and generate biomass per m^2 by site

#first, get biomass per site
all_fish_data %>%
  group_by(crn, species, agecode) %>%
  summarize(total_weight = sum(weight))%>%
  left_join(all_effort %>% select(projcode, agency, crn, year, month, day, effdur, effdist, effstat, depth, totdist, longitude, latitude)) %>% #join effort data
  filter(effstat == 1, year != 1987, !is.na(effdist))%>%
  mutate(depth_strata = case_when(depth > 6 ~ "deep", #called DEPT = 1 in old code
                                  T ~ "shallow"),
         effdist = effdist * 1852,
         totdist = totdist * 1852,
         deploy_dist = totdist - effdist,
         hypot = (depth^2) + (deploy_dist^2),
         dep_dist = hypot^(1/2),
         total_dist = dep_dist + effdist,
         #next section applies correction for OH trawl before 2002:
         total_weight = case_when((agency == "OH" & year < 2002) ~ case_when(species == 63 & agecode == 1   ~ total_weight*1.216,
                                                                          species == 196 & agecode == 1  ~ total_weight*2.170,
                                                                          species == 196 & agecode == 3  ~ total_weight*2.170,
                                                                          species == 302 & agecode == 1  ~ total_weight*1.644,
                                                                          species == 331 & agecode == 1  ~ total_weight*1.321,
                                                                          species == 331                 ~ total_weight*1.185,
                                                                          species == 334 & agecode == 1  ~ total_weight*1.520,
                                                                          species == 371 & agecode == 3  ~ total_weight*1.505,
                                                                          T                              ~ total_weight), #now OH 2002 - present:
                               (agency == "OH" & year > 2001) ~ case_when(species == 63 & agecode == 1   ~ total_weight*2.362,
                                                                          species == 196 & agecode == 1  ~ total_weight*1.494,
                                                                          species == 196 & agecode == 3  ~ total_weight*1.494,
                                                                          species == 291 & agecode == 1  ~ total_weight*0.704,
                                                                          species == 291 & agecode == 3  ~ total_weight*0.704,
                                                                          species == 301 & agecode == 1  ~ total_weight*1.121,
                                                                          species == 302 & agecode == 1  ~ total_weight*3.203,
                                                                          species == 334 & agecode == 1  ~ total_weight*1.561,
                                                                          species == 366 & agecode == 1  ~ total_weight*0.423,
                                                                          species == 366                 ~ total_weight*0.423,
                                                                          species == 371 & agecode == 3  ~ total_weight*0.598,
                                                                          T                              ~ total_weight),
                               T ~ total_weight),
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
         biomass_per_area = total_weight/area_swept) %>%
  group_by(projcode, year, month, day, crn, species, agecode, agency, state, area_swept, depth_strata) %>%
  summarize(biomass_per_area = sum(biomass_per_area)) %>%
  ungroup-> biomass_summary_all_spp_by_area
