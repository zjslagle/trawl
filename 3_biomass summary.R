source("2_abundance summary.R")

### Biomass estimates for Interagency trawl - by site and entire basin   ############################
# This code reproduces SAS code originally written by Jeff Tyson for Forage Task Group summaries. Unfortunately,
#   the original code contained numerous errors and previous FTG biomass estimates should NOT be trusted whatsoever.
# See: "SAS to R Interagency Trawl Biomass Summary Conversion.docx"
#   Code finally working Nov 2021. Z Slagle

### Calculate Biomass by site, so we can estimate biomass for species-years   ###########################
#read Length-weight regression data (need to redo these at some point, who knows how old these are!)
l_w_reg <- read_excel("data sources/forlenwt.xls") %>% janitor::clean_names()

### Make new Tibble with simulated TL for unmeasured fish and estimated wts (from L-W reg) ##########################################
#need to count how many actually measured lengths we have, then add rows for "unknown length" fish to estimate TL
length_summary <- all_lengths %>%
  group_by(crn, agency, yrsam, species, agecode) %>%
  summarize(n_lengths = n(),
            mean_length = mean(length, na.rm = T), # one spp-agegroup entry has NA's (crn = 1908593); ignore since n=1
            sd_length = if_else(n_lengths > 1, sd(length, na.rm = T), 0)) %>%
  arrange(yrsam)

# join to catches, calc # of lengths to create (with mean and sd)
lengths_to_make <- catch_summary %>%
  left_join(length_summary) %>% 
  mutate(n_lengths = if_else(is.na(n_lengths), 0, as.double(n_lengths)), # turn NA -> zero
         n_lengths_to_make = sum_catch - n_lengths) %>%                 # calc N lengths to make
  filter(n_lengths_to_make > 0, n_lengths > 0) %>%                  #remove species/age classes that we don't measure (e.g. adult sportfish)
  select(crn, year, agency, yrsam, species, agecode, mean_length, sd_length, n_lengths_to_make)
  
sum(lengths_to_make$n_lengths_to_make) # new dataframe needs this many rows

# new data frame with empty lengths
new_lengths <- lengths_to_make[rep(row.names(lengths_to_make), lengths_to_make$n_lengths_to_make), 1:8] 

# did we do it right? should be TRUE
sum(lengths_to_make$n_lengths_to_make) == nrow(new_lengths) # Yay!

# draw simulated lengths from a normal dist
new_lengths <- new_lengths %>%
  mutate(length = rnorm(n(), mean = mean_length, sd = sd_length),
         length_type = "simulated") %>%
  mutate(length = if_else(length > 0, length, 10)) %>% # random length can be negative, so force negative lengths to be 10 mm (should have minimal effect in end)
  select(-mean_length, -sd_length, -year)

# join measured and simulated lengths into one
full_lengths <- all_lengths %>%
  select(-projcode) %>%
  #filter(agency == "OH") %>%
  mutate(length_type = "measured") %>%
  full_join(new_lengths) %>%
  arrange(agency, crn, species, agecode)


# view cases where catch != total lengths...
# errors in database? missing catch when we had catch? or bad CRNs?
# full_lengths %>%
#   group_by(crn, yrsam, species, agecode) %>%
#     summarize(total_lengths = n()) %>%
#   full_join(catch_summary %>% 
#               select(crn, yrsam, species, agecode, sum_catch)) %>%
#   mutate(diff = total_lengths - sum_catch) %>%
#   filter(diff != 0) %>% view

# generate weights based on old L-W regression
combo_length_data <- full_lengths %>%
  left_join(l_w_reg) %>%
  mutate(weight = int * (length ^ slope)) %>% 
  select(crn, yrsam, agency, species, agecode, length, weight)

catch_summary %>%
  filter(crn == 948027, species == 63)

### Estimate biomass per m^2 ###############################
# Much of this code duplicates that found in "2_cpha and abundance summary.R"
# Apply FPCs and generate biomass per m^2 by site

#first, get biomass per site
biomass_summary_by_site <- combo_length_data %>%
  group_by(crn, yrsam, agency, species, agecode) %>%
    summarize(total_weight = sum(weight)) %>%
  ungroup() %>% # so complete() can function properly
  full_join(all_effort %>% select(projcode, agency, crn, yrsam, year, #join effort data
                                  effdist, effstat, depth, longitude, latitude)) %>% 
  complete(nesting(projcode, agency, crn, yrsam, year,# make explicit zeros - ORDER MATTERS HERE! must complete() AFTER effort joined, otherwise we lose sites w/ zero catch
                   effdist, effstat, depth, longitude, latitude),           
           nesting(species, agecode), 
           fill = list(total_weight = 0)) %>%
  filter(effstat == 1, year != 1987, !is.na(effdist)) %>% 
  mutate(depth_strata = case_when(depth > 6 ~ "deep", #called DEPT = 2 in old code
                                  T ~ "shallow"),
         effdist = effdist * 1852, #convert nautical miles to meters
         #next section applies fishing power corrections; first for OH trawl before 2002:
         total_weight = case_when((agency == "OH" & year < 2002) ~ case_when(species == 63 & agecode == 1   ~ total_weight*1.216,
                                                                          species == 196 & agecode == 1  ~ total_weight*2.170,
                                                                          species == 196 & agecode == 3  ~ total_weight*2.170,
                                                                          species == 302 & agecode == 1  ~ total_weight*1.644,
                                                                          species == 331 & agecode == 1  ~ total_weight*1.321,
                                                                          species == 331                 ~ total_weight*1.185,
                                                                          species == 334 & agecode == 1  ~ total_weight*1.520,
                                                                          species == 371 & agecode == 3  ~ total_weight*1.505,
                                                                          T                              ~ total_weight), 
                                  #now OH 2002 - present:
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
         wings = case_when(agency == "OH" ~ case_when(year < 2002 ~ 4.83,
                                                      year > 2001 ~ 5.0,
                                                      T ~ NA_real_), #must use NA_real_, else we get a matching error
                           agency == "ON" ~ 5.02),
         country = case_when((agency == "ON" & longitude < -83.2900) ~ "US", # far west sites are Michigan sites trawled by ON, so need to be classified into MI/OH waters
                           agency == "ON" ~ "CAN",
                           T ~ "US"),
         area_swept = effdist * wings,
         ha_swept = area_swept/10000,
         biomass_per_area = total_weight/area_swept) # units are per grams per m2

# Output biomass by site - test for now
 # write_csv(biomass_summary_by_site %>% #filter(year == 2021, species == 331, agecode == 1) %>%
 #             select(year, crn, yrsam, species, agecode, agency, total_weight, area_swept, ha_swept, biomass_per_area) %>%
 #              mutate(kg_per_ha = biomass_per_area*10),
 #             "data derived/IA Biomass by CRN.csv")



### Expand biomass per site to West Basin (stratified by depth and agency)   #############################################
#
# Expand biomass/m2 to absolute biomass (total g) by agency based on estimated area across each depth strata;
#   Estimates of area by depth strata are from Arcview exercise; 
#     See: S:\DuFour\1_Projects\1_Trawl\misc\Background info\Interagency Area and Vol Calcs.xls
#
# NOTE - SAS code does this by volume, giving shallow ON sites 62% of total calc'ed biomass despite those sites only making up 
#         0-15% of trawled sites per year. Here I switch to area (following abundance code).
 # Pending LEC approval: recode years missing shallow ON sites to a combined shallow/deep strata. 
 #                        Old code would assume there were zero fish!

# first find years where shallow sites are missing or low sample size
missing_site_years <- biomass_summary_by_site %>%
  group_by(year, country, depth_strata) %>%
    summarize(site_count = n_distinct(crn)) %>% #121 + zeros
  ungroup() %>%
  complete(nesting(year, country), depth_strata, 
           fill = list(site_count = 0)) %>%
  filter(site_count < 3) %>%
  mutate(combine = TRUE) %>%
  select(-site_count)


yearly_biomass_by_agency <- biomass_summary_by_site %>%
  left_join(missing_site_years) %>%
  #mutate(depth_strata = if_else(is.na(combine), depth_strata, "all_depths")) %>% # add 3rd strata "combined" - NEEDS LEC APPROVAL!
  group_by(year, country, depth_strata, species, agecode) %>%
    summarize(mean_biomass_g = mean(biomass_per_area)) %>% #mean biomass (g/m2) per year/country/depth/spp/ageclass
  ungroup() %>%
  mutate(total_biomass_g = case_when(country == "CAN" ~ case_when(depth_strata == "shallow" ~ mean_biomass_g * 1.681E+8,
                                                                  depth_strata == "deep" ~ mean_biomass_g * 1.244E+9,
                                                                  depth_strata == "all_depths" ~ mean_biomass_g * 1.4121e+09),
                                     country == "US" ~ case_when(depth_strata == "shallow" ~ mean_biomass_g * 9.281E+8,
                                                                 depth_strata == "deep" ~ mean_biomass_g * 1.413E+9))) %>%
  select(-mean_biomass_g)                   # basinwide area = 3.7532e+09 m2 or 375320 ha

#sum across lake and state
biomass_summary_all_spp_by_state <- yearly_biomass_by_agency %>%
  group_by(year, country, species, agecode) %>%
    summarize(total_biomass_kg_ha = (sum(total_biomass_g)/1000)/375320) %>% #convert to kg/ha
  arrange(country, year) %>%
  left_join(spp_codes %>% select(-latin_name)) %>%
  left_join(forage_groups)

#Biomass by species across lake
biomass_summary_all_spp <- yearly_biomass_by_agency %>%
  group_by(year, species, agecode) %>%
    summarize(total_biomass_kg_ha = (sum(total_biomass_g)/1000)/375320) %>% #convert to kg/ha
  arrange(species, agecode, year) %>%
  left_join(spp_codes %>% select(-latin_name))

biomass_summary_all_spp %>%
  write_csv("data derived/West Basin biomass estimates by species.csv")



# total biomass
biomass_summary_total <- biomass_summary_all_spp %>%
  left_join(forage_groups) %>%
  filter(!is.na(group)) %>%
  group_by(year) %>%
    summarize(total_biomass_kg_ha = sum(total_biomass_kg_ha)) %>%
  ungroup() %>%
  mutate(group = "all_forage")

# biomass by functional group (spiny rayed, soft rayed, clupeid), join total
biomass_summary_groups <- biomass_summary_all_spp %>%
  left_join(forage_groups) %>%
  group_by(year, group) %>%
    summarize(total_biomass_kg_ha = sum(total_biomass_kg_ha)) %>%
  ungroup() %>%
  filter(!is.na(group)) %>%
  bind_rows(biomass_summary_total)


### Output biomass into older formats   ########################################

# reshape into older format - forage groups Total/Basinwide in wide format
biomass_summary_all_spp %>%
  left_join(forage_groups) %>%
  group_by(year, group) %>%
  summarize(total_biomass_kg_ha = sum(total_biomass_kg_ha)) %>%
  ungroup() %>%
  filter(!is.na(group)) %>%
  pivot_wider(names_from = group,
              values_from = total_biomass_kg_ha) %>%
  mutate(total = clupeid + soft_ray + spiny_ray) %>%
  arrange(year) %>%
  write_csv("data derived/West Basin biomass estimates Basinwide.csv")

beep(3)

