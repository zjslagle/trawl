source("ohio read data.R")


### ****** NOT CURRENTLY WORKING RIGHT   **********   ###


### Calculate Biomass by site, so we can estimate biomass for species-years   ###########################

#read Length-weight regression data (need to redo these at some point, who knows how old these are!)
l_w_reg <- read_excel("data sources/forlenwt.xls") %>% janitor::clean_names()


### Make new Tibble with simulated TL for unmeasured fish and estimated wts (from L-W reg) ##########################################
#need to count how many actually measured lengths we have, then add rows for "unknown length" fish to estimate TL
oh_all_lengths %>%
  group_by(crn, species, agecode) %>%
  summarize(n_lengths = n(),
            mean_length = mean(length, na.rm = T), # one spp-agegroup entry has NA's (crn = 1908593); ignore since n=1
            sd_length = if_else(n_lengths > 1, sd(length, na.rm = T), 0)) %>% 
  ungroup %>%
  arrange(crn, species) -> oh_length_summary

# format ohio catches - filter out bad trawls and summarize over crn/spp/agegroup (if necc)
oh_all_catch %>%
  left_join(oh_all_effort) %>%
  filter(effstat == 1, year > 1991, !is.na(effdist)) %>%
  group_by(year, crn, species, agecode) %>%
  summarize(sum_catch = sum(catch)) %>%
  ungroup %>%
  arrange(crn, species) -> oh_catch_summary

# join to catches, calc # of lengths to create (with mean and sd)
oh_catch_summary %>%
  left_join(oh_length_summary) %>%  
  mutate(n_lengths = if_else(is.na(n_lengths), 0, as.double(n_lengths)),
         n_lengths_to_make = sum_catch - n_lengths) %>%  # filter(n_lengths_to_make < 0) %>% tail                # calc N lengths to make
  filter(n_lengths_to_make > 0, n_lengths > 0) %>%                  #remove species/age classes that we don't measure (e.g. adult sportfish) OR WERE ALREADY MEASURED
  select(crn, year, species, agecode, mean_length, sd_length, n_lengths, n_lengths_to_make) -> lengths_to_make

# new data frame with empty lengths
new_lengths <- lengths_to_make[rep(row.names(lengths_to_make),lengths_to_make$n_lengths_to_make), 1:7] 

# did we do it right? should be TRUE
sum(lengths_to_make$n_lengths_to_make) == nrow(new_lengths) 

# draw simulated lengths from a normal dist
new_lengths %>%
  mutate(length = round(rnorm(n(), mean = mean_length, sd = sd_length),0),
         length_type = "simulated") %>%
  select(-mean_length, -sd_length, -year, -n_lengths) -> new_lengths

# join measured and simulated lengths into one
oh_all_lengths %>%
  mutate(length_type = "measured") %>%
  full_join(new_lengths) %>%
  arrange(crn, species, agecode) -> oh_lengths_combined

# do catch numbers match number of lengths?
nrow(oh_lengths_combined) == sum(oh_catch_summary$sum_catch)
sum(oh_all_catch$catch)








# generate weights based on old L-W regression - lengths in mm, weights in grams
oh_lengths_combined %>%
  left_join(l_w_reg) %>%
  mutate(weight = int*(length^slope)) %>% 
  select(crn, species, agecode, length, weight) -> oh_all_fish_data

### Summarize biomass by site, including FPCs ##################
oh_all_fish_data %>%
  left_join(oh_all_effort %>% select(crn, year, month, effdist)) %>%
  group_by(year, month, crn, species, agecode, effdist) %>%
  summarize(catch = n(),
            total_weight = sum(weight)) %>%
  ungroup %>%
  mutate(effdist = effdist * 1852,
         total_weight = case_when(year < 2002 ~ case_when(species == 63 & agecode == 1   ~ total_weight*1.216,
                                                          species == 196 & agecode == 1  ~ total_weight*2.170,
                                                          species == 196 & agecode == 3  ~ total_weight*2.170,
                                                          species == 302 & agecode == 1  ~ total_weight*1.644,
                                                          species == 331 & agecode == 1  ~ total_weight*1.321,
                                                          species == 331                 ~ total_weight*1.185,
                                                          species == 334 & agecode == 1  ~ total_weight*1.520,
                                                          species == 371 & agecode == 3  ~ total_weight*1.505,
                                                          T                              ~ total_weight), #now OH 2002 - present:
                           year > 2001 ~ case_when(species == 63 & agecode == 1   ~ total_weight*2.362,
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
        wings = case_when(year < 2002 ~ 4.83,
                          year > 2001 ~ 5.0,
                            T ~ NA_real_),
        area_swept = effdist * wings,
        biomass_per_area = total_weight/area_swept) %>%
  filter(year > 1991) -> oh_biomass_by_crn # should be grams per m2

write_csv(oh_biomass_by_crn, path = "data derived/ohio trawl biomass by site.csv")





#### Test - does my data line up with Marks?     ###############
# reminder - DuFour's code loads packages that mask commands that I like, so might fuck up other stuff
source("DuFour biomass code.R")

# get Giz Shad biomass estimates from this code and DuFour's code and join
oh_biomass_by_crn %>%
  filter(species == 63) %>%
  left_join(Combo2_exp %>% transmute(crn = as.character(CRN), GSHADG)) %>% #rename CRN and drop other columns
  left_join(Catch %>% filter(SPECIES == 63) %>% transmute(crn = as.character(CRN), CATCH)) %>%
  left_join(oh_catch_summary %>% filter(species == 63) %>% transmute(crn, orig_catch = sum_catch)) # should be the right catch n






### Biomass by year/month (averaged over sites)   ################
oh_biomass_by_crn %>%
  group_by(year, month, species, agecode) %>%
  summarize(mean_biomass_per_m2 = mean(total_weight)) %>%
  ungroup %>%
  filter(!is.na(mean_biomass_per_m2)) %>%
  left_join(spp_codes %>% select(-latin_name), by = c(species = "code")) %>%
  complete(year, month, nesting(species, common_name), agecode, fill = list(mean_biomass_per_m2 = 0)) %>%
  arrange(species, agecode, month, year) -> oh_biomass_by_month

write_csv(oh_biomass_by_month, path = here("data", "ohio trawl biomass by month.csv"))
