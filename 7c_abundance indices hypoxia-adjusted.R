source("7a_hypoxia and catch prep.R")

### Abundance Summary, Adjusted with hypoxic nearest neighbor    ##################################
# Following Kraus, R.T., Knight, C.T., Farmer, T.M., Gorman, A.M., Collingsworth, P.D., Warren, G.J., 
# Kocovsky, P.M., Conroy, J.D., 2015. Dynamic hypoxic zones in Lake Erie compress fish habitat, 
# altering vulnerability to fishing gears. Can. J. Fish. Aquat. Sci. 72, 797–806. 
# https://doi.org/10.1139/cjfas-2014-0517
#
# Implement hypoxia adjustments to the abundance summary



# quick Age-0 walleye (or whatever) graph
# cpha_by_site %>%
#   group_by(year, spp_agegroup) %>%
#   summarize(mean_cpha = mean(cpha)) %>%
#   filter(spp_agegroup == "334_1") %>%
#   ggplot(aes(x = year, y = mean_cpha))+
#     geom_col()

### Create functional groupings (spiny-rayed, clupeid, soft-rayed, and total) in new tibble   ################
fish_group_summary <- catch_per_area %>%
  group_by(year, yrsam, state, depth_strata) %>%
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
                             "371_1", "clupeid", "soft_ray", "spiny_ray", "all_forage")) %>%
  select(-total_abundance)

write_csv(yearly_abundance_tall, "data derived/HYPOXIA - West Basin absolute abundance tall format.csv")


# spread data and print - similar to older FTG SAS output
yearly_abundance_total <- yearly_abundance_tall %>%
  select(-species, -agecode, -common_name) %>%
  spread(key = spp_agegroup, value = abundance_per_ha) %>%
  select(year, "371_1", "196_3", "196_1", "63_1", "366_5", "191_3", "191_1", "204_1",
         "291_3", "291_1", "201_3", "201_1", "291_3", "291_1", "302_1",
         "301_1", "334_1", "331_1")

write_csv(yearly_abundance_total, "data derived/HYPOXIA - West Basin absolute abundance.csv")


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

write_csv(yearly_abundance_by_state, "data derived/HYPOXIA - West Basin absolute abundance by agency.csv")


beep(5)

