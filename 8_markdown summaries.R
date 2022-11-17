
source("2_abundance summary.R")

this_year # make sure this is ACTUALLY this year!

### EFFORT ###################################################
#count trawl sites, count # of low DO sites
effort_summary <- all_effort %>%
  filter(year == this_year) %>%  # filter(is.na(botoxy < 2)) %>% select(crn, botoxy)
  summarize(trawl_sites = n(),
            good_trawl_sites = sum(effstat == 1),
            low_do_sites = sum(botoxy < 2, na.rm = T),
            missing_do_sites = sum(is.na(botoxy)))

### INDICES    ###############################################
# Index values and comparison to 10-yr mean
abund <- yearly_abundance_tall %>%
  group_by(spp_agegroup, common_name, agecode) %>%
  #filter(spp_agegroup %in% c("334_1", "334_2", "331_1", "331_2")) %>%
  summarize(ten_year_mean = round(mean(abundance_per_ha[which(year %in% seq(this_year-11,this_year-1))])+.001, 1),
            abundance = round(abundance_per_ha[which(year == this_year)]+.001, 1),
            percent_diff_10_yr = sprintf("%0.1f%%", 100*round(((abundance+.0000001)-ten_year_mean+.0000001)/ten_year_mean, 3)), #percentage change from 10 yr mean
            ) %>% 
  ungroup() %>%
  arrange(common_name, agecode)

# ranks in time series
ranks <- yearly_abundance_tall %>%
  group_by(spp_agegroup, common_name) %>%
  mutate(rank = scales::ordinal(rank(-abundance_per_ha))) %>% 
  arrange(year) %>%
  filter(year == this_year) 

# output quick summary
target = "201_1"
abund %>% filter(spp_agegroup == target)

abund$abundance[which(abund$spp_agegroup == "196_1")]
abund$ten_year_mean[which(abund$spp_agegroup == "196_1")]
ranks$rank[which(ranks$spp_agegroup == "196_1")]

