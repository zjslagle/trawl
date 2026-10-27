source("2_abundance summary.R")

### Summary stats for Travis/ ODNR press release    ##############

# years of survey:
this_year
length(1988:this_year)

# percids index values (whole number), time series means, and ranks
yearly_abundance_tall %>%
  filter(spp_agegroup %in% c("331_1", "334_1")) %>%
  arrange(species, year) %>%
  group_by(spp_agegroup, common_name) %>%
  mutate(rank = dense_rank(desc(abundance_per_ha))) %>%  
  summarize(index_value = round(abundance_per_ha[which(year == this_year)], 0),
            mean_time_series = round(mean(abundance_per_ha[which(year %in% c(1988:(this_year-1)))]),0),
            rank_this_year = rank[which(year == this_year)]) %>% view
  #write_csv("data derived/press release.csv")


# rank output for WAE
yearly_abundance_tall %>%
  filter(spp_agegroup %in% c("334_1")) %>%
  arrange(species, year) %>%
  mutate(rank = dense_rank(desc(abundance_per_ha))) %>% view
#  write_csv("data derived/press release - walleye ranks.csv")
