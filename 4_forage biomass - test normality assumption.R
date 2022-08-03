source("3_biomass summary.R")


library(broom)

# get sample/spp/ageclass combos with 30+ lengths
max_length_to_get <- all_lengths %>%
  group_by(crn, agency, yrsam, species, agecode) %>%
  summarize(n_lengths = n()) %>%
  arrange(yrsam) %>%
  filter(n_lengths > 29) %>%
  select(yrsam, species, agecode) %>%
  ungroup
  
#join actual lengths
max_lengths <- left_join(max_length_to_get, all_lengths)

# run shapiro tests on each dataset and sum
max_lengths %>% 
  select(-crn, -agency, -projcode, -common_name) %>%
  group_by(yrsam, species, agecode) %>%
  do(tidy(shapiro.test(.$length))) %>%
  mutate(is_normal = if_else(p.value < .05, "not_normal", "normal")) %>%
  group_by(is_normal) %>%
  tally
