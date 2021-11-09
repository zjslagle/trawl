source("3_biomass by site.R")

# Compare randomly generated lengths from R and SAS - do we get different biomass ests with high simulated weights?

# 1994 Data only - (although this dep. on what you switch the SAS code to)
sas_data <- read_csv("SAS vs R Debug/Indiv weights SAS WORKING.csv")


### Combine R and SAS data   #####################################
#clean up SAS data
sas_data <- sas_data %>%
  janitor::clean_names() %>%
  select(year, "crn" = catrec, species, age, length, weight) %>%
  mutate(source = "SAS")

# clean up R data
r_data <- combo_length_data %>%
  left_join(all_effort %>% select(yrsam, year)) %>%
  filter(year == 1994) %>%
  mutate(crn = as.double(crn),
         source = "R",
         age = agecode) %>%
  select(-yrsam)

combo_data <- r_data %>%
                full_join(sas_data)

#### Summarize by source - where are we missing fish?    ###############
# Show differences b/t SAS and R number of lengths
combo_data %>%
  group_by(crn, species, age) %>%
  summarize(R_lengths = sum(source == "R"),
            SAS_lengths = sum(source == "SAS"),
            diff = R_lengths - SAS_lengths) %>%
  filter(diff != 0) %>% view

combo_data %>%
  filter(species == 63, crn == 948015) %>%
  ggplot(aes(x = weight, fill = source))+
    geom_density(alpha = .5)

combo_data %>%
  filter(species == 63, crn == 948015) %>%
  group_by(source) %>%
  summarize(total_wt = sum(weight))


### Ontario deep vs shallow extrapolation   #########################

#find years where Ontario's only "shallow" (< 6 m) are in Michigan waters (i.e., long < -83.29)
all_effort %>%
  filter(!is.na(depth)) %>%
  # mutate(depth_strata = case_when(depth < 6 & longitude < -83.2900 ~ "shallow_OH",
  #                                 depth < 6 ~ "shallow_ON",
  #                                 depth > 5.99 & longitude < -83.2900 ~ "deep_OH",
  #                                 T ~ "deep_ON")) %>%
  mutate(depth_strata = if_else(depth < 6, "shallow", "deep")) %>%
  group_by(year, agency, depth_strata) %>%
  tally() %>%
  pivot_wider(names_from = c(agency, depth_strata), values_from = n, values_fill = 0) %>% view
  write_csv("SAS vs R Debug/ON trawls by depth strata.csv")


