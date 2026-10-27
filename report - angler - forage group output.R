source("2_abundance summary.R")

# group up forage by Angler Report groups
yearly_abundance_tall %>%
  mutate(group_angler = case_when(spp_agegroup == "all_forage" ~ "total",
                                  spp_agegroup == "clupeid" ~ "clupeid",
                                  spp_agegroup %in% c("121_1", "121_3") ~ "smelt",
                                  spp_agegroup %in% c("331_1", "334_1") ~ "percid",
                                  spp_agegroup %in% c("291_1", "191_1", "191_3", "291_3", "366_5") ~ "soft-ray",
                                  spp_agegroup %in% c("196_1", "201_1", "196_3", "201_3") ~ "shiner",
                                  spp_agegroup %in% c("301_1", "302_1", "371_1") ~ "spiny-ray",
                                  T ~ "drop")) %>%
  filter(group_angler != "drop") %>%
  group_by(year, group_angler) %>%
  summarize(catch = sum(abundance_per_ha)) %>%
  mutate(mu = 1,
         month = 8,
         metric = "abundance",
         unit = "CPHA",
         comments = "Interagency trawl") %>%
  select(year, group_angler, mu, month, catch, metric, unit, comments) %>%
  write_csv("data derived/angler report forage groups.csv")
