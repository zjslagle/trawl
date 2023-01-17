source("2_abundance summary.R")

### Align OH Interagency summary with ON estimates   #############################
# Started 9/2022. Solved this issue by 11/2022, this code only kept for future reference. ZJS


# 2021 discrepancy - Andy estimate less perch than me    ###########################
# these sites have catch of YOY WAE but not YOY YP
catch_summary %>%
  filter(year == 2021, species %in% c(331, 334), agecode == 1) %>%
  filter(yrsam %in% c("ON2021255", "ON2021282"))

# are they in my CPHA? YES
cpha_by_site %>%
  filter(yrsam %in% c("ON2021255", "ON2021282"))

# 2016 discrepancy     ##############################################
# Andy estimates 8-9% more YOY WAE and YP
all_effort %>%
  filter(year == 2016, is.na(botoxy)) %>% select(crn)
# issue with botoxy == NA?

all_effort %>%
  filter(is.na(botoxy)) %>%
  group_by(year) %>%
  summarize(missing_botoxy = n())

all_catch_raw %>%
  filter(crn %in% c("2168019", "2168025")) # these sites missing botoxy

all_catch %>% filter(crn %in% c("2168019", "2168025"))

# effdist missing issue? probably not   #####################
all_effort %>%
  filter(is.na(effdist), effstat == 1)


### Issues with ON trawling in MI waters?   ##############
all_effort %>%
  filter(agency == "ON", longitude < -83.2900) %>%
  group_by(year) %>%
  summarize(n_trawls = n())

catch_summary %>%
  mutate(area = case_when((agency == "ON" & longitude < -83.2900) ~ "MI", # ON-fished sites in MI waters 1988-95; these get summarized in US waters
                           agency == "ON" ~ "CA",
                           T ~ "dOH")) %>%
  group_by(year, area) %>%
  summarize(sum_catch = sum(sum_catch[species == 334 & agecode == 1]),
            n_trawls = sum(species == 334 & agecode == 1)) %>%
  pivot_wider(id_cols = year, names_from = area, values_from = c(sum_catch, n_trawls), values_fill = 0) %>%
  write_csv("data derived/WAE YOY catch by state.csv")

