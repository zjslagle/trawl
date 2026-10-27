source("3_biomass summary.R")

### Locations for FTG Report map  ####################
all_effort %>%
  filter(year == this_year, effstat == 1) %>%
  mutate(latitude = round(latitude, 6),
         longitude = round(longitude, 6),
         survey = "interagency",
         basin = "west",
         agency = if_else(agency == "ON", "OMNRF", "ODNR")) %>%
  select(survey, agency, basin, latitude, longitude) %>%
  write_csv("data derived/ftg report points.csv")

### IA summary CPHAs for Forage_Fish_Trends-All_Agencies    #####################
yearly_abundance_tall %>%
  filter(year == this_year,
         common_name %in% c("Rainbow smelt", "Gizzard shad", "Emerald shiner", "Round goby")|
           spp_agegroup %in% c("soft_ray", "spiny_ray", "clupeid")) %>%
  mutate(agency = "ON + OH",
         basin = "WB",
         species = case_when(!is.na(common_name) ~ str_to_title(common_name),
                             spp_agegroup == "soft_ray" ~ "Soft-rayed",
                             spp_agegroup == "spiny_ray" ~ "Spiny-rayed",
                             spp_agegroup == "clupeid" ~ "Clupeid"),
         age_class = case_when(is.na(agecode) ~ "all",
                               agecode == 1 ~ "YOY",
                               agecode == 3 ~ "YAO",
                               agecode == 5 ~ "all")) %>%
  select(year, agency, basin, species, age_class, abundance_per_ha) %>%
  write_csv("data derived/ftg Forage Fish Trends output.csv")

         