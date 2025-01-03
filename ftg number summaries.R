
source("3_biomass summary.R")

# Numeric summaries for FTG report and meeting presentation - compliments "ftg graphs.R"

# species code list
#yearly_abundance_tall %>% select(spp_agegroup) %>% distinct %>% view

#### Low DO trawls - this year vs previous   ######################
# IMPORTANT - for sites with DO < 2, ON assigns effstat = 0! Be careful not to filter out. There does seem to
#            be some inconsistencies though, in 2022 there's one normoxic, hung-trawl site w/ effstat == 0.
# Counts of hypoxic/good/bad trawls by year - in pre-filtered effort

do_summary <- all_effort_raw %>%
  mutate(effstat = if_else(botoxy < 2, 0, effstat)) %>%
  group_by(year) %>%
  summarize(n_hypoxic = sum(effstat == 0, na.rm = T),
            n_bad = sum(effstat > 1, na.rm = T),
            n_good = sum(effstat == 1, na.rm = T),
            n_total = n(),
            n_na_effdist = sum(is.na(effdist) & effstat == 1),
            percent_low_do = n_hypoxic/n_total)


do_summary %>% tail

# Percent low DO sites graph
do_summary %>%
  ggplot(aes(x = year, y = percent_low_do))+
    geom_col(fill = "orangered", color = "black")+
    labs(x = "Year", y = "Percent of trawl sites", 
         title = "Trawl sites affected by Anoxic Conditions",
         subtitle = "Interagency trawl in the West Basin of Lake Erie, August trawls")+
    theme_minimal()

ggsave("figures/low DO percentage trawls over time.png", width = 7, height = 5, units = "in",dpi = 400)

# Total low DO sites graph
do_summary %>%
  ggplot(aes(x = year, y = n_hypoxic))+
  geom_col(fill = "orangered", color = "black")+
  labs(x = "Year", y = "Hypoxic trawls (n)", 
       title = "Trawl sites affected by Anoxic Conditions",
       subtitle = "Interagency trawl in the West Basin of Lake Erie, August trawls")+
  theme_minimal()

ggsave("figures/low DO trawl count over time.png", width = 7, height = 5, units = "in",dpi = 400)

### Total abundance and spp functional group differences  ##################################
# comparisons to 10 yr mean and last year
percent_diff_groups <- yearly_abundance_tall %>%
  filter(spp_agegroup %in% c("all_forage", "clupeid", "spiny_ray", "soft_ray")) %>%
  group_by(spp_agegroup) %>%
  summarize(ten_year_mean = round(mean(abundance_per_ha[which(year %in% seq(this_year-11,this_year-1))]), 1),
            this_year_abundance = round(abundance_per_ha[which(year == this_year)], 1),
            last_year_abundance = round(abundance_per_ha[which(year == this_year-1)], 1),
            percent_diff_10_yr = round((((this_year_abundance+.0000001)-ten_year_mean)/ten_year_mean*100), 1),
            percent_diff_last_year = round((((this_year_abundance+.0000001)-last_year_abundance)/last_year_abundance)*100, 1)) %>% #percentage change from 10 yr mean
  ungroup()

percent_diff_groups

### percentage of makeup  ############
yearly_abundance_tall %>%
  filter(year == this_year , !(spp_agegroup %in% c("clupeid", "spiny_ray", "soft_ray"))) %>%
  mutate(percent_makeup = round(abundance_per_ha/abundance_per_ha[which(spp_agegroup == "all_forage")] * 100, 1)) %>%
  arrange(-percent_makeup) %>%
  filter(spp_agegroup != "all_forage")


# same but for biomass
biomass_summary_groups %>%
  group_by(group) %>%
  summarize(ten_year_mean = mean(total_biomass_kg_ha[which(year %in% seq(this_year-11,this_year-1))]),
            this_year_biomass = total_biomass_kg_ha[which(year == this_year)],
            last_year_biomass = total_biomass_kg_ha[which(year == this_year-1)],
            percent_diff_10_yr = (((this_year_biomass+.0000001)-ten_year_mean)/ten_year_mean)*100,
            percent_diff_last_year = (((this_year_biomass+.0000001)-last_year_biomass)/last_year_biomass)*100) %>% #percentage change from 10 yr mean
  ungroup()

biomass_summary_all_spp %>%
  group_by(species, agecode) %>%
  summarize(ten_year_mean = mean(total_biomass_kg_ha[which(year %in% seq(this_year-11,this_year-1))]),
            this_year_biomass = total_biomass_kg_ha[which(year == this_year)],
            last_year_biomass = total_biomass_kg_ha[which(year == this_year-1)],
            percent_diff_10_yr = (((this_year_biomass+.0000001)-ten_year_mean)/ten_year_mean)*100,
            percent_diff_last_year = (((this_year_biomass+.0000001)-last_year_biomass)/last_year_biomass)*100) %>% #percentage change from 10 yr mean
  ungroup() %>%
  filter(agecode == 1, species %in% c(63, 301, 331, 291))




### individual spp summaries    ############### 
# -------- or just use the output file, really
abund_percent_diff %>%
  #filter(spp_agegroup %in% c("334_1", "331_1"))            # turn these on/off for diff spp
  #filter(spp_agegroup %in% c("301_1", "302_1", "63_1"))      
  #filter(spp_agegroup %in% c("121_1", "121_3"))
  #filter(spp_agegroup %in% c("196_1", "196_3"))
  filter(spp_agegroup %in% c("366_5", "206_1", "291_1", "291_3")) 

abund_percent_diff %>% 
  arrange(-percent_diff_10_yr)

### Output Tables     ##################
percent_diff_groups %>%
  select(spp_agegroup, ten_year_mean, this_year_abundance, percent_diff_10_yr) %>%
  mutate(percent_diff_10_yr = round(percent_diff_10_yr, 1)) %>%
  write_csv("data derived/Table 2_1_3_1 groups.csv")

abund_percent_diff %>%
  filter(spp_agegroup %in% c("196_1", "196_3", "371_1", "63_1", "121_1", "121_3", "366_5", "334_1", "302_1", "301_1", "331_1")) %>%
  select(common_name, ages, ten_year_mean, this_year_abundance, percent_diff_10_yr) %>%
  arrange(common_name, ages) %>%
  write_csv("data derived/Table 2_1_3_2 species.csv")
