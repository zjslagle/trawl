
source("2_abundance summary.R")

# Numeric summaries for FTG report and meeting presentation - compliments "ftg graphs.R"

# species code list
yearly_abundance_tall %>% select(spp_agegroup) %>% distinct %>% view

#### Low DO trawls - this year vs previous   ######################
all_effort %>%
  group_by(year) %>%
  summarize(trawl_sites = n(),
            good_trawl_sites = sum(effstat == 1),
            low_do_sites = sum(botoxy < 2, na.rm = T),
            percent_low_do = 100*low_do_sites/trawl_sites) -> do_summary #old data has missing DO

do_summary %>% tail

do_summary %>%
  ggplot(aes(x = year, y = percent_low_do))+
    geom_area()+
    labs(x = "Year", y = "Percent of trawl sites", 
         title = "Trawl sites affected by Anoxic Conditions",
         subtitle = "Interagency trawl in the West Basin of Lake Erie, August trawls")+
    theme_minimal()

ggsave("figures/low DO trawls over time.png", width = 7, height = 5, units = "in",dpi = 400)

### Total abundance and spp functional group differences  ##################################
# comparisons to 10 yr mean and last year
yearly_abundance_tall %>%
  filter(spp_agegroup %in% c("all_forage", "clupeid", "spiny_ray", "soft_ray")) %>%
  group_by(spp_agegroup) %>%
  summarize(ten_year_mean = mean(abundance_per_ha[which(year %in% seq(this_year-11,this_year-1))]),
            this_year_abundance = abundance_per_ha[which(year == this_year)],
            last_year_abundance = abundance_per_ha[which(year == this_year-1)],
            percent_diff_10_yr = (((this_year_abundance+.0000001)-ten_year_mean)/ten_year_mean)*100,
            percent_diff_last_year = (((this_year_abundance+.0000001)-last_year_abundance)/last_year_abundance)*100) %>% #percentage change from 10 yr mean
  ungroup()


### percentage of makeup  ############
yearly_abundance_tall %>%
  filter(year == this_year , !(spp_agegroup %in% c("clupeid", "spiny_ray", "soft_ray"))) %>%
  mutate(percent_makeup = round(abundance_per_ha/abundance_per_ha[which(spp_agegroup == "all_forage")] * 100, 1)) %>%
  arrange(-percent_makeup)

### individual spp summaries    ##############
yearly_abundance_tall %>%
  #filter(spp_agegroup %in% c("334_1", "331_1")) %>%           # turn these on/off for diff spp
  #filter(spp_agegroup %in% c("301_1", "302_1", "63_1")) %>%
  #filter(spp_agegroup %in% c("196_1", "196_3")) %>%
  filter(spp_agegroup %in% c("366_5", "206_1", "291_1", "291_3")) %>%
  group_by(spp_agegroup) %>%
  summarize(ten_year_mean = mean(abundance_per_ha[which(year %in% seq(this_year-11,this_year-1))]),
            this_year_abundance = abundance_per_ha[which(year == this_year)],
            last_year_abundance = abundance_per_ha[which(year == this_year-1)],
            percent_diff_10_yr = (((this_year_abundance+.0000001)-ten_year_mean)/ten_year_mean)*100,
            percent_diff_last_year = (((this_year_abundance+.0000001)-last_year_abundance)/last_year_abundance)*100) %>% #percentage change from 10 yr mean
  ungroup()
