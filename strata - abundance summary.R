source("2_abundance summary.R")

### proposed adjustment: When ON has <3 trawl sites in shallow strata, only estimate 1 strata   ###########
eff_cutoff = 2 #what effort level triggers lumping of strata (less than this num)

# count effort by strata
strata_count <- all_effort %>% filter(effstat == 1, year > 1987) %>%
  mutate(depth_strata = case_when(depth > 6 ~ "deep", 
                                  T ~ "shallow"),
         state = case_when((agency == "ON" & longitude < -83.2900) ~ "US", 
                           agency == "ON" ~ "CA", T ~ "US")) %>%
  group_by(year, state, depth_strata) %>%
  summarize(n_sites = n()) %>%
  ungroup() %>%
  complete(year, state, depth_strata,
           fill = list(n_sites = 0)) %>%
  filter(state == "CA", depth_strata == "shallow") %>%#this only affects shallow Canadian strata, so filter to just that
  select(-depth_strata)

#here's years that we'll lump CAN deep+shallow strata together
strata_count %>% filter(n_sites < eff_cutoff)

# lump strata if n_sites < eff_cutoff, otherwise do normal summary
yearly_abundance_by_agency_mod <- mean_cpha_by_strata %>%
  left_join(strata_count) %>% 
  mutate(total_abundance = case_when(state == "CA" ~ if_else(n_sites >= eff_cutoff, #TRUE = standard
                                                             case_when(depth_strata == "shallow" ~ mean_fish * 1.681E+8,
                                                                       depth_strata == "deep" ~ mean_fish * 1.244E+9),
                                                             mean_fish * 1.4121E+9), # FALSE = lump 'em!
                                     state == "US" ~ case_when(depth_strata == "shallow" ~ mean_fish * 9.281E+8,
                                                               depth_strata == "deep" ~ mean_fish * 1.413E+9)),
         abundance_per_ha = total_abundance/375320) # number per hectare
         
### Remainder is same as 2_abundance summary   #################################################################
# now create West Basin summary, calc abs abundance and fish/ha, then saves totals; use this for graphing
# Average catch/m2 over year, agency, depth strata, and spp_agegroup; first, filter out only "important" or
#   classic forage fish groupings, i.e.
yearly_abundance_tall_mod <- yearly_abundance_by_agency_mod %>%
  group_by(year, spp_agegroup, species, agecode) %>%
  summarize(total_abundance = sum(total_abundance),
            abundance_per_ha = total_abundance/375320) %>%
  ungroup %>% 
  left_join(spp_codes %>% select(-latin_name)) %>%
  filter(spp_agegroup %in% c("61_1", "61_3", "63_1", "196_1", "196_3", "201_1", # filter "forage fish" only
                             "201_3", "180_1", "180_3", "204_1", "204_3", "206_1",
                             "206_3", "191_1", "191_3", "121_1", "121_3", "291_1",
                             "291_3", "366_5","301_1", "302_1", "331_1", "334_1",
                             "331_2", "334_2",
                             "371_1", "clupeid", "soft_ray", "spiny_ray", "all_forage")) %>%
  select(-total_abundance)

#write_csv(yearly_abundance_tall_mod, "data derived/West Basin absolute abundance tall format - strata corrected.csv")


# spread data and print - similar to older FTG SAS output
yearly_abundance_total_mod <- yearly_abundance_tall_mod %>%
  select(-species, -agecode, -common_name) %>%
  spread(key = spp_agegroup, value = abundance_per_ha) %>%
  select(year, "371_1", "196_3", "196_1", "63_1", "366_5", "191_3", "191_1", "204_1",
         "291_3", "291_1", "201_3", "201_1", "291_3", "291_1", "302_1",
         "301_1", "334_1", "331_1", "331_2", "334_2")

#write_csv(yearly_abundance_total_mod, "data derived/West Basin absolute abundance - strata corrected.csv")


# write CSV with same but split by state
yearly_abundance_by_state_mod <- yearly_abundance_by_agency_mod %>%
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

#write_csv(yearly_abundance_by_state_mod, "data derived/West Basin absolute abundance by agency - strata corrected.csv")         

### Now plot the changes!     ##############################
#combine trad. summary and strata-adjusted summary, filter to WAE/YP/total forage
abundance_combo <- yearly_abundance_tall %>%
  mutate(type = "Status quo") %>%
  full_join(yearly_abundance_tall_mod %>% mutate(type = "Corrected")) %>%
  filter(spp_agegroup %in% c("331_1", "334_1", "all_forage"))

#walleye
abundance_combo %>%
  filter(spp_agegroup == "334_1") %>%
  ggplot(aes(x = year, y = abundance_per_ha, fill = type))+
    geom_col(position = position_dodge(), col = "white")+
    scale_x_continuous(expand = c(0,.5), 
                       breaks = seq(1988,this_year,by=3))+
    scale_y_continuous(expand = c(0,0))+
    scale_fill_manual(values = c("#68affc", "#ff864a"))+
    labs(x="", y="Fish/ha", title = paste0("Walleye/ha; eff_cutoff = ", eff_cutoff),
         caption = paste0("Pooling shallow & deep ON strata when n < ", eff_cutoff))+
    inverse_theme()+
    theme(legend.title = element_blank())

ggsave(paste0("figures/strata correction/abundance - Walleye YOY eff_cutoff =", eff_cutoff, ".png"), 
       width = 13.5, height = 6.5, units = "in",dpi = 600)

#yellow perch
abundance_combo %>%
  filter(spp_agegroup == "331_1") %>%
  ggplot(aes(x = year, y = abundance_per_ha, fill = type))+
  geom_col(position = position_dodge(), col = "white")+
  scale_x_continuous(expand = c(0,.5), 
                     breaks = seq(1988,this_year,by=3))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("#68affc", "#ff864a"))+
  labs(x="", y="Fish/ha", title = paste0("Yellow perch/ha; eff_cutoff = ", eff_cutoff),
       caption = paste0("Pooling shallow & deep ON strata when n < ", eff_cutoff))+
  inverse_theme()+
  theme(legend.title = element_blank())

ggsave(paste0("figures/strata correction/abundance - Perch YOY eff_cutoff =", eff_cutoff, ".png"), 
       width = 13.5, height = 6.5, units = "in",dpi = 600)

#total forage
abundance_combo %>%
  filter(spp_agegroup == "all_forage") %>%
  ggplot(aes(x = year, y = abundance_per_ha, fill = type))+
  geom_col(position = position_dodge(), col = "white")+
  scale_x_continuous(expand = c(0,.5), 
                     breaks = seq(1988,this_year,by=3))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("#68affc", "#ff864a"))+
  labs(x="", y="Fish/ha", title = paste0("Total forage n/ha; eff_cutoff = ", eff_cutoff),
       caption = paste0("Pooling shallow & deep ON strata when n < ", eff_cutoff))+
  inverse_theme()+
  theme(legend.title = element_blank(),
        text = element_text(color = "white"))

ggsave(paste0("figures/strata correction/abundance - Total forage YOY eff_cutoff =", eff_cutoff, ".png"), 
       width = 13.5, height = 6.5, units = "in",dpi = 600)

