source("spatial_2 - nearest neighbor.R")

### Abundance Summary, Adjusted with hypoxic nearest neighbor    ##################################
# Following Kraus, R.T., Knight, C.T., Farmer, T.M., Gorman, A.M., Collingsworth, P.D., Warren, G.J., 
# Kocovsky, P.M., Conroy, J.D., 2015. Dynamic hypoxic zones in Lake Erie compress fish habitat, 
# altering vulnerability to fishing gears. Can. J. Fish. Aquat. Sci. 72, 797–806. 
# https://doi.org/10.1139/cjfas-2014-0517
#
# Implement hypoxia adjustments to the abundance summary

### Create Catch Summary   #############################################################
# sum over CRN (only summarizes ~10 rows), join effort, filter out bad trawls
draft_catch_summary <- all_catch %>%
  group_by(year, agency, yrsam, crn, species, agecode) %>% 
  summarize(sum_catch = sum(catch)) %>% 
  ungroup() %>% 
  left_join(effort_hypoxia %>% 
              select(year, agency, yrsam, effdist, effstat)) %>%
  arrange(year, yrsam) %>% 
  filter(effstat == 1, year != 1987, !is.na(effdist)) #last part filters out one site in '93

#filter only spp-age groups that we care about, sum, then add explicit zero catches
catch_summary <- draft_catch_summary %>%
  mutate(spp_agegroup = paste0(species, "_", agecode)) %>%
  group_by(year, agency, yrsam, species, agecode, spp_agegroup) %>%
  summarize(sum_catch = sum(sum_catch)) %>%
  ungroup() %>%
  complete(nesting(year, agency, yrsam), 
           nesting(species, agecode, spp_agegroup), 
           fill = list(sum_catch = 0))

#join in effort stats
catch_summary <- catch_summary %>%
  left_join(effort_hypoxia %>% 
              select(year, agency, yrsam, depth, effdist, longitude, botoxy, neighbor_dist, neighbor_botoxy, apply_do_rule, do_diff)) %>%
  arrange(year, yrsam)


### plots !    ##########################################
summary(effort_hypoxia$do_diff)

#how far apart are neighbors
effort_hypoxia %>%
  ggplot(aes(x = neighbor_dist))+
  geom_density(fill = "black")

# view the greatest catches
catch_summary %>%
  filter(sum_catch > 10000) %>%
  select(year, yrsam, spp_agegroup, sum_catch, botoxy, neighbor_botoxy, apply_do_rule, do_diff) %>%
  view

# plot - which catches are we excluding? 
catch_summary %>%
  filter(!is.na(do_diff)) %>%
  ggplot(aes(x = do_diff,
             y = sum_catch,
             color = apply_do_rule))+
  geom_point(size = 2)+
  scale_color_manual(values = c("black", "red"),
                     breaks = c(F, T),
                     labels = c("Not adjusted", "Adjusted"))+
  labs(title = "Will the catch be adjusted?",
       subtitle = "Application of Kraus et al. (2016) to the interagency trawl",
       y = "Total catch (by species and age class)",
       x = "Dissolved oxygen difference (site - nearest neighbor, mg/L)",
       color = paste0("Adjust\ncatch?"))+
  theme_classic()

ggsave("figures/hypoxia adjusted/catch by do difference.png",
       width = 7, height = 6, units = "in")

# how are neighbor dist, do diff, and catches related? nothing apparent
catch_summary %>%
  filter(!is.na(do_diff)) %>%
  ggplot(aes(x = neighbor_dist, y = do_diff, size = sum_catch))+
  geom_point()
