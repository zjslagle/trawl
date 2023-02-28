source("7a_hypoxia and catch prep.R")

### plots !    ##########################################

summary(effort_hypoxia$do_diff)

#how far apart are neighbors
effort_hypoxia %>%
  ggplot(aes(x = neighbor_dist))+
  geom_density(fill = "black")+
  geom_vline(xintercept = 5600, color = "grey80")

# view the largest catches
catch_summary %>%
  filter(sum_catch > 10000) %>%
  select(year, yrsam, spp_agegroup, sum_catch, botoxy, neighbor_botoxy, apply_do_rule, do_diff) 

# plot - which catches are we excluding? 
catch_summary %>%
  filter(!is.na(do_diff), sum_catch > 50) %>%
  ggplot(aes(x = do_diff,
             y = sum_catch,
             color = apply_do_rule))+
  geom_point(size = 2)+
  scale_color_manual(values = c("black", "red"),
                     breaks = c(F, T),
                     labels = c("Not adjusted", "Adjusted"))+
  labs(title = "Will the catch be adjusted?",
       subtitle = "Application of Kraus et al. (2015) to the interagency trawl",
       y = "Total catch (by species and age class)",
       x = "Dissolved oxygen difference (site - nearest neighbor, mg/L)",
       color = paste0("Adjust catch?"))+
  theme_classic()+
  theme(legend.position = c(.2, .9), legend.box.background =element_rect(color = "grey70"))

ggsave("figures/hypoxia adjusted/catch by do difference.png",
       width = 8, height = 6, units = "in")

# how are neighbor dist, do diff, and catches related? nothing apparent
catch_summary %>%
  filter(!is.na(do_diff)) %>%
  ggplot(aes(x = neighbor_dist, y = do_diff, size = sum_catch))+
  geom_point()
