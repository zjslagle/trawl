source("7a_hypoxia and catch prep.R")

# to fix - these sites are retrawls or something; same location but diff day
effort_hypoxia %>% filter(neighbor_dist == 0) %>% view

#sample sizes
effort_hypoxia %>%
  summarize(total_n = n(),
            no_do = sum(is.na(botoxy)|is.na(neighbor_botoxy)),
            no_neigh_within = sum(neighbor_dist > 5600, na.rm = T),
            cant_use = sum(is.na(botoxy)|is.na(neighbor_botoxy)|neighbor_dist > 5600),
            good_sample = total_n - cant_use)

### data prep   ###################
target_species = c("301_1", "331_1", "334_1")

# join Nearest Neighbor data, filter only sites with DO data and NN
cpha_hypoxia <- cpha_by_site %>%
  left_join(effort_hypoxia) %>%
  filter(spp_agegroup %in% target_species,
         neighbor_dist < 5600,
         !is.na(botoxy),
         !is.na(neighbor_botoxy)) %>%
  select(year, yrsam, state, spp_agegroup, species, agecode, cpha, botoxy, neighbor_botoxy) %>%
  mutate(do_category_trawl = as_factor(if_else(botoxy > 2, "Normoxic", "Hypoxic")),
         do_category_nn = as_factor(if_else(neighbor_botoxy > 2, "Normoxic", "Hypoxic")), # as_factor preserves order
         log_cpha = log(cpha+.0000000001))

# sum over these species
cpha_combo <- cpha_hypoxia %>%
  group_by(year, yrsam, state, do_category_trawl, do_category_nn) %>%
  summarize(cpha = sum(cpha),
            log_cpha = log(cpha+.000001))


# plot it!
cpha_combo %>%
  ggplot(aes(x = do_category_nn, y = cpha, fill = do_category_nn))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = .3)+
  facet_wrap(. ~ do_category_trawl)+
  scale_y_log10()+
  scale_fill_manual(breaks = c("Normoxic", "Hypoxic"),
                    values = c("white", "lightskyblue"))+
  labs(x = "Nearest Neighbor", y = "Total catch per ha", title = "Trawl Status")+
  theme_minimal()+
  theme(strip.background = element_rect(color = "grey70"),
        plot.title = element_text(hjust = .5),
        axis.title = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        panel.background = element_rect(color = "grey70"),
        legend.position = "none")

ggsave("figures/hypoxia adjusted/boxplot - nearest neighbor comparison.png",
       width = 8, height = 6, units = "in", dpi = 500)

table(cpha_combo$do_category_trawl, cpha_combo$do_category_nn)

# Powerpoint plot
cpha_combo %>%
  ggplot(aes(x = do_category_nn, y = cpha, fill = do_category_nn))+
  geom_boxplot(outlier.shape = NA, color = "white", linewidth = 1)+
  geom_jitter(width = .3, color = "white", alpha = .5)+
  facet_wrap(. ~ do_category_trawl)+
  scale_y_log10()+
  scale_fill_manual(breaks = c("Normoxic", "Hypoxic"),
                    values = c("green3", "lightskyblue"))+
  labs(x = "Nearest Neighbor", y = "Total catch per ha", title = "Trawl Status",
       fill = "")+
  inverse_theme()+
  theme(plot.title = element_text(color = "white", face = "bold", size = 16),
        legend.position = "none")

ggsave("figures/hypoxia adjusted/boxplot - nearest neighbor comparison POWERPOINT.png",
       width = 13.5, height = 6, units = "in", dpi = 500)
