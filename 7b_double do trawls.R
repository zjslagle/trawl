source("7a_hypoxia and catch prep.R")

# Look at DO levels at start and end of trawl - can we predict fish concentrations with these data?

#sample size - how many trawls even have botoxy2?
effort_hypoxia %>%
  summarize(total_n = n(),
            cant_use = sum(is.na(botoxy)|is.na(botoxy2)),
            good_sample = total_n - cant_use)

table(effort_hypoxia$year, is.na(effort_hypoxia$botoxy2))         

### data prep   ###################
target_species = c("301_1", "331_1", "334_1")

# join Nearest Neighbor data, filter only sites with DO data and NN
cpha_hypoxia <- cpha_by_site %>%
  left_join(effort_hypoxia) %>%
  filter(spp_agegroup %in% target_species,
         !is.na(botoxy),
         !is.na(botoxy2)) %>%
  select(year, yrsam, state, spp_agegroup, species, agecode, cpha, botoxy, botoxy2) %>%
  mutate(do_cat_start = as_factor(if_else(botoxy > 2, "Normoxic", "Hypoxic")),
         do_cat_end = as_factor(if_else(botoxy2 > 2, "Normoxic", "Hypoxic")), # as_factor preserves order
         log_cpha = log(cpha+.0000000001),
         do_diff = abs(botoxy-botoxy2)) #absolute value of diff)

# sum over these species
cpha_combo <- cpha_hypoxia %>%
  group_by(year, yrsam, state, do_cat_start, do_cat_end, do_diff) %>%
  summarize(cpha = sum(cpha),
            log_cpha = log(cpha+.000001))

table(cpha_combo$do_cat_start, cpha_combo$do_cat_end)


# simple plot - distribution of catches and hypoxic diffs
cpha_combo %>%
  ggplot(aes(x = do_cat_start, y = cpha, fill = do_cat_start))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = .3)+
  facet_wrap(. ~ do_cat_end)+
  scale_y_log10()+
  scale_fill_manual(breaks = c("Normoxic", "Hypoxic"),
                    values = c("white", "lightskyblue"))+
  labs(x = "DO at start", y = "Total catch per ha", title = "DO at end")+
  theme_minimal()+
  theme(strip.background = element_rect(color = "grey70"),
        plot.title = element_text(hjust = .5),
        axis.title = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        panel.background = element_rect(color = "grey70"),
        legend.position = "none")

ggsave("figures/hypoxia adjusted/boxplot - do at start-end comparison.png",
       width = 8, height = 6, units = "in", dpi = 500)

### DO difference across two trawls   ######
cpha_hypoxia %>%
  ggplot(aes(x = do_diff, y = cpha))+
    geom_smooth()+
    geom_point()+
    scale_y_log10()+
    labs(x = "DO difference (mg/L)", y = "Total catch per ha")+
    theme_minimal()+
    theme(plot.title = element_text(hjust = .5),
          axis.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 12),
          panel.background = element_rect(color = "grey70"))

ggsave("figures/hypoxia adjusted/points - do at start-end difference.png",
       width = 8, height = 6, units = "in", dpi = 500)
