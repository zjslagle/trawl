source("2_abundance summary.R")

data_perch_regression <- yearly_abundance_tall %>%
  filter(species == 331) %>%
  mutate(age = if_else(agecode == 1, "zero", "one"),
         year = if_else(agecode == 2, year-1, year)) %>%
  select(year, age, abundance_per_ha) %>% 
  pivot_wider(names_from = age, names_prefix = "age_", values_from = abundance_per_ha) %>% 
  filter(year > 1987, year < this_year)

data_perch_regression %>%
  ggplot(aes(x = age_zero, y = age_one))+ #fill = factor(year)
    geom_smooth(method = "lm", color = "red")+
    geom_point(size = 2)+
    ggrepel::geom_text_repel(aes(label = year))+
    stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")), label.y = 225)+
    scale_fill_viridis_d(option = "inferno")+
  labs(title = "Survival from Age-0 to Age-1 Yellow Perch, MU1",
       subtitle = "Interagency trawl data, 1988-present", 
       x = "Age-0 abundance (cpha)", y = "Age-1 abundance (cpha)", fill = "Year")+
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white", color = "black"))

ggsave("figures/yptg/age-0 to age-1 survival by year.png", width = 9, height = 6, dpi = 400, units = "in")
