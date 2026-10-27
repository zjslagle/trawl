source("2_abundance summary.R")

# sample size over years 
all_effort %>%
  filter(year > 1987, year < 2025, effstat == 1) %>%
  group_by(year) %>%
  tally() %>% ungroup %>%
  summarize(min = min(n),
            max = max(n),
            mean = mean(n))

### Figure 4   - plot goby catch and darter catches
yearly_abundance_tall_all_spp %>%
  filter(species %in% c(366, 343, 341, 342), year < 2025) %>%
  mutate(group = if_else(species %in% c(341, 343), "Johnny and channel", common_name)) %>%
  group_by(year, group) %>%
  summarize(abundance_per_ha = sum(abundance_per_ha)) %>%
  ggplot(aes(x = year, y = abundance_per_ha))+
  geom_hline(yintercept = 0)+
  geom_line()+
  geom_point(size = 2, shape = 21, color = "black", fill = "orangered")+
  facet_grid(group ~ ., scales = "free_y")+
  labs(y = "Abundance (n/ha)")+
  scale_x_continuous(breaks = seq(1990, 2020, by = 5))+
  scale_y_continuous(expand = c(0.01, 0))+
  theme_classic()+
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 12, color = "black"),
        axis.title.x = element_blank())

ggsave("figures/abundance/Fig 4 - abundance of goby+datrters.png", width = 6, height = 6, dpi = 500)
