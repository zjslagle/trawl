source("graph - ftg whole lake trends.R")

alewife <- read_csv("data sources/alewife index.csv")


colors <- c("#68affc", "#ff864a", "#fffa66", "white") #"new_colors" from whole lake trends
line_types = c(1, 2, 1, 95)

start_year = 2000

missing_data <- alewife %>% filter(is.na(abund), year > start_year-1)

### Bar graph - alewife catches ###################################
alewife %>%
  filter(year > start_year-1,
         agency_survey != "ODNR trawl") %>%
  ggplot(aes(x = year, y = abund, color = agency_survey, lty = agency_survey))+
  geom_line(lwd = 2)+
  geom_point(size = 4)+
  scale_x_continuous(expand = c(0,.5), breaks = seq(1988,this_year,by=3), limits = c(2000, this_year))+
  scale_y_continuous(expand = c(0,0))+
  scale_color_manual("Survey", values = colors)+
  scale_linetype_manual("Survey", values = line_types)+
  labs(x="", y="Fish/ha or Gillnet CPE", title = "Alewife Age-0")+
  inverse_theme()


ggsave("figures/whole lake/abundance - alewife.png", 
       width = width_print, height = height_print, units = "in",dpi = 600)

### Faceted graph - alewife catch rank within survey ###################################
alewife %>%
  group_by(agency_survey) %>%
  mutate(rank = round(percent_rank(abund)*100,0)) %>% #rank within survey
  ungroup() %>%
  filter(year > start_year-1,
         agency_survey != "ODNR trawl") %>%
  ggplot(aes(x = year, y = rank, fill = agency_survey))+
  geom_bar(stat = "identity", color = "white")+
  geom_text(data = missing_data,
            aes(x = year, y = 10), label = "*",
            color = "white", size = 5)+
  scale_x_continuous(expand = c(0,0), 
                     breaks = seq(start_year, this_year, by = 2), 
                     limits = c(start_year-.5, this_year+0.5))+
  scale_y_continuous(expand = c(0,0), n.breaks = 4)+
  scale_fill_manual("Source:", values = colors)+
  labs(y = "Survey catch percentile rank")+
       # title = "Large 2023 Alewife Abundance in Lake Erie",
       # subtitle = "Lake Erie Committee, Forage Task Group")+
  facet_wrap(. ~ agency_survey, ncol = 1, scales = "free_y")+
  inverse_theme()+
  theme(axis.title.x = element_blank(),
        plot.title = element_text(color = "white", size = 18, face = "bold", hjust = 0, vjust = .7),
        plot.subtitle = element_text(color = "white", size = 12, vjust = 0),
        plot.caption = element_text(color = "grey70", face = "italic"),
        panel.border = element_rect(fill = NA, color = "white"),
        strip.text = element_blank(),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.key.width = unit(2, "lines"),
        panel.spacing = unit(0.8, "lines"),
        legend.position = "top",
        plot.caption.position = "panel",
        legend.justification = "left")

ggsave("figures/whole lake/2023 alewife catches - percentile.png", 
       width = 13.3333, height = 6.5, units = "in", dpi = 600)

