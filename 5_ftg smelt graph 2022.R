source("5_ftg whole lake trends.R")

# Graph - combined age-0 smelt indices for 2022 LEC Meeting social media

hydro_data <- read_csv("data sources/2022 hydro smelt data.csv")

colors <- c("#68affc", "#ff864a", "#fffa66", "white") #"new_colors" from whole lake trends
line_types = c(1, 2, 1, 95)

start_year = 2010

# reformat existing data
forage %>%
  filter(species == "Rainbow Smelt" & age_class == "YOY" & basin != "WB" & year > start_year-1 & !is.na(abundance_per_ha)) %>%
  full_join(hydro_data) %>%
  mutate(survey = case_when(agency == "OH" ~ "OH trawl",
                            agency == "ON" ~ "ON trawl",
                            agency == "Hydroacoustics" ~ "Hydroacoustics",
                            agency == "NY" ~ "NY trawl")) %>% #filter(year == 2021)
  ggplot(aes(x = year, y = abundance_per_ha, fill = survey))+
  geom_bar(stat = "identity", color = "white")+
  scale_x_continuous(expand = c(0,0), breaks = c(2010, 2013, 2016, 2019, 2021), limits = c(2009.5, this_year+0.5))+
  scale_y_continuous(expand = c(0,0), n.breaks = 4)+
  scale_fill_manual("Survey:", values = colors)+
  labs(y="Fish per hectare", 
       title = "Large 2021 Rainbow Smelt year-class in Lake Erie",
       subtitle = "Lake Erie Committee, Forage Task Group",
       caption = "Sources: NYDEC, OMNDMNRF, ODNR trawls; USGS, ODNR, OMNDMNRF hydroacoustics survey.")+
  facet_wrap(. ~ survey, ncol = 1, scales = "free_y")+
  inverse_theme()+
  theme(axis.title.x = element_blank(),
      plot.title = element_text(color = "white", size = 18, face = "bold", hjust = 0, vjust = .7),
      plot.subtitle = element_text(color = "white", size = 12, vjust = 0),
      plot.caption = element_text(color = "grey70", face = "italic"),
      panel.border = element_rect(fill = NA, color = "white"),
      strip.text = element_blank(),
      axis.text.y = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.key.width = unit(2, "lines"),
      panel.spacing = unit(0.8, "lines"),
      legend.position = "top",
      plot.caption.position = "panel",
      legend.justification = "left")

ggsave("figures/whole lake/2021 age-0 smelt catches.png", 
       width = 10, height = 6.5, units = "in", dpi = 600)
