source("Read Trawl Data.R")

biomass <- read_excel(path = "C:/Users/zakjs/Desktop/Work Files/Forage TG/2020/Data/biomassall.xlsx")

# clean 'r up
biomass %>% 
  janitor::clean_names() %>%
  select(-total) %>%
  gather("spp_group", "biomass", 2:4) -> biomass

### Fig 2.4.3 Biomass Graph    #####################
cols = c("clupeid" = "red", "spiny_ray" = "yellow", "soft_ray" = "deepskyblue")

biomass %>%
  ggplot(aes(x = year, y = biomass, group = spp_group, fill = spp_group))+
  geom_area(position = "stack", color = "black")+
  theme(axis.title = element_text(size=14, face = "bold"), axis.text.y=element_text(size=14, color = "black"), 
        axis.text.x=element_text(size=14, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
        panel.background = element_blank(), text = element_text(family = "Times"), legend.position = "top",
        legend.text = element_text(size = 12)) +
  scale_x_continuous(expand = c(0,0), breaks = seq(1988,this_year,by=5))+
  scale_y_continuous(expand = c(0,0))+
  labs(x="", y="Fish/ha")+
  scale_fill_manual(name = "", values = cols, labels = c("Clupeid", "Soft-rayed", "Spiny-rayed"))-> biomass_by_group

biomass_by_group

ggsave(plot = biomass_by_group, here("figures", "fig 2_4_3 biomass by group.png"), 
       width = 7, height = 5, units = "in",dpi = 600)


### Paneled biomass graph - log scale to look at trends over years

biomass %>%
  ggplot(aes(x = year, y = biomass, group = spp_group, fill = spp_group))+
  geom_area(position = "stack", color = "black")+
  theme(axis.title = element_text(size=14, face = "bold"), axis.text.y=element_text(size=14, color = "black"), 
        axis.text.x=element_text(size=14, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
        panel.background = element_blank(), text = element_text(family = "Times"), legend.position = "top",
        legend.text = element_text(size = 12)) +
  facet_grid(spp_group ~ .)+
  scale_x_continuous(expand = c(0,0), breaks = seq(1988,this_year,by=5))+
  scale_y_log10(expand = c(0,0))+
  labs(x="", y="Fish/ha")+
  scale_fill_manual(name = "", values = cols, labels = c("Clupeid", "Soft-rayed", "Spiny-rayed"))

