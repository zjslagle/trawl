source("Read Trawl Data.R")

biomass <- read_excel(path = "SAS vs R Debug/biomassall 2019.xls")

# clean 'r up
biomass %>% 
  janitor::clean_names() %>%
  select(-catchtot) %>%
  gather("spp_group", "biomass", 2:4) -> biomass

### Fig 2.4.3 Biomass Graph    #####################
cols_old = c("clupeid" = "red", "spinyray" = "yellow", "softray" = "deepskyblue")

biomass %>%
  ggplot(aes(x = year, y = biomass, group = spp_group, fill = spp_group))+
  geom_area(position = "stack", color = "black")+
  theme(axis.title = element_text(size=14, face = "bold"), axis.text.y=element_text(size=14, color = "black"), 
        axis.text.x=element_text(size=14, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
        panel.background = element_blank(), text = element_text(family = "Times"), legend.position = "top",
        legend.text = element_text(size = 12)) +
  scale_x_continuous(expand = c(0,0), breaks = seq(1988,this_year,by=5))+
  scale_y_continuous(expand = c(0,0))+
  labs(x="", y="Biomass (metric tonnes)")+
  scale_fill_manual(name = "", values = cols_old, 
                    labels = c("Clupeid", "Spiny-rayed", "Soft-rayed"))



ggsave("figures/fig 2_4_3 biomass by group OLD ESTIMATES.png", 
       width = 7, height = 4, units = "in",dpi = 600)


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

