source("3_biomass summary.R")

### Fig 2.4.3 Biomass Graph    #####################
cols = c("clupeid" = "red", "spiny_ray" = "yellow", "soft_ray" = "deepskyblue")
panel_labels = c("clupeid" = "Clupeid", "spiny_ray" = "Spiny-rayed", "soft_ray" = "Soft-rayed")

biomass_by_group <- biomass_summary_groups %>%
  filter(group != "all_forage") %>%
  ggplot(aes(x = year, y = total_biomass_kg_ha, group = group, fill = group))+
  geom_col(position = "stack", color = "black")+
  scale_x_continuous(expand = c(0,0), breaks = seq(1988,this_year,by=5))+
  scale_y_continuous(expand = c(0,0))+
  labs(x="", y="Total biomass (kg/ha)")+
  scale_fill_manual(name = "", 
                    values = cols, 
                    labels = panel_labels)+
  theme(axis.title = element_text(size=14, face = "bold"), 
        axis.text.y=element_text(size=14, color = "black"), 
        axis.text.x=element_text(size=14, color = "black", vjust = -1), 
        axis.line = element_line(color = "black"), 
        panel.background = element_blank(), 
        text = element_text(family = "Times"),
        legend.position = "inside",
        legend.position.inside = c(.5, .9), 
        legend.direction = "horizontal",
        legend.text = element_text(size = 12),
        plot.margin = unit(c(.5,1,.5,.5), "cm"))

biomass_by_group

ggsave(plot = biomass_by_group, "figures/ftg report/fig 2_1_3_3 biomass by group.png", 
       width = 7, height = 4, units = "in",dpi = 600)


### Paneled biomass graph - log scale to look at trends over years

biomass_summary_groups %>%
  filter(group != "all_forage") %>%
  ggplot(aes(x = year, y = total_biomass_kg_ha, group = group, fill = group))+
  geom_col(position = "stack", color = "black")+
  theme(axis.title = element_text(size=14, face = "bold"), axis.text.y=element_text(size=14, color = "black"), 
        axis.text.x=element_text(size=14, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
        panel.background = element_blank(), text = element_text(family = "Times"), legend.position = "top",
        legend.text = element_text(size = 12)) +
  facet_grid(group ~ ., scales = "free_y", labeller = as_labeller(panel_labels))+
  scale_x_continuous(expand = c(0,0), breaks = seq(1988,this_year,by=5))+
  scale_y_continuous(expand = c(0,0))+
  labs(x="", y="Total biomass (kg/ha)",
       caption = "Y-axes are not equal.")+
  scale_fill_manual(name = "", values = cols, labels = panel_labels)+
  theme(strip.background = element_rect(fill = "white", color = "black"),
        strip.text = element_text(size = 12, face = "bold"))

ggsave("figures/biomass by group faceted.png", 
       width = 7, height = 5, units = "in",dpi = 600)

