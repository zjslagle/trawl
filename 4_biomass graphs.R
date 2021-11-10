source("3_biomass summary.R")


### Fig 2.4.3 Biomass Graph    #####################
cols = c("clupeid" = "red", "spiny_ray" = "yellow", "soft_ray" = "deepskyblue")

biomass_summary_groups %>%
  ggplot(aes(x = year, y = total_biomass_mt, group = group, fill = group))+
  geom_area(position = "stack", color = "black")+
  theme(axis.title = element_text(size=14, face = "bold"), axis.text.y=element_text(size=14, color = "black"), 
        axis.text.x=element_text(size=14, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
        panel.background = element_blank(), text = element_text(family = "Times"), legend.position = "top",
        legend.text = element_text(size = 12))+
  scale_x_continuous(expand = c(0,0), breaks = seq(1988,this_year,by=5))+
  scale_y_continuous(expand = c(0,0))+
  labs(x="", y="Total biomass (metric tonnes)")+
  scale_fill_manual(name = "", values = cols, 
                    labels = c("Clupeid", "Spiny-rayed", "Soft-rayed"))-> biomass_by_group

biomass_by_group

ggsave(plot = biomass_by_group, "figures/fig 2_4_3 biomass by group.png", 
       width = 7, height = 4, units = "in",dpi = 600)


### Paneled biomass graph - log scale to look at trends over years

biomass_summary_groups %>%
  ggplot(aes(x = year, y = total_biomass_mt, group = group, fill = group))+
  geom_area(position = "stack", color = "black")+
  theme(axis.title = element_text(size=14, face = "bold"), axis.text.y=element_text(size=14, color = "black"), 
        axis.text.x=element_text(size=14, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
        panel.background = element_blank(), text = element_text(family = "Times"), legend.position = "top",
        legend.text = element_text(size = 12)) +
  facet_grid(group ~ ., scales = "free_y")+
  scale_x_continuous(expand = c(0,0), breaks = seq(1988,this_year,by=5))+
  scale_y_continuous(expand = c(0,0))+
  labs(x="", y="Total biomass (metric tonnes)",
       caption = "Y-axes are not equal.")+
  scale_fill_manual(name = "", values = cols, labels = c("Clupeid", "Soft-rayed", "Spiny-rayed"))

ggsave("figures/biomass by group faceted.png", 
       width = 7, height = 5, units = "in",dpi = 600)
