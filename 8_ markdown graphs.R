
source("2_abundance summary.R")

# Figures for R Markdown Report

# loop over each group, make graph
groups_to_graph <- yearly_abundance_tall %>% select(spp_agegroup) %>% unique %>% as_vector()

for(i in 1:length(groups_to_graph)){
  yearly_abundance_tall %>%
    filter(spp_agegroup == groups_to_graph[i]) %>%
    ggplot(aes(x = year, y = abundance_per_ha))+
    geom_col(fill = "forestgreen", col = "black")+
    theme(axis.title = element_text(size=14), 
          axis.text.y=element_text(size=14, color = "black"), 
          axis.text.x=element_text(size=14, vjust=1, color = "black"), 
          axis.line = element_line(color = "black"), 
          panel.background = element_blank(),
          text = element_text(family = "Times New Roman")) +
    scale_x_continuous(expand = c(0,.5), breaks = seq(1988,this_year,by=5))+
    scale_y_continuous(expand = c(0,0))+
    labs(x="", y="Fish/ha")
  
  ggsave(paste0("figures/markdown/markdown abundance ", groups_to_graph[i], ".png"), 
         width = 7, height = 3.5, units = "in",dpi = 600)

}


#### Abundance by functional group   ##########################
cols = c("clupeid" = "red", "spiny_ray" = "yellow", "soft_ray" = "deepskyblue")

density_by_group <- yearly_abundance_tall %>%
  filter(spp_agegroup %in% c("clupeid", "spiny_ray", "soft_ray")) %>%
  ggplot(aes(x = year, y = abundance_per_ha, group = spp_agegroup, fill = spp_agegroup))+
  geom_col(position = "stack", color = "black")+
  theme(axis.title = element_text(size=12), 
        axis.text.y=element_text(size=14, color = "black"), 
        axis.text.x=element_text(size=14, vjust=1, color = "black"), 
        axis.line = element_line(color = "black"), 
        panel.background = element_blank(),
        text = element_text(family = "Times New Roman"), 
        legend.position = "top",
        legend.text = element_text(size = 12)) +
  scale_x_continuous(expand = c(0,0), breaks = seq(1988,this_year,by=5))+
  scale_y_continuous(expand = c(0,0))+
  labs(x="", y="Fish/ha")+
  scale_fill_manual(name = "", 
                    values = cols, 
                    breaks = c("clupeid", "spiny_ray", "soft_ray"),
                    labels = c("Clupeid", "Spiny-rayed", "Soft-rayed"))

density_by_group

ggsave(plot = density_by_group, "figures/markdown/markdown abundance by forage group.png", 
       width = 7, height = 4, units = "in",dpi = 600)
