source("3_biomass summary.R")

require(cowplot) #for combo plots


### Combined abundance and biomass plots by species  - Age-0 fishes   ##########################################

species_vec = c(063, 196, 301, 331, 334)

for(i in 1:length(species_vec)){
  target_species = species_vec[i]
  
  abund_graph <- yearly_abundance_tall %>%
    filter(species == target_species, agecode == 1) %>%
    ggplot(aes(x = year, y = abundance_per_ha))+
    geom_col(color = "black")+
    scale_x_continuous(expand = c(0,.5), breaks = seq(1988,this_year,by=5))+
    scale_y_continuous(expand = c(0,0))+
    labs(x="", y="Abundance (fish/ha)")+
    theme(axis.title = element_text(size=12, face = "bold"), axis.text.y=element_text(size=12, color = "black"), 
          axis.text.x=element_text(size=14, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
          panel.background = element_blank(), text = element_text(family = "Times"),
          plot.margin = unit(c(3,1,1,1), "mm"))
  
  biomass_graph <- biomass_summary_all_spp %>%
    filter(species == target_species, agecode == 1) %>%
    ggplot(aes(x = year, y = total_biomass_kg_ha))+
    geom_col(color = "black")+
    theme(axis.title = element_text(size=12, face = "bold"), axis.text.y=element_text(size=12, color = "black"), 
          axis.text.x=element_text(size=14, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
          panel.background = element_blank(), text = element_text(family = "Times"), legend.position = "top",
          strip.background = element_rect(fill = "white", color = "black"), strip.text = element_text(face = "bold", size = 12)) +
    scale_x_continuous(expand = c(0,.5), breaks = seq(1988,this_year,by=5))+
    scale_y_continuous(expand = c(0,0))+
    labs(x="", y="Biomass (kg/ha)", 
         caption = paste("Age-0", spp_codes$common_name[which(spp_codes$species == target_species)]))
  
  combo_plot = plot_grid(abund_graph, biomass_graph, align = "v", ncol = 1)
  
  ggsave(filename = paste0("figures/combo abund + biomass/combo abund biomass - ", target_species, " Age-0.png"),
         plot = combo_plot,
         width = 7, height = 4, units = "in", dpi = 600)
}


### Same combo plots, but for ADULT fishes   ##########################################

species_vec = c(196, 291)

for(i in 1:length(species_vec)){
  target_species = species_vec[i]
  
  abund_graph <- yearly_abundance_tall %>%
    filter(species == target_species, agecode == 3) %>%
    ggplot(aes(x = year, y = abundance_per_ha))+
    geom_col(color = "black")+
    scale_x_continuous(expand = c(0,.5), breaks = seq(1988,this_year,by=5))+
    scale_y_continuous(expand = c(0,0))+
    labs(x="", y="Abundance (fish/ha)")+
    theme(axis.title = element_text(size=12, face = "bold"), axis.text.y=element_text(size=12, color = "black"), 
          axis.text.x=element_text(size=14, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
          panel.background = element_blank(), text = element_text(family = "Times"),
          plot.margin = unit(c(3,1,1,1), "mm"))
  
  biomass_graph <- biomass_summary_all_spp %>%
    filter(species == target_species, agecode == 3) %>%
    ggplot(aes(x = year, y = total_biomass_kg_ha))+
    geom_col(color = "black")+
    theme(axis.title = element_text(size=12, face = "bold"), axis.text.y=element_text(size=12, color = "black"), 
          axis.text.x=element_text(size=14, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
          panel.background = element_blank(), text = element_text(family = "Times"), legend.position = "top",
          strip.background = element_rect(fill = "white", color = "black"), strip.text = element_text(face = "bold", size = 12)) +
    scale_x_continuous(expand = c(0,.5), breaks = seq(1988,this_year,by=5))+
    scale_y_continuous(expand = c(0,0))+
    labs(x="", y="Biomass (kg/ha)", 
         caption = paste("Age-1+", spp_codes$common_name[which(spp_codes$species == target_species)]))
  
  combo_plot = plot_grid(abund_graph, biomass_graph, align = "v", ncol = 1)
  
  ggsave(filename = paste0("figures/combo abund + biomass/combo abund biomass - ", target_species, " Age-1 plus.png"),
         plot = combo_plot,
         width = 7, height = 4, units = "in", dpi = 600)
}

# Finally, round goby (since they are AC == 5)
abund_graph <- yearly_abundance_tall %>%
  filter(species == 366, agecode == 5) %>%
  ggplot(aes(x = year, y = abundance_per_ha))+
  geom_col(color = "black")+
  scale_x_continuous(expand = c(0,.5), breaks = seq(1988,this_year,by=5))+
  scale_y_continuous(expand = c(0,0))+
  labs(x="", y="Abundance (fish/ha)")+
  theme(axis.title = element_text(size=12, face = "bold"), axis.text.y=element_text(size=12, color = "black"), 
        axis.text.x=element_text(size=14, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
        panel.background = element_blank(), text = element_text(family = "Times"),
        plot.margin = unit(c(3,1,1,1), "mm"))

biomass_graph <- biomass_summary_all_spp %>%
  filter(species == 366, agecode == 5) %>%
  ggplot(aes(x = year, y = total_biomass_kg_ha))+
  geom_col(color = "black")+
  theme(axis.title = element_text(size=12, face = "bold"), axis.text.y=element_text(size=12, color = "black"), 
        axis.text.x=element_text(size=14, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
        panel.background = element_blank(), text = element_text(family = "Times"), legend.position = "top",
        strip.background = element_rect(fill = "white", color = "black"), strip.text = element_text(face = "bold", size = 12))+
  scale_x_continuous(expand = c(0,.5), breaks = seq(1988,this_year,by=5))+
  scale_y_continuous(expand = c(0,0))+
  labs(x="", y="Biomass (kg/ha)", 
       caption = "Round Goby (all ages)")

combo_plot = plot_grid(abund_graph, biomass_graph, align = "v", ncol = 1)

ggsave(filename = paste0("figures/combo abund + biomass/combo abund biomass - Round Goby all ages.png"),
     plot = combo_plot,
     width = 7, height = 4, units = "in", dpi = 600)


### Other species - insert desired species code and ageclass   ###############################
target_spp = 201
target_ac = 3
target_common = spp_codes$common_name[which(spp_codes$species == target_spp)]
target_age_text = agecodes$ages[which(agecodes$agecode == target_ac)]

abund_graph <- yearly_abundance_tall %>%
  filter(species == target_spp, agecode == target_ac) %>%
  ggplot(aes(x = year, y = abundance_per_ha))+
  geom_col(color = "black")+
  scale_x_continuous(expand = c(0,.5), breaks = seq(1988,this_year,by=5))+
  scale_y_continuous(expand = c(0,0))+
  labs(x="", y="Abundance (fish/ha)")+
  theme(axis.title = element_text(size=12, face = "bold"), axis.text.y=element_text(size=12, color = "black"), 
        axis.text.x=element_text(size=14, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
        panel.background = element_blank(), text = element_text(family = "Times"),
        plot.margin = unit(c(3,1,1,1), "mm"))

biomass_graph <- biomass_summary_all_spp %>%
  filter(species == target_spp, agecode == target_ac) %>%
  ggplot(aes(x = year, y = total_biomass_kg_ha))+
  geom_col(color = "black")+
  theme(axis.title = element_text(size=12, face = "bold"), axis.text.y=element_text(size=12, color = "black"), 
        axis.text.x=element_text(size=14, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
        panel.background = element_blank(), text = element_text(family = "Times"), legend.position = "top",
        strip.background = element_rect(fill = "white", color = "black"), strip.text = element_text(face = "bold", size = 12))+
  scale_x_continuous(expand = c(0,.5), breaks = seq(1988,this_year,by=5))+
  scale_y_continuous(expand = c(0,0))+
  labs(x="", y="Biomass (kg/ha)", 
       caption = paste(target_age_text, target_common))

combo_plot = plot_grid(abund_graph, biomass_graph, align = "v", ncol = 1)

ggsave(filename = paste("figures/combo abund + biomass/combo abund biomass -", target_age_text, target_common, ".png"),
       plot = combo_plot,
       width = 7, height = 4, units = "in", dpi = 600)
