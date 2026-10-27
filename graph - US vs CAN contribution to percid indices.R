source("2_abundance summary.R")

# Graph relative contribution to IA indices by US/CAN     ##################

### Percid indices      ###########################################
# get index values by state
index_by_state <- yearly_abundance_by_agency %>%
  left_join(spp_codes) %>%
  group_by(year, spp_agegroup, common_name, species, agecode, state) %>%
  summarize(total_abundance = sum(total_abundance),
            abundance_per_ha = round(total_abundance/375320,2)) %>%
  mutate(state = if_else(state == "CA", "ON", "OH"))

# GRAPH - abundance split by country
index_by_state %>%
  filter(agecode == 1, species %in% c(331, 334)) %>%
  ggplot(aes(x = year, y = abundance_per_ha, fill = state))+
  geom_col(color = "black", linewidth = .3)+
  facet_grid(common_name ~ ., scales = "free_y")+
  labs(y = "Abundance (n/ha)", fill = "State/Province")+
  scale_fill_brewer(palette = "Set3")+
  scale_x_continuous(expand = c(0,.5), breaks = seq(1988,this_year,by=5))+
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal()+
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10, color = "black"),
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "grey50", fill = NA))

ggsave("figures/country contribution/percid index contribution - abundance per ha.png", 
       width = 7, height = 4, units = "in", dpi = 600)

# Calc percentage by country
percentage <- index_by_state %>%
  left_join(yearly_abundance_tall %>% 
              rename("lakewide_abundance" = abundance_per_ha)) %>%
  mutate(percent_contribution = round(abundance_per_ha/lakewide_abundance, 3)) %>%
  ungroup

# GRAPH - percentage by country
percentage %>%
  filter(agecode == 1, species %in% c(331, 334)) %>%
  ggplot(aes(x = year, y = percent_contribution, fill = state))+
  geom_col(color = "black", linewidth = .3)+
  facet_grid(common_name ~ ., scales = "free_y")+
  labs(y = "Percent contribution to index", fill = "State/Province")+
  scale_fill_brewer(palette = "Set3")+
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal()+
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10, color = "black"),
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.border = element_rect(color = "grey50", fill = NA))

ggsave("figures/country contribution/percid index contribution - percentage.png", 
       width = 7, height = 4, units = "in", dpi = 600)


# GRAPH - percentage trend of US contribution to catches
percentage %>%
  filter(agecode == 1, species %in% c(331, 334)) %>% 
  filter(state == "OH") %>%
  ggplot(aes(x = year, y = percent_contribution))+
  geom_point(color = "black")+
  geom_smooth()+
  facet_grid(common_name ~ ., scales = "free_y")+
  labs(y = "Percent contribution to index",
       title = "Trends in US Contribution to Interagency Trawl Index")+
  scale_fill_brewer(palette = "Set3")+
  scale_x_continuous(expand = c(0,.5), breaks = seq(1988,this_year,by=5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,1))+
  theme_minimal()+
  theme(strip.text = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10, color = "black"),
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.border = element_rect(color = "grey50", fill = NA),
        panel.spacing = unit(5, "mm"))

ggsave("figures/country contribution/trend in percid index contribution.png", 
       width = 7, height = 4, units = "in", dpi = 600)


### Other fishes    ###########################################################
# loop over common species, make US contribution graphs
spp_to_graph <- percentage %>% 
  left_join(forage_groups) %>% 
  filter(!is.na(group), abundance_per_ha > 1) %>% 
  select(spp_agegroup) %>% unique %>% as_vector

for(i in 1:length(spp_to_graph)){
  common <- unique(percentage$common_name[which(percentage$spp_agegroup == spp_to_graph[i])])
  age <- unique(percentage$ages[which(percentage$spp_agegroup == spp_to_graph[i])])
  
  graph <- percentage %>%
    filter(state == "OH", spp_agegroup == spp_to_graph[i]) %>%
    ggplot(aes(x = year, y = percent_contribution))+
    geom_point(color = "black")+
    geom_smooth()+
    labs(y = "Percent contribution to index",
         title = paste0(age, " ", common, " - Ohio Contribution to Interagency Trawl Index"))+
    scale_fill_brewer(palette = "Set3")+
    scale_x_continuous(expand = c(0,.5), breaks = seq(1988,this_year,by=5))+
    scale_y_continuous(expand = c(0, 0), limits = c(0,1))+
    theme_minimal()+
    theme(strip.text = element_text(size = 12, face = "bold"),
          axis.text = element_text(size = 10, color = "black"),
          axis.title.x = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_rect(color = "grey50", fill = NA),
          panel.spacing = unit(5, "mm"))

ggsave(plot = graph,
       filename = paste("figures/country contribution/other spp/trend in", 
              spp_to_graph[i], 
              "index contribution.png"), 
       width = 7, height = 4, units = "in", dpi = 600)
}

