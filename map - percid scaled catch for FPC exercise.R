source("spatial_1 - make simple features.R")
source("2_abundance summary.R")

### Calc mean scaled percid catches by grids   ####################
#filter percid catches
percid_catch <- catch_summary_by_site %>%
  filter(species %in% c(331, 334) & agecode == 1) %>%
  filter(year > 2019) %>% #, !(yrsam %in% c("ON2021253", "ON2018253")))# %>% # remove huge outlier catch of WAE in 2021 (hypoxia-related)
  left_join(all_effort %>% select(yrsam, grid))

percid_catch %>% filter(species == 334) %>% 
  select(yrsam, sum_catch, fish_per_area, grid) %>%
  arrange(-fish_per_area) %>% view

# scale percid catches and transform to sf
percid_catch <- percid_catch %>% 
  group_by(species, agecode) %>%
  mutate(fish_cpe_scaled = percent_rank(fish_per_area)*100) %>% # scale perch and walleye to std scale (mean = 0, sd = 1)
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


ggplot(data = percid_catch, aes(x = fish_cpe_scaled))+
  geom_density(fill = "orangered")+
  labs(x = "Scaled percid CPHA", y = "Density")+
  theme_classic()
  
ggsave(filename = "figures/maps/scaled catches/density - percentiles.png",
       width=8, height=7, units="in", dpi=400)

#join w/ two min grid
two_min_wb <- two_min_grid %>%
  janitor::clean_names() %>%
  st_crop(wb_bound_box) %>%
  st_intersection(percid_catch)

# calc mean catch by species and grid
scaled_catch_by_spp_grid <- two_min_wb %>%
  st_drop_geometry() %>%
  group_by(grid, species) %>%
  summarize(mean_cpha_scaled = mean(fish_cpe_scaled),
            median_cpha_scaled = median(fish_cpe_scaled)) %>%
  mutate(species = as.character(species))
  

# calc mean catch by grid (species combined) and combine with spp-specific
scaled_catch_by_grid  <- two_min_wb %>%
  st_drop_geometry() %>%
  group_by(grid) %>% 
  summarize(mean_cpha_scaled = mean(fish_cpe_scaled),
            median_cpha_scaled = median(fish_cpe_scaled)) %>%
  mutate(species = "combined") %>%
  rbind(scaled_catch_by_spp_grid)

# join back to two_min_grid
grid_catch <- two_min_grid %>%
  janitor::clean_names() %>%
  select(grid) %>%
  st_crop(wb_bound_box) %>%
  left_join(scaled_catch_by_grid) %>%
  arrange(grid, species)

#mapview(grid_catch)

grid_catch %>%
  filter(!is.na(species), species == 331) %>%
  arrange(-mean_cpha_scaled)

### Now make some maps   ##############################################

species_to_map = c("331", "334", "combined")
common_to_map = c("Perch", "Walleye", "Perch & Walleye")
# MEAN catches scaled
for(i in 1:length(species_to_map)){
  ggplot(data = grid_catch %>% filter(species == species_to_map[i]))+
    scale_x_continuous(limits = c(wb_bounds["xmin"], wb_bounds["xmax"]))+
    scale_y_continuous(limits = c(wb_bounds["ymin"], wb_bounds["ymax"]))+
    geom_sf(data = us_and_can, fill = "lightyellow2")+
    geom_sf(data = lake_erie, fill = "#c5dfed")+
    geom_sf(aes(fill = mean_cpha_scaled), color = "black", alpha = .85)+
    geom_sf(data = state_border, lty = 3)+
    geom_sf_text(aes(label = grid), color = "white", size = 3, fontface = "bold")+
    scale_fill_viridis_c(name = paste("Scaled mean\n", common_to_map[i], "per ha"))+
    annotation_north_arrow(which_north = "true",
                           location = "tl")+
    annotation_scale(unit_category = "metric", #add scale after red OH line
                     location = "br")+
    labs(title = "Method: Percentile rank",
         subtitle = "Data 2020-2025")+
    theme_minimal()+
    theme(axis.text = element_blank(), axis.title = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          legend.position = "inside",
          legend.position.inside = c(.15, .15))
  
  ggsave(filename = paste0("figures/maps/scaled catches/scaled MEAN catch of ", common_to_map[i], " map.png"),
         width=8, height=7, units="in", dpi=400)
}

# MEDIAN catches scaled
for(i in 1:length(species_to_map)){
  ggplot(data = grid_catch %>% filter(species == species_to_map[i]))+
    scale_x_continuous(limits = c(wb_bounds["xmin"], wb_bounds["xmax"]))+
    scale_y_continuous(limits = c(wb_bounds["ymin"], wb_bounds["ymax"]))+
    geom_sf(data = us_and_can, fill = "lightyellow2")+
    geom_sf(data = lake_erie, fill = "#c5dfed")+
    geom_sf(aes(fill = median_cpha_scaled), color = "black", alpha = .85)+
    geom_sf(data = state_border, lty = 3)+
    geom_sf_text(aes(label = grid), color = "white", size = 3, fontface = "bold")+
    scale_fill_viridis_c(name = paste("Scaled median\n", common_to_map[i], "per ha"))+
    annotation_north_arrow(which_north = "true",
                           location = "tl")+
    annotation_scale(unit_category = "metric", #add scale after red OH line
                     location = "br")+
    labs(title = "Method: Percentile rank",
         subtitle = "Data 2020-2025")+
    theme_minimal()+
    theme(axis.text = element_blank(), axis.title = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          legend.position = "inside",
          legend.position.inside = c(.15, .15))
  
  ggsave(filename = paste0("figures/maps/scaled catches/scaled MEDIAN catch of ", common_to_map[i], " map.png"),
         width=8, height=7, units="in", dpi=400)
}

