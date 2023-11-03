source("spatial_1 - make simple features.R")
source("2_abundance summary.R")
### Maps of YOY YP and WAE catches for internal report     #############

species_to_map <- c(331, 334)

percid_sf <- catch_per_area %>%
  filter(year == this_year, agecode == 1, species %in% species_to_map) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# make the maps!
for(i in 1:length(species_to_map)){
  ggplot(data = percid_sf %>% filter(species == species_to_map[i]))+
    scale_x_continuous(limits = c(wb_bounds["xmin"], wb_bounds["xmax"]))+
    scale_y_continuous(limits = c(wb_bounds["ymin"], wb_bounds["ymax"]))+
    geom_sf(data = us_and_can, fill = "lightyellow2")+
    geom_sf(data = lake_erie, fill = "#c5dfed")+
    geom_sf(aes(size = fish_per_area*10000,  #convert to hectare
                fill = fish_per_area*10000), color = "black", shape = 21)+
    geom_sf(data = state_border, lty = 3)+
    scale_size(name = "Fish per hectare",
               range = c(.3, 6))+
    scale_fill_gradientn(name = "Fish per hectare",
                         colours = c('white', "#FEECE3", "orangered"),
                         values = c(0, .00001, 1))+
    guides(fill = guide_legend(), size = guide_legend())+ #combines legends
    ggsn::scalebar(dist = 15, model = "WGS84",location = "topright", dist_unit = "km", 
                   x.min = wb_bounds["xmin"], y.min = wb_bounds["ymin"], 
                   x.max = wb_bounds["xmax"], y.max = wb_bounds["ymax"],
                   st.dist = .03, anchor = c(x = -83.2, y = 41.4), transform = TRUE,
                   border.size = .5, st.size = 3)+
    ggsn::north(x.min = wb_bounds["xmin"], y.min = wb_bounds["ymin"], 
                x.max = wb_bounds["xmax"], y.max = wb_bounds["ymax"],
                location = "topright", symbol = 3, anchor = c(x = -83.33, y = 41.5))+
    theme_minimal()+
    theme(axis.text = element_blank(), axis.title = element_blank(),
          plot.background = element_rect(fill = "white", color = NA))
  
  ggsave(filename = paste0("figures/maps/percid indices/age-0 ", species_to_map[i], " map.png"),
         width=7, height=5, units="in", dpi=400, pointsize=12)
}
