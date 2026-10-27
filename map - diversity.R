### Maps for FTG Report
source("spatial_1 - make simple features.R")
source("2_abundance summary.R")

### get species richness and abundance, make shapefile for them   ###########################
catch_summary_by_site %>%
  filter(sum_catch > 0) %>% view

catch_rich_abund <- catch_summary_by_site %>%
  filter(sum_catch > 0)%>%
  group_by(yrsam) %>%
  summarize(richness = length(unique((species))),
            abundance = sum(fish_per_area)) 


#make a simple feature for sites
site_rich_abund <- all_effort %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  left_join(catch_rich_abund) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


### This_year effort Map   #############################################

ggplot(data = site_rich_abund)+
  scale_x_continuous(limits = c(wb_bounds["xmin"], wb_bounds["xmax"]))+
  scale_y_continuous(limits = c(wb_bounds["ymin"], wb_bounds["ymax"]))+
  geom_sf(data = us_and_can, fill = "white")+
  geom_sf(data = lake_erie, fill = "#c5dfed")+
  geom_sf(aes(fill = richness, size = abundance), color = "black", shape = 21)+
  geom_sf(data = state_border, lty = 3)+
  scale_fill_viridis_c(limits = c(3, 12), oob = scales::squish)+
  scale_size(range = c(2, 15))+
  labs(fill = "Richness", size = "Fish\nper ha")+
  annotation_north_arrow(which_north = "true",
                         location = "bl")+
  annotation_scale(unit_category = "metric", #add scale after red OH line
                   location = "br")+
  theme_minimal()+
  theme(axis.text = element_blank(), axis.title = element_blank(),
        plot.background = element_rect(fill = "white", color = NA))

ggsave(filename = paste0("figures/maps/richness and cpe map.png"),
       width=9, height=7, units="in", dpi=400, pointsize=12)

#### Interactive map   #########################
library(mapview); library(RColorBrewer); library(leafem); library(leaflet)

# interactive map of trawl sites on two minute grid

all_effort %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  mutate(do_level = ifelse(botoxy < 2, "low_do", "ok_do"),
         effstat = ifelse(botoxy < 2, 1, effstat), #need to keep Low DO sites, so set effstat==1 so we can filter out the rest
         longitude = ifelse(longitude > 0, -1 * longitude, longitude),
         site = as.numeric(str_sub(crn, -2, -1))) %>% #fixes error in trawl database, longitude should be negative
  filter(year == this_year) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  select(crn, trldate, botoxy, botoxy2, effstat, comments) -> trawl_sites

# trawl_sites %>% filter(site == 37) -> trawl_sites_filtered

mapview(trawl_sites, 
        #zcol = "botoxy", layer.name = "DO", at = c(0,2,4,10),
        zcol = "crn")
