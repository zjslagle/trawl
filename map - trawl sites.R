### Maps for FTG Report
source("spatial_1 - make simple features.R")

### Count this year's trawl sites, make shapefile for them   ###########################
#count trawl sites, count # of low DO sites
effort_summary <- all_effort %>%
  filter(year == this_year) %>%  # filter(is.na(botoxy < 2)) %>% select(crn, botoxy)
  summarize(trawl_sites = n(),
            good_trawl_sites = sum(effstat == 1),
            low_do_sites = sum(botoxy < 2, na.rm = T),
            missing_do_sites = sum(is.na(botoxy)))

effort_summary$trawl_sites

#make a simple feature for sites
trawl_sites <- all_effort %>%
  filter(year == this_year) %>%   #effstat will filter out the low DO sites
  select(agency, crn, latitude, longitude, botoxy, effstat) %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  mutate(do_level = ifelse(botoxy < 2, "low_do", "ok_do"),
         effstat = ifelse(botoxy < 2, 1, effstat), #need to keep Low DO sites, so set effstat==1 so we can filter out the rest
         longitude = ifelse(longitude > 0, -1 * longitude, longitude),
         site = as.numeric(str_sub(crn, -2, -1))) %>% #fixes error in trawl database, longitude should be negative
  filter(effstat == 1) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


low_do_sites <- filter(trawl_sites, do_level == "low_do")

### Static Tmap Map   #############################################
sites_tmap <- tm_shape(us_and_can, bbox = wb_bounds)+tm_fill()+ #plot land
  tm_shape(urban_areas)+tm_fill(col = "wheat3")+ #plot cities
  tm_shape(lake_erie)+tm_borders()+tm_fill(col="#ADD8E6")+ #plot lake, fill with blue
  tm_shape(feature_names)+tm_text("NAME", size = .8, fontface = "bold", bg.color = "wheat3", bg.alpha = 1, xmod = 1)+
  tm_shape(us_and_can)+tm_borders(col = "black", lty = 4)+ #redraw borders
  tm_shape(trawl_sites)+tm_bubbles(scale = .5, col = "black", border.col = "black")+
  tm_shape(low_do_sites)+tm_bubbles(scale = .5, col = "firebrick1", border.col = "black")+
  tm_scale_bar(breaks = c(0, 15, 30), position = c("left", "bottom"), text.size = 1)+
  tm_add_legend(type = "symbol", labels = c("Low DO", "Normoxic"), col = c("firebrick1", "black"), alpha = 1)+
  tm_layout(legend.position = c(.05,.14), legend.frame = "black", legend.bg.color = "white", fontfamily = "Times")

tmap_save(sites_tmap,
          filename = paste("figures/maps/trawl sites map ", this_year, ".png", sep = ""), 
          width = 5, height = 4, units = "in")


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


### Catch map   ##########################################
target_species = 301

all_catch %>%
  left_join(all_effort %>% select(yrsam, year, longitude, latitude)) %>%
  filter(species == target_species, year == 2021) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
