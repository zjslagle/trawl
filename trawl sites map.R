### Summaries and graphs for FTG Report

# mapping libraries
library(sf)
library(tmap)

source("Read Trawl Data.R")

#read in shapefiles
states = st_read("C:/Users/slaglez/Documents/Maps/ne_10m_admin_1_states_provinces.shp")
states %>% filter(gu_a3 == "CAN"|gu_a3 == "USA"|gu_a3 == "MEX") -> us_and_can #filter out US and CAN only
lake_erie = st_read("C:/Users/slaglez/Documents/Maps/Lake_Erie_Shoreline.shp") #not sure where this came from but it's detailed - maybe G Steinhart's files
feature_names = st_read("C:/Users/slaglez/Documents/Maps/ne_10m_populated_places.shp")
feature_names %>% filter(NAME %in% c("Toledo", "Detroit")) -> feature_names #label for Toledo, must subset so BG doesn't show up
urban_areas = st_read("C:/Users/slaglez/Documents/Maps/ne_10m_urban_areas.shp") #cities

#bounding box
wb_bounds = c(xmin = -83.5935, ymin = 41.3498, xmax = -82.4193, ymax = 42.1235)
big_bounds = c(xmin = -83.7591, ymin = 41.166, xmax = -78.4966, ymax = 43.0727)%>%
  st_bbox() %>% st_as_sfc() %>% st_set_crs(4326)

urban_areas <- st_intersection(urban_areas, big_bounds)

### Count this year's trawl sites, make shapefile for them   ###########################
#count trawl sites, count # of low DO sites
all_effort %>%
  filter(year == this_year) %>%
  summarize(trawl_sites = n(),
            good_trawl_sites = sum(effstat == 1),
            low_do_sites = sum(botoxy < 2))

#make a simple feature for sites
all_effort %>%
  filter(year == this_year) %>%   #effstat will filter out the low DO sites
  select(agency, crn, latitude, longitude, botoxy, effstat) %>%
  mutate(do_level = ifelse(botoxy < 2, "low_do", "ok_do"),
         effstat = ifelse(botoxy < 2, 1, effstat), #need to keep Low DO sites, so set effstat==1 so we can filter out the rest
         longitude = ifelse(agency == "OH", -1 * longitude, longitude)) %>% #fixes error in trawl database, longitude should be negative
  filter(effstat == 1) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) -> trawl_sites


low_do_sites <- filter(trawl_sites, do_level == "low_do")

### Map it!   #############################################
tm_shape(us_and_can, bbox = wb_bounds)+tm_fill()+ #plot land
  tm_shape(urban_areas)+tm_fill(col = "wheat3")+ #plot cities
  tm_shape(lake_erie)+tm_borders()+tm_fill(col="#ADD8E6")+ #plot lake, fill with blue
  tm_shape(feature_names)+tm_text("NAME", size = .8, fontface = "bold", bg.color = "wheat3", bg.alpha = 1, xmod = 1)+
  tm_shape(us_and_can)+tm_borders(col = "black", lty = 4)+ #redraw borders
  tm_shape(trawl_sites)+tm_bubbles(scale = .5, col = "black", border.col = "black")+
  tm_shape(low_do_sites)+tm_bubbles(scale = .5, col = "firebrick1", border.col = "black")+
  tm_scale_bar(breaks = c(0, 15, 30), position = c("left", "bottom"), size = 1)+
  tm_add_legend(type = "symbol", labels = c("Low DO", "Normoxic"), col = c("firebrick1", "black"), alpha = 1)+
  tm_layout(legend.position = c(.05,.14), legend.frame = "black", legend.bg.color = "white", fontfamily = "Times")

tmap_save(filename = paste(here("figures", "trawl sites map.png")), width = 6, height = 5, units = "in")
