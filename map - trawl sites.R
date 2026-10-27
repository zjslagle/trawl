### Maps for FTG Report
source("spatial_1 - make simple features.R")

### Count this year's trawl sites, make shapefile for them   ###########################
#count trawl sites, count # of low DO sites
effort_summary <- all_effort %>%
  filter(year == this_year) %>%  # filter(is.na(botoxy < 2)) %>% select(crn, botoxy)
  summarize(trawl_sites = n(),
            good_trawl_sites = sum(effstat == 1),
            bad_trawl_sites = sum(effstat != 1),
            low_do_sites = sum(effstat == 0),
            missing_do_sites = sum(is.na(botoxy)),
            hung_etc_sites = bad_trawl_sites - low_do_sites)

effort_summary

#make a simple feature for sites
trawl_sites <- all_effort %>%
  filter(year == this_year) %>%   #effstat will filter out the low DO sites
  select(agency, crn, latitude, longitude, botoxy, effstat) %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  mutate(status = case_when(botoxy < 2|effstat == 0 ~ "low_do",
                            effstat > 1 ~ "bad_trawl", 
                            effstat == 1 ~ "ok_do"),
         longitude = ifelse(longitude > 0, -1 * longitude, longitude),
         site = as.numeric(str_sub(crn, -2, -1))) %>% #fixes error in trawl database, longitude should be negative
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


bad_sites <- filter(trawl_sites, status == "bad_trawl")
low_do_sites <- filter(trawl_sites, status == "low_do")

### This_year effort Map   #############################################

ggplot(data = trawl_sites)+
  scale_x_continuous(limits = c(wb_bounds["xmin"], wb_bounds["xmax"]))+
  scale_y_continuous(limits = c(wb_bounds["ymin"], wb_bounds["ymax"]))+
  geom_sf(data = us_and_can, fill = "lightyellow2")+
  geom_sf(data = lake_erie, fill = "#c5dfed")+
  geom_sf(aes(fill = status), color = "black", shape = 21, size = 2)+
  geom_sf(data = state_border, lty = 3)+
  scale_fill_manual(name = "Effort status",
                      values = c("dodgerblue", "orangered", "black"),
                      breaks = c("low_do", "bad_trawl", "ok_do"),
                    labels = c("Hypoxic", "Bad", "Good"))+
  annotation_north_arrow(which_north = "true",
                         location = "bl")+
  annotation_scale(unit_category = "metric", #add scale after red OH line
                   location = "br")+
  theme_minimal()+
  theme(axis.text = element_blank(), 
        axis.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(.1, .25),
        legend.background = element_rect(fill = "white", color = "black"),
        plot.background = element_rect(fill = "white", color = NA),
        text = element_text(family = "Times New Roman"))

ggsave(filename = paste0("figures/maps/", this_year, " effort map.png"),
       width=8, height=7, units="in", dpi=400, pointsize=12)

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
