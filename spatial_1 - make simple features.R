

# mapping libraries
library(magrittr)
library(sf)
library(tmap)
library(mapview)
require(RODBC)
library(stringr)
library(tidyverse)
this_year = year(now())#-1 #get this year

### Get effort data  (duplicates 1_read data)  #############################################################
trawl_database <- odbcConnectAccess2007("data sources/ON23parse_OH23_ERIEWEST_TRAWL_IN_OH_FMT_Oct30_23.accdb")
all_effort_raw <- as_tibble(sqlFetch(trawl_database, "EFFORT"))
odbcCloseAll()

names(all_effort_raw) %<>% tolower

all_effort <- all_effort_raw %>%
  mutate(crn = as.character(crn),
         projcode = as.character(projcode))

all_effort <- all_effort %>% 
  mutate(projcode = case_when(projcode %in% c("LEA-IF00_001","LEA_IF00-001") ~ "LEA_IF00_001",
                              T ~ projcode),#fix mismatch between underscores and dashes)
         effdist = if_else(is.na(effdist), speed*(effdur/60), effdist),
         effstat = case_when(botoxy <= 2 & year %in% c(2009:2012) ~ 0, #This codes matches ON/Andy's summary no explanation as for why.
                             botoxy <= 2 & !is.na(botoxy) & year %in% c(2016:this_year) ~ 0, 
                             T ~ effstat))  #SAS note: TEMPORARY MEASURE TO HANDLE LOW DO 2009-2011 # LOL @ "temporary") 


#source("1_read data - 2020 format.R") # <- may need this

map_folder = "C:/Users/10173930/Documents/maps/shapefiles/"

#units are annoying
clean_units <- function(x){
  attr(x,"units") <- NULL
  class(x) <- setdiff(class(x),"units")
  x<- as.vector(x)
  x
}

#read in shapefiles
states = st_read(paste(map_folder, "ne_10m_admin_1_states_provinces.shp", sep = ""))
states %>% filter(gu_a3 == "CAN"|gu_a3 == "USA"|gu_a3 == "MEX") -> us_and_can #filter out US and CAN only
state_border = st_read(paste0(map_folder, "erieborder.shp")) %>% st_set_crs(4326)
lake_erie = st_read(paste(map_folder, "Lake_Erie_Shoreline.shp", sep = "")) #not sure where this came from but it's detailed - maybe G Steinhart's files
feature_names = st_read(paste(map_folder, "ne_10m_populated_places.shp", sep = ""))
feature_names %>% filter(NAME %in% c("Toledo", "Detroit")) -> feature_names #label for Toledo, must subset so BG doesn't show up
urban_areas = st_read(paste(map_folder, "ne_10m_urban_areas.shp", sep = "")) #cities
two_min_grid <- st_read(paste(map_folder, "2 minute grids lakewide.shp", sep = ""), crs = 4326)

#bounding box
wb_bounds = c(xmin = -83.5935, ymin = 41.3498, xmax = -82.4193, ymax = 42.1235)
big_bounds = c(xmin = -83.7591, ymin = 41.166, xmax = -78.4966, ymax = 43.0727)%>%
  st_bbox() %>% st_as_sfc() %>% st_set_crs(4326)

urban_areas <- st_intersection(urban_areas, big_bounds)

# make effort into shapefile
effort_points <- all_effort %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

