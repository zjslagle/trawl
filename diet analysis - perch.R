### Diet Analysis for Yellow Perch - USGS data for West Basin, summarized for the Forage Task Group
# Code started Mar 2020 (COVID-19 yay) by Z Slagle

# Comments from Kevin Keretz, USGS:
  # So FID is fish id and those are unique by season/yr.  So every cruise was started with a new set of fish id's. 
  # Serial is the predetermined site number where the fish was captured.  
  # 100's are spring sampling and 500's are fall sampling.   
  # Also if you looked through the data set there are some diet items we weren't able to get dry weights for.  
  # For these there is an average measurement that may be converted to estimated dry weight. 
  # The put the conversion factors that our lab uses in the second tab.  I think some of them were calculated 
  # in there but not all of them.


### Load libraries   ##################################
library(magrittr)
#library(sqldf)
library(forcats)
require(lubridate)
require(stringr)
require(here)
require(readxl)
library("stringr")
library(tidyverse)


this_year = year(now())-1 #get this year
windowsFonts(Times=windowsFont("Times New Roman"))




### Load data #######

diet_data <- read_excel(here("data", "WBRR YP diet data 2017-2019 USGS.xlsx"), sheet = "diet_data", 
                        na = c("", "NA"),
                        col_types = c(rep("numeric", 4), "text",rep("numeric", 4), "text", rep("numeric", 6), "text"))
prey_type <- read_excel(here("data", "WBRR YP diet data 2017-2019 USGS.xlsx"), sheet = "prey_type", 
                        na = c("", "NA"), col_types = c(rep("text", 4)))


# fix data issues
diet_data %<>%
  mutate(month = case_when(month == 6 ~ "June", #text month
                           month == 9 ~ "Sept",
                           T ~ "Other"),
         food_item = tolower(food_item),
         food_item = case_when(str_detect(food_item, "chironomid") ~ "chironomid", #group chironomid stages together
                               T ~ food_item)) %>%
  filter(species == "yellow perch") # get rid of walleye

prey_type %<>% select("food_item" = item, type) %>% #only use this for groupings of prey
  mutate(food_item = tolower(food_item))

### Summarize data    ###########################################

# n sites
diet_data %>%
  group_by(year, month) %>%
  summarize(site_n = n_distinct(serial))

# Sample size - samples/month, empties, non-empties
diet_data %>%
  mutate(empty = ifelse(food_item == "empty", 1,0)) %>%
  group_by(year, month) %>%
  summarize(total_diets = n_distinct(fid),
            empty_stomachs = sum(empty),
            full_stomachs = total_diets - empty_stomachs) -> sample_sizes

sample_sizes

# now filter out empty stomachs
diet_data %<>% filter(food_item != "empty")

### Frequency of occurence    #############################################################
#frequency of occurence by food GROUP - make tall dataset
diet_data %>%
  filter(food_item != "Empty") %>%
  left_join(prey_type) %>% 
  group_by(year, month, type) %>%
  summarize(occurence = n_distinct(fid)) %>%
  ungroup %>%
  left_join(sample_sizes %>% select(year, month, full_stomachs)) %>%
  mutate(percent = 100*occurence/full_stomachs) %>%
  arrange(year, month, percent) -> percent_occurence_type

#quick summary of % occurence
  percent_occurence_type %>%
  filter(year == this_year) %>%
  select(month, type, percent) %>%
  spread(month, percent) %>%
  arrange(-Sept)


#frequency of occurence by food item - make tall dataset
diet_data %>%
  filter(food_item != "Empty") %>%
  group_by(year, month, food_item) %>%
  summarize(occurence = n_distinct(fid)) %>%
  ungroup %>%
  left_join(sample_sizes %>% select(year, month, full_stomachs)) %>%
  mutate(percent = 100*occurence/full_stomachs) %>%
  arrange(year, month, percent) -> percent_occurence_item

#quick summary of % occurence
percent_occurence_item %>%
  filter(year == this_year) %>%
  select(month, food_item, percent) %>%
  spread(month, percent) %>%
  arrange(-Sept)


 ### Graphs   - frequency of occurance   #################

# by prey group
percent_occurence_type %>%
  filter(year == this_year) %>%
  ggplot(aes(x = reorder(str_wrap(type, 20), -percent), y = percent))+
    geom_col(fill = "navyblue")+
    facet_grid(month ~ .)+
    labs(x = "", y = "Freq. of occurence (%)")+
    theme_minimal()+
    scale_y_continuous(expand = c(0,0), limits = c(0,100))+
    theme(panel.grid = element_blank(), text = element_text(size = 12, face = "bold", family = "Times"), 
          panel.border = element_rect(color = "black", fill = NA), panel.spacing = unit(1, "lines"))

ggsave(here("figures", "frequency by group - yp diet.png"), width = 6, height = 4.5, units = "in")

# by specific prey - FILTERED FOR IN 5+% of diets!
percent_occurence_item %>%
  filter(year == this_year, percent > 4.9) %>%
  ggplot(aes(x = reorder(str_wrap(food_item, 20), -percent), y = percent))+
  geom_col(fill = "navyblue")+
  facet_grid(month ~ .)+
  labs(x = "", y = "Freq. of occurence (%)")+
  theme_minimal()+
  scale_y_continuous(expand = c(0,0), limits = c(0,100))+
  theme(panel.grid = element_blank(), text = element_text(size = 12, face = "bold", family = "Times"), 
        panel.border = element_rect(color = "black", fill = NA), axis.text.x = element_text(angle = 90, vjust = -.3),
        panel.spacing = unit(1, "lines"))

ggsave(here("figures", "frequency by item - yp diet.png"), width = 6, height = 4.5, units = "in")




### Summary - dry weights    #############################
# get total weights
diet_data %>%
  group_by(year, month) %>%
  summarize(total_weight = sum(dw_mg, na.rm = T)*1000) -> total_by_group

#dry weight by food GROUP - make tall dataset
diet_data %>%
  filter(food_item != "Empty") %>%
  left_join(prey_type) %>% 
  group_by(year, month, type) %>%
  summarize(dry_weight = sum(dw_mg, na.rm = T)*1000) %>%
  ungroup %>%
  left_join(total_by_group) %>%
  mutate(percent = 100*dry_weight/total_weight) %>%
  arrange(year, month, percent) -> percent_wt_type

#quick summary of % by dry weight
percent_wt_type %>%
  filter(year == this_year) %>%
  select(month, type, percent) %>%
  spread(month, percent) %>%
  arrange(-Sept)


# dry wt by food item - make tall dataset
diet_data %>%
  filter(food_item != "Empty") %>%
  group_by(year, month, food_item) %>%
  summarize(dry_weight = sum(dw_mg, na.rm = T)*1000) %>%
  ungroup %>%
  left_join(total_by_group) %>%
  mutate(percent = 100*dry_weight/total_weight) %>%
  arrange(year, month, percent) -> percent_wt_item

#quick summary of dry wt
percent_wt_item %>%
  filter(year == this_year) %>%
  select(month, food_item, percent) %>%
  spread(month, percent) %>%
  arrange(-Sept)

### Graphs   - dry weights   #################

# by prey group
percent_wt_type %>%
  filter(year == this_year) %>%
  ggplot(aes(x = reorder(str_wrap(type, 15), -percent), y = percent))+
  geom_col(fill = "navyblue")+
  facet_grid(month ~ .)+
  labs(x = "", y = "Dry weight (% of total)")+
  theme_minimal()+
  scale_y_continuous(expand = c(0,0), limits = c(0,100))+
  theme(panel.border = element_rect(color = "black", fill = NA), 
        panel.grid = element_blank(), text = element_text(size = 12, face = "bold", family = "Times"), panel.spacing = unit(1, "lines"))

ggsave(here("figures", "dry weight by group - yp diet.png"), width = 6, height = 4.5, units = "in")

# by specific prey - FILTERED FOR IN 5+% of diets!
percent_wt_item %>%
  filter(year == this_year, percent > 4.9) %>%
  ggplot(aes(x = reorder(str_wrap(food_item, 20), -percent), y = percent))+
  geom_col(fill = "navyblue")+
  facet_grid(month ~ .)+
  labs(x = "", y = "Dry weight (% of total)")+
  theme_minimal()+
  scale_y_continuous(expand = c(0,0), limits = c(0,100))+
  theme(panel.grid = element_blank(), text = element_text(size = 12, color = "black", face = "bold", family = "Times"), 
        panel.border = element_rect(color = "black", fill = NA), axis.text.x = element_text(angle = 90, vjust = -.3), 
        panel.spacing = unit(1, "lines"))

ggsave(here("figures", "dry weight by item - yp diet.png"), width = 6, height = 4.5, units = "in")
