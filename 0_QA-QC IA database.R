# Use this script to check IA database once Ontario sends it.
source("1_read data - 2020 format.R")

this_year_projcode = "LEA_IF24_001" #chage to current year

crns <- all_effort %>%
  filter(year == this_year + 1, effstat == 1) %>%
  select(yrsam) %>% arrange

### CRN check   ###############
crns %>%
  anti_join(all_catch %>% filter(year == this_year+1))

crns %>%
  anti_join(all_lengths %>% filter(projcode == this_year_projcode))

all_lengths %>%
  filter(projcode == this_year_projcode) %>%
  select(crn) %>% unique %>% view


## Effort   #################################
all_effort %>%
  filter(year == this_year + 1) %>%
  filter(stemp < 11| stemp > 30|
           btemp < 11| btemp > 30|
           botoxy  < 0| botoxy  > 12.5|
           grid < 0| grid > 1000|
           speed < 1 | speed > 2|
           depth < 2 | depth > 14|
           effdur < 0| effdur > 11|
           statime < 500| statime > 1700|
           secchi < 0| secchi > 5) 

### Catch    ##################
all_catch %>% 
  filter(year == this_year + 1) %>%
  left_join(spp_codes) %>%
  filter(agecode  < 1| agecode  > 5| is.na(agecode)|
           !(species %in% spp_codes$species)|
           is.na(catch)| is.na(species))

# rare catches this year
all_catch %>%
  filter(year == this_year + 1) %>%
  left_join(spp_codes) %>%
  group_by(species, common_name, agecode) %>%
  summarize(total_catch = sum(catch)) %>%
  arrange(total_catch) %>%  head

## filter out crn/spp/age code combos that appear >1; likely errors (i.e., wrong crn)
all_catch %>% 
  filter(year == this_year + 1) %>%
  arrange(crn, species, agecode) %>%
  add_count(yrsam, species, agecode) %>%
  filter(n > 1)
# note, some weirdos might show up here, e.g. Logperch (as ON lumps these into AC5)

### Lengths   ##########################
### Filter out suspicious data
all_lengths %>% 
  filter(projcode == this_year_projcode) %>%
  filter(length < 10|
           is.na(species)|
           is.na(agecode)|
           agecode < 1| agecode > 5) %>%
  filter(species != 300)

all_lengths %>%
  filter(species == 180, year == 2024)

# any species in length but not catch data?
all_lengths %>%
  filter(projcode == this_year_projcode) %>%
  left_join(spp_codes) %>%
  select(species, common_name) %>%
  unique() %>%
  anti_join(all_catch)

### Filter out low occurrence species codes 
all_lengths %>%
  filter(projcode == this_year_projcode) %>%
  left_join(spp_codes) %>%
  group_by(species, common_name, agecode) %>%
  summarize(total_catch = n()) %>%
  arrange(total_catch) %>%  head

# plot lengths - note that ON measures many of their adults, so we filter out the adult categories
all_lengths %>%
  filter(projcode == this_year_projcode, agecode %in% 1:2) %>%
  mutate(cat = paste(species, agecode, sep = "_")) %>%
  ggplot(aes(x = length, y = cat, label = crn, group = cat))+
  geom_boxplot()+
  ggrepel::geom_text_repel()
