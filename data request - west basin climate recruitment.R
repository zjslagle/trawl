### WB Climate Synchrony USGS - percent catches etc. ############################
total_catch <- catch_summary %>%
  summarize(sum(sum_catch)) %>% as_vector()

# YOY summary
catch_summary %>%
  filter(agecode == 1) %>%
  group_by(species, agecode) %>%
  summarize(catch = sum(sum_catch),
            percent_catch = (catch/total_catch)) %>%
  write_csv("data derived/data requests/age-0 catch percentages.csv")

# all fish summary
catch_summary %>%
  group_by(species, agecode) %>%
  summarize(catch = sum(sum_catch),
            percent_catch = (catch/total_catch)) %>%
  write_csv("data derived/data requests/all ages catch percentages.csv")

# adult summary
catch_summary %>%
  filter(agecode %in% c(3,4,5)) %>%
  group_by(species, agecode) %>%
  summarize(catch = sum(sum_catch),
            percent_catch = (catch/total_catch)) %>%
  write_csv("data derived/data requests/adult catch percentages.csv")

### Deal w/ weirdo AC-5 fish    ############################################
# What are they, by agency?
all_catch %>%
  filter(agecode == 5) %>%
  left_join(spp_codes) %>%
  janitor::tabyl(common_name, agency)

# What are they, by agency?
all_catch %>%
  filter(agecode == 5) %>%
  left_join(spp_codes) %>%
  group_by(common_name) %>%
  summarize(n_catches = n(),
            total_catch = sum(catch)) %>% view

# most common
all_catch %>%
  filter(agecode == 5) %>%
  group_by(species) %>%
  tally() %>% filter(n > 10)

all_lengths %>%
  filter(species %in% c(180, 206)) %>%
  ggplot(aes(x = length, fill = agecode))+
  geom_bar()+
  facet_grid(species ~ .)

all_lengths %>%
  filter(species %in% c(180, 206, 196, 201),
         length < 200) %>%
  ggplot(aes(x = length, fill = agecode, group = agecode))+
  geom_bar()+
  facet_grid(species ~ .)

all_lengths %>%
  filter(species == 196, length < 150) %>%
  ggplot(aes(x = length, fill = agecode, group = agecode))+
  geom_bar()+
  geom_vline(aes(xintercept = 68))

all_lengths %>%
  filter(species == 366) %>%
  ggplot(aes(x = length, fill = agecode, group = agecode))+
  geom_bar()+
  geom_vline(aes(xintercept = 45))
