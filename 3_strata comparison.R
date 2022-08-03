source("2_abundance summary.R")

# 2_9_22 - Look at Interagency trawl numbers per strata - ON doesn't always sample shallow

effort_table <- all_effort %>%
  filter(!is.na(depth), year > 1987) %>%
  mutate(depth_strata = if_else(depth > 6, "deep", "shallow")) %>%
  group_by(year, agency, depth_strata) %>%
    summarize(n_trawls = sum(effstat == 1, na.rm = T)) %>% 
  ungroup %>%
  pivot_wider(names_from = c(agency, depth_strata), values_from = n_trawls, values_fill = 0) %>%
  mutate(n_trawls = rowSums(select(., c(OH_deep, OH_shallow, ON_deep, ON_shallow))))

# summarize ON Shallow trawl sample numbers
effort_table  %>% 
  summarize(n_no_trawls = sum(ON_shallow == 0),
            n_equals_1 = sum(ON_shallow == 1),
            percent_no_trawls = n_no_trawls/n()*100,
            percent_1_trawl = n_equals_1/n()*100)

# 34 years of survey


#Interagency strata areas (hectares, I think)
on_shallow = 1.681E+8
on_deep = 1.244E+9
on_total = on_deep + on_shallow

oh_shallow = 9.281E+8
oh_deep = 1.413E+9
oh_total = oh_shallow + oh_deep

total = on_total + oh_total

# ON percent shallow
on_shallow/on_total * 100
oh_shallow/oh_total *100
