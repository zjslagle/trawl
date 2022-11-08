source("spatial_1 - make simple features.R")


library(nngeo)

### Identify nearest neighbor sites to find hypoxia-related fish concentrations    #################
# Following Kraus, R.T., Knight, C.T., Farmer, T.M., Gorman, A.M., Collingsworth, P.D., Warren, G.J., 
# Kocovsky, P.M., Conroy, J.D., 2015. Dynamic hypoxic zones in Lake Erie compress fish habitat, 
# altering vulnerability to fishing gears. Can. J. Fish. Aquat. Sci. 72, 797–806. 
# https://doi.org/10.1139/cjfas-2014-0517

# Look at months+depths with most hypoxia
effort_points %>%
  mutate(hypoxic = botoxy < 2,
         depth_rounded = round(depth, 0)) %>%
  xtabs(~depth_rounded+hypoxic, data = .)

# filter for those months/depths
sf_hypoxic <- effort_points %>% 
  filter(!is.na(botoxy), depth > 1, year > 1987, effstat %in% 0:1) %>%
  select(yrsam, agency, year, botoxy) %>%
  arrange(yrsam)

#separate dataframe to input nearest neighbors - otherwise there will be an error in the loop output below
data_hypoxic <- st_drop_geometry(sf_hypoxic)


### Loop over each year/month, identify nearest neighbor (and distance), output to data_hypoxic
#follows https://gis.stackexchange.com/questions/437968/r-finding-closest-point-to-each-point-and-filtering-which-points-to-consider
# iterate over individual trawls, filter by year before `st_nn`
for (i in 1:dim(data_hypoxic)[1]) {
  
  trawl <- sf_hypoxic[i, ]
  
  # which neighbors to consider? 
  n_trawls <- sf_hypoxic[-i, ] %>%
    dplyr::filter(year == trawl[["year"]] & # same year
                    yrsam != trawl[["yrsam"]] & # not the site itself
                    agency == trawl[["agency"]]) # same agency
  
  # skip iteration if there are no neighbours
  if (dim(n_trawls)[1] == 0) {
    
    next
    
  } else {
    
    # perform nearest neighbor search for simple features
    result <- st_nn(trawl, n_trawls, k = 1, returnDist = TRUE)
  }
  
  data_hypoxic[i, "n1_name"] <- n_trawls$yrsam[result[["nn"]][[1]][1]]
  data_hypoxic[i, "n1_dist"] <- result[["dist"]][[1]][1] # returns dist in METERS
  
}

# rename columns and join to get neighbor DO
neighbor_data <- data_hypoxic %>%
  rename("neighbor_crn" = n1_name,
         "neighbor_dist" = n1_dist) %>%
  left_join(data_hypoxic %>% select(yrsam, botoxy),
            by = c("neighbor_crn" = "yrsam")) %>%
  rename("neighbor_botoxy" = botoxy.y) %>%
  select(-year, -botoxy.x)

# join to effort and determine sites that need rule applied
effort_hypoxia <- all_effort %>%
  left_join(neighbor_data) %>% 
  mutate(do_diff  = botoxy - neighbor_botoxy,
         apply_do_rule = if_else(botoxy > 2 & neighbor_botoxy < 2 & neighbor_dist < 5600, TRUE, FALSE))
