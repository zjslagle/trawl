### Forage Summary of whole-lake trends     ######################################
# For Forage Task Group LEC presentation
# plot summaries of important forage fishes for 4 main trawl surveys:
# WB/Interagency, CB Ohio, EB NY, EB ON (Long Point Bay)
# Zak Slagle Mar 2020

source("graph - abundance.R")

# read data
forage <- read_excel("data sources/Forage Fish Trends_All Agencies.xlsx", sheet = "trawl_tall")

forage %<>% mutate(basin = case_when(agency == "ON" ~ "LPB", T ~ basin))  # convert "ON" to Long Point Bay (see trawl lack of correlation, below)

### Summary counts and Within Basin Comps     ###############################
#total counts of data by basin/agency
forage %>%
  group_by(basin, agency) %>%
  summarize(total_num = n())
# 
# # years of data by agency (if they're not close within basin, will throw off trends these graphs are 2000-present)
# forage %>%
#   filter(species == "Rainbow Smelt" & age_class == "YOY" & year > 1999) %>%
#   group_by(basin, agency) %>%
#   summarize(total_years = sum(!is.na((abundance_per_ha))))
# 
# # do EB trawls correlate? NOPE
# eb_data_comp <- forage %>%
#   filter(basin %in% c("EB", "LPB")) %>% 
#   mutate(abundance_per_ha = abundance_per_ha+.001) %>%
#   spread(key = agency, value = abundance_per_ha) %>%
#   filter(!is.na(NY))
# 
# eb_data_comp %>%
#   ggplot(aes(x = NY, y = ON))+
#   stat_smooth(method = "lm", color = "red")+
#   geom_point(size = 2)+
#   facet_grid(species ~ age_class)+
#   theme_minimal()+
#   scale_x_log10()+
#   scale_y_log10()

#because of this, plot NY and ON LPB surveys separate 

# Trim data - remove MI (sorry Jan) and years before 2000
forage <- forage %>% filter(year > 1999, agency != "MI")

### PreLEC/LEC - West Basin Abundance by forage group  #####################


density_by_group+
  inverse_theme()+
  geom_area(position = "stack", color = "white")+
  theme(plot.background = element_rect(fill = "#003D73"),
        plot.margin = unit(c(.5,1,.5,.5), "cm"),
        axis.title = element_text(size=18, face = "bold"), 
        axis.text = element_text(size=16))

ggsave("figures/whole lake/abundance - WESTERN BASIN-IA by group.png", 
       width = 13.5, height = 6.5, units = "in",dpi = 600)

### Abundance Plots by Species  ######################

# colors for different basins - trying to fine tune so they are most visible/distinguishable 
# and don't look like shit
test_colors <- c("#51C1AB", "#FFD56B", "#6371C9", "#FFA56B")
new_colors <- c("#68affc", "#ff864a", "#fffa66", "white")
colors <- new_colors
line_types = c(1, 2, 1, 95) #line types across all line graphs
width_print = 13.3333 #fucking LEC making us put video in the bottom right corner where the trends are
height_print = 6.5 #5.1

scales::show_col(new_colors)

# Smelt YOY
smelt_yoy <- forage %>%
  filter(species == "Rainbow Smelt" & age_class == "YOY" & !is.na(abundance_per_ha)) %>%
  group_by(year, basin) %>%
  summarize(mean_abundance = mean(abundance_per_ha)) %>%
  ungroup %>%
  mutate(basin = fct_relevel(basin, "WB", "CB", "EB", "LPB")) %>%
  ggplot(aes(x = year, y = mean_abundance, color = basin, lty = basin))+
  geom_line(lwd = 2)+
  scale_x_continuous(expand = c(0,.5), breaks = seq(1988,this_year,by=3), limits = c(2000, this_year))+
  scale_y_continuous(expand = c(0,0))+
  scale_color_manual("Basin", values = colors)+
  scale_linetype_manual("Basin", values = line_types)+
  labs(x="", y="Fish/ha", title = "Rainbow Smelt YOY")+
  inverse_theme()


ggsave(plot = smelt_yoy, "figures/whole lake/abundance - smelt YOY.png", 
       width = width_print, height = height_print, units = "in",dpi = 600)


# Smelt adult
smelt_adult <- forage %>%
  filter(species == "Rainbow Smelt" & age_class == "YAO" & !is.na(abundance_per_ha)) %>%
  group_by(year, basin) %>%
  summarize(mean_abundance = mean(abundance_per_ha)) %>%
  ungroup %>%
  mutate(basin = fct_relevel(basin, "WB", "CB", "EB", "LPB")) %>%
  ggplot(aes(x = year, y = mean_abundance, color = basin, lty = basin))+
  scale_linetype_manual("Basin", values = line_types)+
  geom_line(lwd = 2)+
  scale_x_continuous(expand = c(0,.5), breaks = seq(1988,this_year,by=3), limits = c(2000, this_year))+
  scale_y_continuous(expand = c(0,0))+
  scale_color_manual("Basin", values = colors)+
  labs(x="", y="Fish/ha", title = "Rainbow Smelt YAO")+
  inverse_theme()

ggsave(plot = smelt_adult, "figures/whole lake/abundance - smelt adult.png", 
       width = width_print, height = height_print, units = "in",dpi = 600)


# Shiner TOTAL
shiner <- forage %>%
  filter(species == "Emerald Shiner" & !is.na(abundance_per_ha)) %>%
  group_by(year, basin) %>%
  summarize(total_abundance = sum(abundance_per_ha)) %>%
  ungroup %>%
  mutate(basin = fct_relevel(basin, "WB", "CB", "EB", "LPB")) %>%
  ggplot(aes(x = year, y = total_abundance, color = basin, lty = basin))+
  geom_line(lwd = 2)+
  scale_x_continuous(expand = c(0,.5), breaks = seq(1988,this_year,by=3), limits = c(2000, this_year))+
  scale_y_continuous(expand = c(0,0))+
  scale_color_manual("Basin", values = colors)+
  scale_linetype_manual("Basin", values = line_types)+
  labs(x="", y="Fish/ha", title = "Emerald Shiner All Ages")+
  inverse_theme()

ggsave(plot = shiner, "figures/whole lake/abundance - shiner ALL AGES.png", width = width_print, height = height_print, units = "in",dpi = 600)


# Shad YOY
shad_yoy <- forage %>%
  filter(species == "Gizzard Shad" & age_class == "YOY" & !is.na(abundance_per_ha)) %>%
  group_by(year, basin) %>%
  summarize(mean_abundance = mean(abundance_per_ha)) %>%
  ungroup %>%
  mutate(basin = fct_relevel(basin, "WB", "CB", "EB", "LPB")) %>%
  ggplot(aes(x = year, y = mean_abundance, color = basin, lty = basin))+
  scale_linetype_manual("Basin", values = line_types)+
  geom_line(lwd = 2)+
  scale_x_continuous(expand = c(0,.5), breaks = seq(1988,this_year,by=3), limits = c(2000, this_year))+
  scale_y_continuous(expand = c(0,0))+
  scale_color_manual("Basin", values = colors)+
  labs(x="", y="Fish/ha", title = "Gizzard Shad YOY")+
  inverse_theme()

ggsave(plot = shad_yoy, "figures/whole lake/abundance - shad YOY.png", width = width_print, height = height_print, units = "in",dpi = 600)

# Goby
goby_all <- forage %>%
  filter(species == "Round Goby" & !is.na(abundance_per_ha)) %>%
  group_by(year, basin) %>%
  summarize(mean_abundance = mean(abundance_per_ha)) %>%
  ungroup %>%
  mutate(basin = fct_relevel(basin, "WB", "CB", "EB", "LPB")) %>%
  ggplot(aes(x = year, y = mean_abundance, color = basin, lty = basin))+
  geom_line(lwd = 2)+
  scale_x_continuous(expand = c(0,.5), breaks = seq(1988,this_year,by=3), limits = c(2000, this_year))+
  scale_y_continuous(expand = c(0,0))+
  scale_color_manual("Basin", values = colors)+
  scale_linetype_manual("Basin", values = line_types)+
  labs(x="", y="Fish/ha", title = "Round Goby")+
  inverse_theme()

ggsave(plot = goby_all, "figures/whole lake/abundance - goby all.png", 
       width = width_print, height = height_print, units = "in",dpi = 600)



### Percentile Plots by species    #####################

#calc percentile rank within basin 
forage_ranked <- forage %>%
  filter(!is.na(abundance_per_ha)) %>%
  mutate(age_class = if_else(species == "Emerald Shiner", #group all ages of shiner together
                             "all ages",
                             age_class)) %>%
  group_by(year, basin, species, age_class) %>%
  summarize(mean_abundance = mean(abundance_per_ha)) %>%
  ungroup %>%
  group_by(basin, species, age_class) %>%
  mutate(rank = rank(mean_abundance, na.last = NA), 
         n = length(mean_abundance),
         percent_rank = round(rank(mean_abundance, na.last = NA)/length(mean_abundance) * 100,1), #percentile rank
         over_under_rank = percent_rank-50) %>%               # percentile rank relative to 50% (under = negative)
  arrange(basin, species, age_class, year) %>% 
  ungroup

# Smelt YOY
smelt_yoy_percent <- forage_ranked %>%
  filter(species == "Rainbow Smelt" & age_class == "YOY" & !is.na(mean_abundance)) %>%
  mutate(basin = fct_relevel(basin, "WB", "CB", "EB", "LPB")) %>%
  ggplot(aes(x = year, y = over_under_rank, fill = basin))+
  geom_col(position = "dodge")+
  geom_hline(aes(yintercept = 0), 
             col = "white",
             linewidth = 1)+
  facet_grid(basin ~ .)+
  scale_x_continuous(expand = c(0,0), 
                     breaks = seq(1988,this_year,by=3), limits = c(1999, this_year+1))+
  scale_y_continuous(expand = c(0,0),
                     breaks = c(-25, 0, 25),
                     labels = c("-25%", "0%", "+25%"))+
  scale_fill_manual("Basin", values = colors)+
  labs(x="", y="Percentile rank (% relative to 50%)", title = "Rainbow Smelt YOY")+
  inverse_theme()

ggsave(plot = smelt_yoy_percent, "figures/whole lake/percentile - smelt YOY.png", 
       width = width_print, height = height_print, units = "in", dpi = 600)


# Smelt adult
smelt_adult_percent <- forage_ranked %>%
  filter(species == "Rainbow Smelt" & age_class == "YAO" & !is.na(mean_abundance)) %>%
  mutate(basin = fct_relevel(basin, "WB", "CB", "EB", "LPB")) %>%
  ggplot(aes(x = year, y = over_under_rank, fill = basin))+
  geom_col(position = "dodge")+
  geom_hline(aes(yintercept = 0), 
             col = "white",
             linewidth = 1)+
  facet_grid(basin ~ .)+
  scale_x_continuous(expand = c(0,0), breaks = seq(1988,this_year,by=3), limits = c(1999, this_year+1))+
  scale_y_continuous(expand = c(0,0),
                     breaks = c(-25, 0, 25),
                     labels = c("-25%", "0%", "+25%"))+
  scale_fill_manual("Basin", values = colors)+
  labs(x="", y="Percentile rank (% relative to 50%)", title = "Rainbow Smelt adult")+
  inverse_theme()

ggsave(plot = smelt_adult_percent, "figures/whole lake/percentile - smelt adult.png", width = width_print, height = height_print, units = "in",dpi = 600)

# shiner All Ages
shiner_percent <- forage_ranked %>%
  filter(species == "Emerald Shiner" & !is.na(mean_abundance)) %>%
  mutate(basin = fct_relevel(basin, "WB", "CB", "EB", "LPB")) %>%
  ggplot(aes(x = year, y = over_under_rank, fill = basin))+
  geom_col(position = "dodge")+
  geom_hline(aes(yintercept = 0), 
             col = "white",
             linewidth = 1)+
  facet_grid(basin ~ .)+
  scale_x_continuous(expand = c(0,0), breaks = seq(1988,this_year,by=3), limits = c(1999, this_year+1))+
  scale_y_continuous(expand = c(0,0),
                     breaks = c(-25, 0, 25),
                     labels = c("-25%", "0%", "+25%"))+
  scale_fill_manual("Basin", values = colors)+
  labs(x="", y="Percentile rank (% relative to 50%)", title = "Emerald Shiner All Ages")+
  inverse_theme()

ggsave(plot = shiner_percent, "figures/whole lake/percentile - shiner ALL AGES.png", 
       width = width_print, height = height_print, units = "in",dpi = 600)

# shad YOY
shad_yoy_percent <- forage_ranked %>%
  filter(species == "Gizzard Shad" & age_class == "YOY" & !is.na(mean_abundance)) %>%
  mutate(basin = fct_relevel(basin, "WB", "CB", "EB", "LPB")) %>%
  ggplot(aes(x = year, y = over_under_rank, fill = basin))+
  geom_col(position = "dodge")+
  geom_hline(aes(yintercept = 0), 
             col = "white",
             linewidth = 1)+
  facet_grid(basin ~ .)+
  scale_x_continuous(expand = c(0,0), breaks = seq(1988,this_year,by=3), limits = c(1999, this_year+1))+
  scale_y_continuous(expand = c(0,0),
                     breaks = c(-25, 0, 25),
                     labels = c("-25%", "0%", "+25%"))+
  scale_fill_manual("Basin", values = colors)+
  labs(x="", y="Percentile rank (% relative to 50%)", title = "Gizzard Shad YOY")+
  inverse_theme()

ggsave(plot = shad_yoy_percent, "figures/whole lake/percentile - shad YOY.png", width = width_print, height = height_print, units = "in",dpi = 600)

# Goby all
goby_percent <- forage_ranked %>%
  filter(species == "Round Goby" & !is.na(mean_abundance)) %>%
  mutate(basin = fct_relevel(basin, "WB", "CB", "EB", "LPB")) %>%
  ggplot(aes(x = year, y = over_under_rank, fill = basin))+
  geom_col(position = "dodge")+
  geom_hline(aes(yintercept = 0), 
             col = "white",
             linewidth = 1)+
  facet_grid(basin ~ .)+
  scale_x_continuous(expand = c(0,0), breaks = seq(1988,this_year,by=3), limits = c(1999, this_year+1))+
  scale_y_continuous(expand = c(0,0),
                     breaks = c(-25, 0, 25),
                     labels = c("-25%", "0%", "+25%"))+
  scale_fill_manual("Basin", values = colors)+
  labs(x="", y="Percentile rank (% relative to 50%)", title = "Round Goby")+
  inverse_theme()

ggsave(plot = goby_percent, "figures/whole lake/percentile - goby.png", width = width_print, height = height_print, units = "in",dpi = 600)




### SOLE (State of Lake Erie) - Indicator graphs by Basin for 2020 LEC Pres   ############################

#"total forage" must be calculated from forage functional groups
total_forage <- forage %>%
  filter(age_class == "all") %>%
  group_by(year, basin) %>%
  summarize(total_forage = sum(abundance_per_ha)) %>% 
  ungroup

mean_forage <- total_forage %>% #mean of time series in the typical graphs, so ~1990 to present
  group_by(basin) %>%
  summarize(mean_forage = mean(total_forage)) %>%
  mutate(x_value = rep(2017, 3))

#graph it!
total_forage_all_basins <- total_forage %>%
  ggplot(aes(x = year, y = total_forage, color = basin))+
  geom_hline(data = mean_forage,
             aes(yintercept = mean_forage),
             color = "grey70",
             linewidth = 3,
             lty = 3)+
  geom_line(lwd = 2)+
  geom_text(data = mean_forage,
            aes(x = x_value, 
                y = mean_forage,
                label = basin),
            size = 6,
            color = "black",
            nudge_y = -3)+
  scale_x_continuous(expand = c(0,.5), 
                     breaks = seq(1990, this_year, by=5), 
                     limits = c(2000, this_year))+
  scale_y_continuous(expand = c(0,0),                     # MUST ADJUST THIS if forage numbers get big!
                     limits = c(0, 9000))+ 
  scale_color_manual("Basin", values = c("#51C1AB", "#FFD56B", "#6371C9"))+
  labs(x="", y="Fish/ha")+
  inverse_theme()

total_forage_all_basins

ggsave(plot = total_forage_all_basins, 
       "figures/whole lake/total forage abundance, all basins.png", 
       width = 13.5, height = 6.5, units = "in",dpi = 600)

# cut down to just last 5 years
sole_graph <- total_forage_all_basins+
  scale_x_continuous(expand = c(0,.5), 
                     breaks = 2016:2020, 
                     limits = c(2016, 2021))

sole_graph

write_csv(total_forage, "data derived/ftg sole indicator - all forage combined.csv")
