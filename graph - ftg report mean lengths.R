source("1_read data - 2020 format.R")

### FTG report - Mean length for YOY sportfish over time      ##########

#join effort data, then summarize over year, spp, age class
mean_lengths <- all_lengths %>%
  left_join(all_effort %>% select(yrsam, year)) %>%
  filter(!is.na(year), !is.na(length), year > 1987) %>%
  left_join(spp_codes) %>%
  group_by(year, species, agecode, common_name) %>%
  summarize(mean_length = mean(length),
            sd_length = sd(length),
            n_length = n()) %>%
  ungroup

#### Fig 2_3_3_1 - mean lengths for sportfish species
mean_lengths %>%
  filter(species %in% c(301, 302, 331, 334), agecode == 1) %>% 
  ggplot(aes(x = year, y = mean_length, group = common_name, lty = common_name, color = common_name))+
    geom_line(linewidth = 1)+
    geom_point(aes(pch = common_name))+
    scale_x_continuous(expand = c(0,0), breaks = seq(1988,this_year,by=5))+
    scale_y_continuous(expand = c(0,0))+
    #coord_cartesian(xlim = c(2000, this_year))+          # <- only recent years
    labs(x="", y="Mean total length (mm)")+
    scale_color_brewer(palette = "Paired", name = "")+
    scale_linetype_discrete(name = "")+
    scale_shape_discrete(name = "")+
    theme(axis.title = element_text(size=14, face = "bold"), 
          axis.text.y=element_text(size=14, color = "black"), 
          axis.text.x=element_text(size=14, vjust=1, color = "black"), 
          axis.line = element_line(color = "black"), 
          panel.background = element_blank(), 
          text = element_text(family = "Times"), 
          legend.position = "top",
          legend.key = element_blank(),
          plot.margin = unit(c(.5,1,.5,.5), "cm")) 

ggsave("figures/ftg report/Fig 2_3_3_1 mean lengths.png", width = 6, height = 3.5, units = "in",dpi = 600)

# text summary:
mean_lengths %>% filter(species %in% c(334, 331, 302, 301), agecode == 1, year == this_year-1)
mean_lengths %>% filter(species %in% c(334, 331, 302, 301), agecode == 1, year == this_year)

#### Fig - mean lengths for sportfish species MINUS WAE
mean_lengths %>%
  filter(species %in% c(301, 302, 331), agecode == 1) %>% 
  ggplot(aes(x = year, y = mean_length, group = common_name, lty = common_name, color = common_name))+
  geom_line(linewidth = 1)+
  geom_point(aes(pch = common_name))+
  scale_x_continuous(expand = c(0,0), breaks = seq(1988,this_year,by=5))+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(xlim = c(2000, this_year))+          # <- only recent years
  labs(x="", y="Mean total length (mm)")+
  scale_color_brewer(palette = "Paired", name = "")+
  scale_linetype_discrete(name = "")+
  scale_shape_discrete(name = "")+
  theme(axis.title = element_text(size=14, face = "bold"), 
        axis.text.y=element_text(size=14, color = "black"), 
        axis.text.x=element_text(size=14, vjust=1, color = "black"), 
        axis.line = element_line(color = "black"), 
        panel.background = element_blank(), 
        text = element_text(family = "Times"), 
        legend.position = "top",
        legend.key = element_blank()) 

ggsave("figures/mean lengths - sportfishes minus WAE.png", width = 6, height = 4, units = "in",dpi = 600)

#### Similar to above, but prey fish
mean_lengths %>%
  filter(species %in% c(196, 063) & agecode == 1| species == 366) %>% 
  ggplot(aes(x = year, y = mean_length, group = common_name, lty = common_name, color = common_name))+
  geom_line(linewidth = 1)+
  geom_point(aes(pch = common_name))+
  scale_x_continuous(expand = c(0,0), breaks = seq(1988,this_year,by=5))+
  scale_y_continuous(expand = c(0,0))+
  labs(x="", y="Mean total length (mm)")+
  scale_color_brewer(palette = "Paired", name = "")+
  scale_linetype_discrete(name = "")+
  scale_shape_discrete(name = "")+
  theme(axis.title = element_text(size=14, face = "bold"), 
        axis.text.y=element_text(size=14, color = "black"), 
        axis.text.x=element_text(size=14, vjust=1, color = "black"), 
        axis.line = element_line(color = "black"), 
        panel.background = element_blank(), 
        text = element_text(family = "Times"), 
        legend.position = "top",
        legend.key = element_blank()) 

ggsave("figures/mean lengths - preyfishes.png", width = 6, height = 4, units = "in",dpi = 600)

mean_lengths %>%
  filter(species == 196, agecode == 1) %>%
  ggplot(aes(x = year, y = mean_length))+
    geom_line()
