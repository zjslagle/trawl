source("Read Trawl Data.R")


#join effort data, then summarize over year, spp, age class
all_lengths %>%
  left_join(all_effort %>% select(projcode, agency, year)) %>%
  filter(!is.na(year), !is.na(length), year > 1987) %>%
  left_join(spp_codes, by = c(species = "code")) %>%
  group_by(year, species, agecode, common_name) %>%
  summarize(mean_length = mean(length),
            sd_length = sd(length),
            n_length = n()) %>%
  ungroup -> mean_lengths

#### Fig 4.3.3 - mean lengths for sportfish species
mean_lengths %>%
  filter(species %in% c(301, 302, 331, 334), agecode == 1) %>%
  ggplot(aes(x = year, y = mean_length, group = common_name, lty = common_name, color = common_name))+
    geom_line(size = 1)+
    geom_point(aes(pch = common_name))+
    theme(axis.title = element_text(size=14, face = "bold"), axis.text.y=element_text(size=14, color = "black"), 
           axis.text.x=element_text(size=14, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
           panel.background = element_blank(), text = element_text(family = "Times"), legend.position = "top") +
    scale_x_continuous(expand = c(0,0), breaks = seq(1988,this_year,by=5))+
    scale_y_continuous(expand = c(0,0), limits = c(40, 170))+
    labs(x="", y="Mean total length (mm)")+
    scale_color_brewer(palette = "Paired", name = "")+
    scale_linetype_discrete(name = "")+scale_shape_discrete(name = "")

ggsave(here("figures", "Fig 4_3_3 mean lengths.png"), width = 6, height = 4, units = "in",dpi = 600)


