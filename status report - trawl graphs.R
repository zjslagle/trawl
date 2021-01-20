### ODNR - DOW Status Report - Trawl Graphs for Status Report Update    ##########################
# Z Slagle      Work began May 2020.

require(ggsci)
require(cowplot)
source("ohio read data.R")

### Load and format DuFour biomass data    ############################
# units are 1000's of kilograms
biomass_data <- read_excel(path = here("data", "Ohio_WBtrawl_1992_2018_dufour.xlsx"), 
                      sheet = "Ohio_Biomass_1992_2018")

biomass_data %<>% janitor::clean_names()

# need to make data long format for easier GGplotting
biomass_data %>%
  gather(-year, -t_mon, key = "species", value = "biomass") %>%
  filter(t_mon == 7) %>%                                   #only use July data, to match with hydroacoustics
  mutate(biomass = biomass/907) -> biomass_data     # convert biomass to tons
  
# what are the species/species groups?
biomass_data %>% select(species) %>% unique %>% as.list

### biomass by species groups over years       #######################
# similar to FTG report, but Ohio only

biomass_data %>%
  filter(species %in% c("totcatch", "cluptot", "softtot", "spintot")) %>%
  group_by(species) %>%
  mutate(st_dev = sd(biomass)) %>%
  ungroup %>% 
  mutate(species = case_when(species == "totcatch" ~ "Total",
                             species == "cluptot" ~ "Shad",
                             species == "softtot" ~ "Soft-rayed",
                             species == "spintot" ~ "Spiny-rayed")) %>%
  ggplot(aes(x = year, y = biomass, group = species, color = species, lty = species))+
    #geom_ribbon(aes(ymin = biomass-st_dev, ymax = biomass+ st_dev), 
    #            color = NA, fill = "grey30", alpha = .2)+
    geom_line(size = 2)+
    scale_color_manual(values = c("#66C5CC","#F6CF71","#F89C74","#9c74f8"), name = "Species group")+
    scale_linetype_manual(name = "Species group", values = c(1,2,6,4))+
    scale_x_continuous(breaks = seq(1992, 2020, by = 5), expand = c(.03,0))+
    scale_y_continuous(expand = c(0,5))+
    labs(y = "Biomass (tons)")+
    coord_cartesian(ylim = c(0,200), xlim = c(1992, this_year))+
    theme_minimal()+
    theme(axis.title.x = element_blank(), panel.grid = element_blank(), axis.text = element_text(size = 12),
          legend.position = c(.2,.8), legend.key.width = unit(15, "mm"),
          panel.background = element_rect(color = "grey40"), strip.background = element_rect(color = "grey40"),
          legend.background = element_rect(color = "grey40"), strip.text = element_text(size = 8, face = "bold")) #-> biomass_plot_2

ggsave(here("figures", "status - biomass by group.png"), 
       width = 7, height = 5, units = "in",dpi = 600)

ggsave(plot = plot_grid(biomass_plot_1, biomass_plot_2, nrow = 1, labels = c("A", "B")), 
       filename = here("figures", "status - biomass by group COMBO.png"), 
       width = 10, height = 5, units = "in",dpi = 600)


### biomass by Specific Species over years - paneled       #######################
# similar to FTG report, but Ohio only

biomass_data %>%
  filter(species %in% c("smeltot", "shadtot", "eshin2to", "gobyadto")) %>%
  group_by(species) %>%
  mutate(st_dev = sd(biomass)) %>%
  ungroup %>% 
  mutate(species = case_when(species == "smeltot" ~ "Rainbow Smelt",
                             species == "shadtot" ~ "Shad",
                             species == "eshin2to" ~ "Emerald Shiner",
                             species == "gobyadto" ~ "Goby")) %>%
  ggplot(aes(x = year, y = biomass, group = species, color = species))+
  geom_line(size = 2)+
  scale_color_manual(values = c("#66C5CC","#F6CF71","#F89C74","#9c74f8"), name = "Species group")+
  scale_x_continuous(breaks = seq(1992, 2020, by = 5), expand = c(.03,0))+
  #scale_y_continuous(expand = c(0,5))+
  facet_grid(species ~ ., scales = "free_y")+
  labs(y = "Biomass (tons)")+
  coord_cartesian(xlim = c(1992, this_year))+
  theme_minimal()+
  theme(axis.title.x = element_blank(), panel.grid = element_blank(), axis.text = element_text(size = 12),
        legend.position = "bottom", 
        panel.background = element_rect(color = "grey40"), strip.background = element_rect(color = "grey40"),
        legend.background = element_rect(color = "grey40"), strip.text = element_text(size = 8, face = "bold"))

ggsave(here("figures", "status - biomass by important species.png"), 
       width = 7, height = 5, units = "in",dpi = 600)

