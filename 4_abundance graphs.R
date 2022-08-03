
source("2_abundance summary.R")

# Figures for FTG report and meeting presentation

#### Fig 2.4.2 Graph - mean abundance by fish group, 1988 - present   ##########################
cols = c("clupeid" = "red", "spiny_ray" = "yellow", "soft_ray" = "deepskyblue")

density_by_group <- yearly_abundance_tall %>%
  filter(spp_agegroup %in% c("clupeid", "spiny_ray", "soft_ray")) %>%
  ggplot(aes(x = year, y = abundance_per_ha, group = spp_agegroup, fill = spp_agegroup))+
    geom_area(position = "stack", color = "black")+
    theme(axis.title = element_text(size=14, face = "bold"), 
          axis.text.y=element_text(size=14, color = "black"), 
          axis.text.x=element_text(size=14, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
          panel.background = element_blank(), text = element_text(family = "Times"), legend.position = "top",
          legend.text = element_text(size = 12)) +
    scale_x_continuous(expand = c(0,0), breaks = seq(1988,this_year,by=5))+
    scale_y_continuous(expand = c(0,0))+
    labs(x="", y="Fish/ha")+
    scale_fill_manual(name = "", values = cols, labels = c("Clupeid", "Spiny-rayed", "Soft-rayed"))

density_by_group

ggsave(plot = density_by_group, "figures/fig 2_4_2 West Basin Density by group.png", 
       width = 7, height = 4, units = "in",dpi = 600)


### Fig 2.4.4 Graph - WAE and YP densities   ##################################
labs = c("334_1" = "Age-0 Walleye", "331_1" = "Age-0 Yellow Perch")

yearly_abundance_tall %>%
  filter(spp_agegroup %in% c("334_1", "331_1")) %>%
  ggplot(aes(x = year, y = abundance_per_ha))+
    geom_area(position = "stack", color = "black")+
    facet_grid(spp_agegroup ~ ., scales = "free", labeller = labeller(spp_agegroup = labs))+
    theme(axis.title = element_text(size=14, face = "bold"), axis.text.y=element_text(size=14, color = "black"), 
          axis.text.x=element_text(size=14, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
          panel.background = element_blank(), text = element_text(family = "Times"), legend.position = "top",
          strip.background = element_rect(fill = "white", color = "black"), strip.text = element_text(face = "bold", size = 12)) +
    scale_x_continuous(expand = c(0,.5), breaks = seq(1988,this_year,by=5))+
    scale_y_continuous(expand = c(0,0))+
    labs(x="", y="Fish/ha")

ggsave("figures/fig 2_4_4 rel abundance perch and walleye.png", 
       width = 7, height = 4, units = "in",dpi = 600)


### Fig 2.4.5 Graph - Gizzard Shad and White Perch densities   ##################################
# removed alewife since they haven't been relevant for 20 years
labs = c("63_1" = "Age-0 Gizzard Shad", "301_1" = "Age-0 White Perch")

yearly_abundance_tall %>%
  filter(spp_agegroup %in% c("63_1", "301_1")) %>% 
  ggplot(aes(x = year, y = abundance_per_ha))+
  geom_area(position = "stack", color = "black")+
  facet_grid(spp_agegroup ~ ., scales = "free", labeller = labeller(spp_agegroup = labs))+
  theme(axis.title = element_text(size=14, face = "bold"), axis.text.y=element_text(size=14, color = "black"), 
        axis.text.x=element_text(size=14, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
        panel.background = element_blank(), text = element_text(family = "Times"), legend.position = "top",
        strip.background = element_rect(fill = "white", color = "black"), strip.text = element_text(face = "bold", size = 12)) +
  scale_x_continuous(expand = c(0,.5), breaks = seq(1988,this_year,by=5))+
  scale_y_continuous(expand = c(0,0))+
  labs(x="", y="Fish/ha")

ggsave("figures/fig 2_4_5 rel abundance white perch and shad.png", 
       width = 7, height = 4, units = "in",dpi = 600)

### Fig 2.4.6 Graph - All ages of Emerald Shiner densities   ##################################
labs = c("196_3" = "Adult Emerald Shiner", "196_1" = "Age-0 Emerald Shiner")

yearly_abundance_tall %>%
  filter(spp_agegroup %in% c("196_3", "196_1")) %>% 
  ggplot(aes(x = year, y = abundance_per_ha, group = spp_agegroup, fill = spp_agegroup))+
  geom_area(position = "stack", alpha = .7, color = "black", lty = 2)+
  #facet_grid(spp_agegroup ~ ., scales = "free", labeller = labeller(spp_agegroup = labs))+
  theme(axis.title = element_text(size=14, face = "bold"), axis.text.y=element_text(size=14, color = "black"), 
        axis.text.x=element_text(size=14, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
        panel.background = element_blank(), text = element_text(family = "Times"), legend.position = "top",
        strip.background = element_rect(fill = "white", color = "black"), strip.text = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12)) +
  scale_x_continuous(expand = c(0,0), breaks = seq(1988,this_year,by=5))+
  scale_y_continuous(expand = c(0,0))+
  labs(x="", y="Fish/ha")+
  scale_fill_manual(labels = labs, 
                    values = c("196_3" = "#fc8d59", "196_1" = "#91bfdb"),
                    name = "")

ggsave("figures/fig 2_4_6 rel abundance emerald shiner.png", 
       width = 7, height = 4, units = "in",dpi = 600)

# Graph - Goby and Troutperch
labs = c("366_5" = "Round Goby", "291_3" = "Adult Troutperch")

yearly_abundance_tall %>%
  filter(spp_agegroup %in% c("366_5", "291_3")) %>%
  ggplot(aes(x = year, y = abundance_per_ha))+
  geom_area(position = "stack", color = "black")+
  facet_grid(spp_agegroup ~ ., scales = "free", labeller = labeller(spp_agegroup = labs))+
  theme(axis.title = element_text(size=14, face = "bold"), axis.text.y=element_text(size=14, color = "black"), 
        axis.text.x=element_text(size=14, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
        panel.background = element_blank(), text = element_text(family = "Times"), legend.position = "top",
        strip.background = element_rect(fill = "white", color = "black"), strip.text = element_text(face = "bold", size = 12)) +
  scale_x_continuous(expand = c(0,.5), breaks = seq(1988,this_year,by=5))+
  scale_y_continuous(expand = c(0,0))+
  labs(x="", y="Fish/ha")

ggsave("figures/rel abundance goby and and troutperch.png", 
       width = 7, height = 5, units = "in",dpi = 600)


### Abundance graph for any species  #########################
target_spp = "302_1"

yearly_abundance_tall %>%
  filter(spp_agegroup == target_spp) %>% 
  ggplot(aes(x = year, y = abundance_per_ha))+
  geom_area(position = "stack", color = "black")+
  theme(axis.title = element_text(size=14, face = "bold"), axis.text.y=element_text(size=14, color = "black"), 
        axis.text.x=element_text(size=14, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
        panel.background = element_blank(), text = element_text(family = "Times"), legend.position = "top",
        strip.background = element_rect(fill = "white", color = "black"), strip.text = element_text(face = "bold", size = 12)) +
  scale_x_continuous(expand = c(0,.5), breaks = seq(1988,this_year,by=5))+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "", y = "Fish/ha", 
       title = paste(target_spp, "relative abundance over time, West Basin Lake Erie"),
       subtitle = "Source: interagency (OMNDMNRF & ODNR) trawl data, 1988 - present")

ggsave(paste("figures/rel abundance ", target_spp, ".png"), 
       width = 7, height = 4, units = "in",dpi = 600)

### Graph - % change for species this year vs last 10 years  ###########

# turn 2nd line on for top 5 +/- changes
abund_percent_diff %>%
 filter(rank(percent_diff) %in% c(1:5, seq(n(),n()-4))) %>% # get top and bottom five (biggest changes) - turn OFF for All Species
  ggplot(aes(x = reorder(paste0(common_name, "-AC ", agecode), percent_diff), y = percent_diff, fill = percent_diff))+
    geom_col()+
    theme_minimal()+
    labs(y = paste("% difference: ", this_year, "- 10 yr mean"),
         title = paste("Percent difference from", this_year, "vs 10 year mean"))+
    scale_fill_distiller(type = "div", palette = "RdBu", direction = 1)+
    scale_y_continuous(limits = c(-100, max(abund_percent_diff$percent_diff)), 
                       breaks = c(-100, 0, 200, 400, 600, 800, 1000, 1200, 1400), 
                       labels = c("-100%", "0", "+200%", "+400%", "+600%", "+800%", "+1000%", "+1200%", "+1400%"))+
    theme(axis.title = element_text(size=14, face = "bold"), axis.text.y=element_text(size=14, color = "black"), 
          axis.text.x=element_text(size=14, vjust=1, hjust = 1, color = "black", angle = 45), axis.line = element_line(color = "black"),
          axis.title.x = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), text = element_text(family = "Times"),
          strip.background = element_rect(fill = "white", color = "black"))

ggsave("figures/percent difference - top 5.png", width = 7, height = 5, units = "in",dpi = 600)


# Log scale (only for increases, log(negative number) is undefined)
abund_percent_diff %>%
  filter(rank(percent_diff) %in% seq(n(),n()-4)) %>% #top 5
  ggplot(aes(x = reorder(spp_agegroup, percent_diff), y = percent_diff, fill = percent_diff))+
  geom_col(col = "black")+
  theme_minimal()+
  labs(y = paste("Log % difference: ", this_year, "- 10 yr mean"))+
  scale_fill_distiller(type = "seq", palette = "Blues", direction = 1)+
  scale_y_log10(labels = function(x) x/10)+
  theme(axis.title = element_text(size=14, face = "bold"), axis.text.y=element_text(size=14, color = "black"), 
        axis.text.x=element_text(size=14, vjust=1, hjust = 1, color = "black", angle = 45), axis.line = element_line(color = "black"),
        axis.title.x = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), text = element_text(family = "Times"),
        strip.background = element_rect(fill = "white", color = "black"))

