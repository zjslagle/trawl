
source("abundance summary.R")


#### Fig 2.4.2 Graph - mean abundance by fish group, 1988 - present   ##########################
cols = c("clupeid" = "red", "spiny_ray" = "yellow", "soft_ray" = "deepskyblue")

yearly_abundance_tall %>%
  filter(spp_agegroup %in% c("clupeid", "spiny_ray", "soft_ray")) %>%
  ggplot(aes(x = year, y = abundance_per_ha, group = spp_agegroup, fill = spp_agegroup))+
    geom_area(position = "stack", color = "black")+
    theme(axis.title = element_text(size=14, face = "bold"), axis.text.y=element_text(size=14, color = "black"), 
          axis.text.x=element_text(size=14, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
          panel.background = element_blank(), text = element_text(family = "Times"), legend.position = "top",
          legend.text = element_text(size = 12)) +
    scale_x_continuous(expand = c(0,0), breaks = seq(1988,this_year,by=5))+
    scale_y_continuous(expand = c(0,0))+
    labs(x="", y="Fish/ha")+
    scale_fill_manual(name = "", values = cols, labels = c("Clupeid", "Soft-rayed", "Spiny-rayed"))

ggsave(here("figures", "fig 2_4_2 rel abundance by group.png"), width = 7, height = 5, units = "in",dpi = 600)

### Fig 2.4.4 Graph - WAE and YP densities   ##################################
labs = c(walleye_yoy = "Age-0 Walleye", y_perch_yoy = "Age-0 Yellow Perch")

yearly_abundance_tall %>%
  filter(spp_agegroup %in% c("walleye_yoy", "y_perch_yoy")) %>%
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

ggsave(here("figures", "fig 2_4_4 rel abundance perch and walleye.png"), width = 7, height = 5, units = "in",dpi = 600)


### Fig 2.4.5 Graph - Gizzard Shad and White Perch densities   ##################################
# removed alewife since they haven't been relevant for 20 years
labs = c(giz_shad_yoy = "Age-0 Gizzard Shad", w_perch_yoy = "Age-0 White Perch")

yearly_abundance_tall %>%
  filter(spp_agegroup %in% c("giz_shad_yoy", "w_perch_yoy")) %>% 
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

ggsave(here("figures", "fig 2_4_5 rel abundance white perch and shad.png"), width = 7, height = 5, units = "in",dpi = 600)

### Fig 2.4.6 Graph - All ages of Emerald Shiner densities   ##################################
labs = c(em_shiner_adult = "Adult Emerald Shiner", em_shiner_yoy = "Age-0 Emerald Shiner")

yearly_abundance_tall %>%
  filter(spp_agegroup %in% c("em_shiner_adult", "em_shiner_yoy")) %>% 
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
                    values = c(em_shiner_adult = "#fc8d59", em_shiner_yoy = "#91bfdb"),
                    name = "")

ggsave(here("figures", "fig 2_4_6 rel abundance emerald shiner.png"), width = 7, height = 5, units = "in",dpi = 600)



#### Graph - goby abundance, 1988 - present   ##########################
yearly_abundance_tall %>%
  filter(spp_agegroup == "goby_adult") %>%
  ggplot(aes(x = year, y = abundance_per_ha))+
  geom_area()+
  theme(axis.title = element_text(size=14, face = "bold"), axis.text.y=element_text(size=14, color = "black"), 
        axis.text.x=element_text(size=14, vjust=1, color = "black"), axis.line = element_line(color = "black"), panel.background = element_blank()) +
  scale_x_continuous(expand = c(0,0), breaks = seq(1988,this_year,by=5))+
  scale_y_continuous(expand = c(0,0))+
  labs(x="", y="Fish/ha", main = "Goby abundance (#/ha)")


### New graph - % change for species this year vs last 10 years  ###########
yearly_abundance_tall %>% 
  filter(!str_detect(spp_agegroup, "notropis")) %>% # get rid of "notropis" category, we ID fish now\
  filter(!spp_agegroup %in% c("all_fish", "clupeid", "soft_ray", "spiny_ray")) %>%
  group_by(spp_agegroup) %>%
  summarize(ten_year_mean = mean(abundance_per_ha[which(year %in% seq(this_year-11,this_year-1))]),
            this_year_abundance = abundance_per_ha[which(year == this_year)],
            percent_diff = (((this_year_abundance+.0000001)-ten_year_mean)/ten_year_mean)*100) %>% #percentage change from 10 yr mean
  ungroup() %>% #write.csv(here("data", "percent change.csv")) 
  filter(rank(percent_diff) %in% c(1:5, seq(n(),n()-4))) %>% # get top and bottom five (biggest changes)
  ggplot(aes(x = reorder(spp_agegroup, percent_diff), y = percent_diff, fill = percent_diff))+
    geom_col()+
    theme_minimal()+
    labs(y = paste("% difference: ", this_year, "- 10 yr mean"))+
    scale_fill_distiller(type = "div", palette = "RdBu", direction = 1)+
    scale_y_continuous(limits = c(-100, 900), breaks = c(-100, 0, 200, 400, 600, 800), 
                       labels = c("-100%", "0", "+200%", "+400%", "+600%", "+800%"))+
    theme(axis.title = element_text(size=14, face = "bold"), axis.text.y=element_text(size=14, color = "black"), 
          axis.text.x=element_text(size=14, vjust=1, hjust = 1, color = "black", angle = 45), axis.line = element_line(color = "black"),
          axis.title.x = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), text = element_text(family = "Times"),
          strip.background = element_rect(fill = "white", color = "black"))

ggsave(here("figures", "percent difference - all species.png"), width = 7, height = 5, units = "in",dpi = 600)
