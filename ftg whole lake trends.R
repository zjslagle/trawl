### Forage Summary of whole-lake trends     ######################################
# For Forage Task Group
# Zak Slagle Mar 2020

library(magrittr)
#library(sqldf)
library(forcats)
require(RODBC)
require(lubridate)
require(stringr)
require(here)
require(readxl)
library(tidyverse)


this_year = year(now())-1 #get this year
windowsFonts(Times=windowsFont("Times New Roman"))

# read data
forage <- read_excel("C:/Users/slaglez/Documents/Forage TG/Forage Fish Trends_All Agencies.xlsx", sheet = "trawl_tall")

forage %<>% mutate(basin = case_when(agency == "ON" ~ "LPB", T ~ basin))  # convert "ON" to Long Point Bay (see trawl lack of correlation, below)

### Summary counts and Within Basin Comps     ###############################
#total counts of data by basin/agency
forage %>%
  group_by(basin, agency) %>%
  summarize(total_num = n())

# years of data by agency (if they're not close within basin, will throw off trends these graphs are 2000-present)
forage %>%
  filter(species == "Rainbow Smelt" & age_class == "YOY" & year > 1999) %>%
  group_by(basin, agency) %>%
  summarize(total_years = sum(!is.na((abundance_per_ha))))

# do EB trawls correlate? NOPE
forage %>%
  filter(basin %in% c("EB", "LPB")) %>%
  mutate(abundance_per_ha = abundance_per_ha+.001) %>%
  spread(key = agency, value = abundance_per_ha) %>%
  filter(!is.na(NY)) -> eb_data_comp

eb_data_comp %>%
  ggplot(aes(x = NY, y = ON))+
  stat_smooth(method = "lm", color = "red")+
  geom_point(size = 2)+
  facet_grid(species ~ age_class)+
  theme_minimal()+
  scale_x_log10()+
  scale_y_log10()
  




### Abundance Plots by Species  ######################

# Smelt YOY
forage %>%
  filter(species == "Rainbow Smelt" & age_class == "YOY" & !is.na(abundance_per_ha) & agency != "MI") %>%
  group_by(year, basin) %>%
  summarize(mean_abundance = mean(abundance_per_ha)) %>%
  ungroup %>%
  mutate(basin = fct_relevel(basin, "WB", "CB", "EB", "LPB")) %>%
  ggplot(aes(x = year, y = mean_abundance, color = basin))+
  geom_line(lwd = 2)+
  scale_x_continuous(expand = c(0,.5), breaks = seq(1988,this_year,by=3), limits = c(2000, this_year))+
  scale_y_continuous(expand = c(0,0))+
  scale_color_manual("Basin", values = c("#51C1AB", "#FFD56B", "#6371C9", "#FFA56B"))+
  labs(x="", y="Fish/ha", title = "Rainbow Smelt YOY")+
  theme(axis.title = element_text(size=16, face = "bold"), axis.text.y=element_text(size=16, color = "black"), 
        axis.text.x=element_text(size=16, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
        panel.background = element_blank(), text = element_text(family = "Times"),
        legend.text = element_text(size = 14, face = "bold"), legend.title = element_text(size = 14, face = "bold"), legend.key = element_blank(),
        strip.background = element_rect(fill = "white", color = "black"), strip.text = element_text(face = "bold", size = 12),
        rect = element_rect(fill = "transparent"), legend.position = c(.9, .9), plot.title = element_text(color = "grey70", size = 10))-> smelt_yoy

ggsave(plot = smelt_yoy, here("figures", "whole lake","smelt YOY.png"), width = 10, height = 6.5, units = "in",dpi = 600)


# Smelt adult
forage %>%
  filter(species == "Rainbow Smelt" & age_class == "YAO" & !is.na(abundance_per_ha) & agency != "MI") %>%
  group_by(year, basin) %>%
  summarize(mean_abundance = mean(abundance_per_ha)) %>%
  ungroup %>%
  mutate(basin = fct_relevel(basin, "WB", "CB", "EB", "LPB")) %>%
  ggplot(aes(x = year, y = mean_abundance, color = basin))+
  geom_line(lwd = 2)+
  scale_x_continuous(expand = c(0,.5), breaks = seq(1988,this_year,by=3), limits = c(2000, this_year))+
  scale_y_continuous(expand = c(0,0))+
  scale_color_manual("Basin", values = c("#51C1AB", "#FFD56B", "#6371C9", "#FFA56B"))+
  labs(x="", y="Fish/ha", title = "Rainbow Smelt YAO")+
  theme(axis.title = element_text(size=16, face = "bold"), axis.text.y=element_text(size=16, color = "black"), 
        axis.text.x=element_text(size=16, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
        panel.background = element_blank(), text = element_text(family = "Times"),
        legend.text = element_text(size = 14, face = "bold"), legend.title = element_text(size = 14, face = "bold"), legend.key = element_blank(),
        strip.background = element_rect(fill = "white", color = "black"), strip.text = element_text(face = "bold", size = 12),
        rect = element_rect(fill = "transparent"), legend.position = c(.9, .9), plot.title = element_text(color = "grey70", size = 10))-> smelt_adult

ggsave(plot = smelt_adult, here("figures", "whole lake","smelt adult.png"), width = 10, height = 6.5, units = "in",dpi = 600)


# Shiner YOY
forage %>%
  filter(species == "Emerald Shiner" & age_class == "YOY" & !is.na(abundance_per_ha) & agency != "MI") %>%
  group_by(year, basin) %>%
  summarize(mean_abundance = mean(abundance_per_ha)) %>%
  ungroup %>%
  mutate(basin = fct_relevel(basin, "WB", "CB", "EB", "LPB")) %>%
  ggplot(aes(x = year, y = mean_abundance, color = basin))+
  geom_line(lwd = 2)+
  scale_x_continuous(expand = c(0,.5), breaks = seq(1988,this_year,by=3), limits = c(2000, this_year))+
  scale_y_continuous(expand = c(0,0))+
  scale_color_manual("Basin", values = c("#51C1AB", "#FFD56B", "#6371C9", "#FFA56B"))+
  labs(x="", y="Fish/ha", title = "Emerald Shiner YOY")+
  theme(axis.title = element_text(size=16, face = "bold"), axis.text.y=element_text(size=16, color = "black"), 
        axis.text.x=element_text(size=16, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
        panel.background = element_blank(), text = element_text(family = "Times"),
        legend.text = element_text(size = 14, face = "bold"), legend.title = element_text(size = 14, face = "bold"), legend.key = element_blank(),
        strip.background = element_rect(fill = "white", color = "black"), strip.text = element_text(face = "bold", size = 12),
        rect = element_rect(fill = "transparent"), legend.position = c(.9, .9), plot.title = element_text(color = "grey70", size = 10))-> shiner_yoy

ggsave(plot = shiner_yoy, here("figures", "whole lake","shiner YOY.png"), width = 10, height = 6.5, units = "in",dpi = 600)


# Shiner adult
forage %>%
  filter(species == "Emerald Shiner" & age_class == "YAO" & !is.na(abundance_per_ha) & agency != "MI") %>%
  group_by(year, basin) %>%
  summarize(mean_abundance = mean(abundance_per_ha)) %>%
  ungroup %>%
  mutate(basin = fct_relevel(basin, "WB", "CB", "EB", "LPB")) %>%
  ggplot(aes(x = year, y = mean_abundance, color = basin))+
  geom_line(lwd = 2)+
  scale_x_continuous(expand = c(0,.5), breaks = seq(1988,this_year,by=3), limits = c(2000, this_year))+
  scale_y_continuous(expand = c(0,0))+
  scale_color_manual("Basin", values = c("#51C1AB", "#FFD56B", "#6371C9", "#FFA56B"))+
  labs(x="", y="Fish/ha", title = "Emerald Shiner YAO")+
  theme(axis.title = element_text(size=16, face = "bold"), axis.text.y=element_text(size=16, color = "black"), 
        axis.text.x=element_text(size=16, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
        panel.background = element_blank(), text = element_text(family = "Times"),
        legend.text = element_text(size = 14, face = "bold"), legend.title = element_text(size = 14, face = "bold"), legend.key = element_blank(),
        strip.background = element_rect(fill = "white", color = "black"), strip.text = element_text(face = "bold", size = 12),
        rect = element_rect(fill = "transparent"), legend.position = c(.9, .9), plot.title = element_text(color = "grey70", size = 10))-> shiner_adult

ggsave(plot = shiner_adult, here("figures", "whole lake","shiner adult.png"), width = 10, height = 6.5, units = "in",dpi = 600)


# Shad YOY
forage %>%
  filter(species == "Gizzard Shad" & age_class == "YOY" & !is.na(abundance_per_ha) & agency != "MI") %>%
  group_by(year, basin) %>%
  summarize(mean_abundance = mean(abundance_per_ha)) %>%
  ungroup %>%
  mutate(basin = fct_relevel(basin, "WB", "CB", "EB", "LPB")) %>%
  ggplot(aes(x = year, y = mean_abundance, color = basin))+
  geom_line(lwd = 2)+
  scale_x_continuous(expand = c(0,.5), breaks = seq(1988,this_year,by=3), limits = c(2000, this_year))+
  scale_y_continuous(expand = c(0,0))+
  scale_color_manual("Basin", values = c("#51C1AB", "#FFD56B", "#6371C9", "#FFA56B"))+
  labs(x="", y="Fish/ha", title = "Gizzard Shad YOY")+
  theme(axis.title = element_text(size=16, face = "bold"), axis.text.y=element_text(size=16, color = "black"), 
        axis.text.x=element_text(size=16, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
        panel.background = element_blank(), text = element_text(family = "Times"),
        legend.text = element_text(size = 14, face = "bold"), legend.title = element_text(size = 14, face = "bold"), legend.key = element_blank(),
        strip.background = element_rect(fill = "white", color = "black"), strip.text = element_text(face = "bold", size = 12),
        rect = element_rect(fill = "transparent"), legend.position = c(.9, .9), plot.title = element_text(color = "grey70", size = 10))-> shad_yoy

ggsave(plot = shad_yoy, here("figures", "whole lake","shad YOY.png"), width = 10, height = 6.5, units = "in",dpi = 600)

# Goby
forage %>%
  filter(species == "Round Goby" & !is.na(abundance_per_ha)) %>%
  group_by(year, basin) %>%
  summarize(mean_abundance = mean(abundance_per_ha)) %>%
  ungroup %>%
  mutate(basin = fct_relevel(basin, "WB", "CB", "EB", "LPB")) %>%
  ggplot(aes(x = year, y = mean_abundance, color = basin))+
  geom_line(lwd = 2)+
  scale_x_continuous(expand = c(0,.5), breaks = seq(1988,this_year,by=3), limits = c(2000, this_year))+
  scale_y_continuous(expand = c(0,0))+
  scale_color_manual("Basin", values = c("#51C1AB", "#FFD56B", "#6371C9", "#FFA56B"))+
  labs(x="", y="Fish/ha", title = "Round Goby")+
  theme(axis.title = element_text(size=16, face = "bold"), axis.text.y=element_text(size=16, color = "black"), 
        axis.text.x=element_text(size=16, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
        panel.background = element_blank(), text = element_text(family = "Times"),
        legend.text = element_text(size = 14, face = "bold"), legend.title = element_text(size = 14, face = "bold"), legend.key = element_blank(),
        strip.background = element_rect(fill = "white", color = "black"), strip.text = element_text(face = "bold", size = 12),
        rect = element_rect(fill = "transparent"), legend.position = c(.9, .9), plot.title = element_text(color = "grey70", size = 10))-> goby_all

ggsave(plot = goby_all, here("figures", "whole lake","goby all.png"), width = 10, height = 6.5, units = "in",dpi = 600)



### Percentile Plots by species    #####################

#calc percentile rank within basin 
forage %>%
  filter(!is.na(abundance_per_ha) & agency != "MI") %>%
  group_by(year, basin, species, age_class) %>%
  summarize(mean_abundance = mean(abundance_per_ha)) %>%
  ungroup %>%
  group_by(basin, species, age_class) %>%
  mutate(rank = rank(mean_abundance, na.last = NA), 
         n = length(mean_abundance),
         percent_rank = round(rank(mean_abundance, na.last = NA)/length(mean_abundance) * 100,1), #percentile rank
         over_under_rank = percent_rank-50) %>%                                                   # percentile rank relative to 50% (under = negative)
  arrange(basin, species, age_class, year) %>% 
  ungroup -> forage_ranked

# Smelt YOY
forage_ranked %>%
  filter(species == "Rainbow Smelt" & age_class == "YOY" & !is.na(mean_abundance) & year > 1999) %>%
  mutate(basin = fct_relevel(basin, "WB", "CB", "EB", "LPB")) %>%
  ggplot(aes(x = year, y = over_under_rank, fill = basin))+
  geom_col(position = "dodge")+
  facet_grid(basin ~ .)+
  scale_x_continuous(expand = c(0,0), breaks = seq(1988,this_year,by=3), limits = c(1999, this_year+1))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual("Basin", values = c("#51C1AB", "#FFD56B", "#6371C9", "#FFA56B"))+
  geom_hline(aes(yintercept = 0))+
  labs(x="", y="Percentile rank (%)", title = "Rainbow Smelt YOY")+
  theme(axis.title = element_text(size=16, face = "bold"), axis.text.y=element_text(size=16, color = "black"), 
        axis.text.x=element_text(size=16, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
        panel.background = element_blank(), text = element_text(family = "Times"),
        legend.text = element_text(size = 14, face = "bold"), legend.title = element_text(size = 14, face = "bold"), legend.key = element_blank(),
        strip.background = element_rect(fill = "white", color = "black"), strip.text = element_text(face = "bold", size = 12),
        rect = element_rect(fill = "transparent"), legend.position = "top", plot.title = element_text(color = "grey70", size = 10),
        panel.spacing.y = unit(5, "mm"))-> smelt_yoy_percent

ggsave(plot = smelt_yoy_percent, here("figures", "whole lake","percentile - smelt YOY.png"), width = 10, height = 6.5, units = "in",dpi = 600)


# Smelt adult
forage_ranked %>%
  filter(species == "Rainbow Smelt" & age_class == "YAO" & !is.na(mean_abundance) & year > 1999) %>%
  mutate(basin = fct_relevel(basin, "WB", "CB", "EB", "LPB")) %>%
  ggplot(aes(x = year, y = over_under_rank, fill = basin))+
  geom_col(position = "dodge")+
  facet_grid(basin ~ .)+
  scale_x_continuous(expand = c(0,0), breaks = seq(1988,this_year,by=3), limits = c(1999, this_year+1))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual("Basin", values = c("#51C1AB", "#FFD56B", "#6371C9", "#FFA56B"))+
  geom_hline(aes(yintercept = 0))+
  labs(x="", y="Percentile rank (%)", title = "Rainbow Smelt adult")+
  theme(axis.title = element_text(size=16, face = "bold"), axis.text.y=element_text(size=16, color = "black"), 
        axis.text.x=element_text(size=16, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
        panel.background = element_blank(), text = element_text(family = "Times"),
        legend.text = element_text(size = 14, face = "bold"), legend.title = element_text(size = 14, face = "bold"), legend.key = element_blank(),
        strip.background = element_rect(fill = "white", color = "black"), strip.text = element_text(face = "bold", size = 12),
        rect = element_rect(fill = "transparent"), legend.position = "top", plot.title = element_text(color = "grey70", size = 10),
        panel.spacing.y = unit(5, "mm"))-> smelt_adult_percent

ggsave(plot = smelt_adult_percent, here("figures", "whole lake","percentile - smelt adult.png"), width = 10, height = 6.5, units = "in",dpi = 600)

# shiner YOY
forage_ranked %>%
  filter(species == "Emerald Shiner" & age_class == "YOY" & !is.na(mean_abundance) & year > 1999) %>%
  mutate(basin = fct_relevel(basin, "WB", "CB", "EB", "LPB")) %>%
  ggplot(aes(x = year, y = over_under_rank, fill = basin))+
  geom_col(position = "dodge")+
  facet_grid(basin ~ .)+
  scale_x_continuous(expand = c(0,0), breaks = seq(1988,this_year,by=3), limits = c(1999, this_year+1))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual("Basin", values = c("#51C1AB", "#FFD56B", "#6371C9", "#FFA56B"))+
  geom_hline(aes(yintercept = 0))+
  labs(x="", y="Percentile rank (%)", title = "Emerald Shiner YOY")+
  theme(axis.title = element_text(size=16, face = "bold"), axis.text.y=element_text(size=16, color = "black"), 
        axis.text.x=element_text(size=16, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
        panel.background = element_blank(), text = element_text(family = "Times"),
        legend.text = element_text(size = 14, face = "bold"), legend.title = element_text(size = 14, face = "bold"), legend.key = element_blank(),
        strip.background = element_rect(fill = "white", color = "black"), strip.text = element_text(face = "bold", size = 12),
        rect = element_rect(fill = "transparent"), legend.position = "top", plot.title = element_text(color = "grey70", size = 10),
        panel.spacing.y = unit(5, "mm"))-> shiner_yoy_percent

ggsave(plot = shiner_yoy_percent, here("figures", "whole lake","percentile - shiner YOY.png"), width = 10, height = 6.5, units = "in",dpi = 600)

# shiner adult
forage_ranked %>%
  filter(species == "Emerald Shiner" & age_class == "YAO" & !is.na(mean_abundance) & year > 1999) %>%
  mutate(basin = fct_relevel(basin, "WB", "CB", "EB", "LPB")) %>%
  ggplot(aes(x = year, y = over_under_rank, fill = basin))+
  geom_col(position = "dodge")+
  facet_grid(basin ~ .)+
  scale_x_continuous(expand = c(0,0), breaks = seq(1988,this_year,by=3), limits = c(1999, this_year+1))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual("Basin", values = c("#51C1AB", "#FFD56B", "#6371C9", "#FFA56B"))+
  geom_hline(aes(yintercept = 0))+
  labs(x="", y="Percentile rank (%)", title = "Emerald Shiner adult")+
  theme(axis.title = element_text(size=16, face = "bold"), axis.text.y=element_text(size=16, color = "black"), 
        axis.text.x=element_text(size=16, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
        panel.background = element_blank(), text = element_text(family = "Times"),
        legend.text = element_text(size = 14, face = "bold"), legend.title = element_text(size = 14, face = "bold"), legend.key = element_blank(),
        strip.background = element_rect(fill = "white", color = "black"), strip.text = element_text(face = "bold", size = 12),
        rect = element_rect(fill = "transparent"), legend.position = "top", plot.title = element_text(color = "grey70", size = 10),
        panel.spacing.y = unit(5, "mm"))-> shiner_adult_percent

ggsave(plot = shiner_adult_percent, here("figures", "whole lake","percentile - shiner adult.png"), width = 10, height = 6.5, units = "in",dpi = 600)

# shad YOY
forage_ranked %>%
  filter(species == "Gizzard Shad" & age_class == "YOY" & !is.na(mean_abundance) & year > 1999) %>%
  mutate(basin = fct_relevel(basin, "WB", "CB", "EB", "LPB")) %>%
  ggplot(aes(x = year, y = over_under_rank, fill = basin))+
  geom_col(position = "dodge")+
  facet_grid(basin ~ .)+
  scale_x_continuous(expand = c(0,0), breaks = seq(1988,this_year,by=3), limits = c(1999, this_year+1))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual("Basin", values = c("#51C1AB", "#FFD56B", "#6371C9", "#FFA56B"))+
  geom_hline(aes(yintercept = 0))+
  labs(x="", y="Percentile rank (%)", title = "Gizzard Shad YOY")+
  theme(axis.title = element_text(size=16, face = "bold"), axis.text.y=element_text(size=16, color = "black"), 
        axis.text.x=element_text(size=16, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
        panel.background = element_blank(), text = element_text(family = "Times"),
        legend.text = element_text(size = 14, face = "bold"), legend.title = element_text(size = 14, face = "bold"), legend.key = element_blank(),
        strip.background = element_rect(fill = "white", color = "black"), strip.text = element_text(face = "bold", size = 12),
        rect = element_rect(fill = "transparent"), legend.position = "top", plot.title = element_text(color = "grey70", size = 10),
        panel.spacing.y = unit(5, "mm"))-> shad_yoy_percent

ggsave(plot = shad_yoy_percent, here("figures", "whole lake","percentile - shad YOY.png"), width = 10, height = 6.5, units = "in",dpi = 600)

# Goby all
forage_ranked %>%
  filter(species == "Round Goby" & !is.na(mean_abundance) & year > 1999) %>%
  mutate(basin = fct_relevel(basin, "WB", "CB", "EB", "LPB")) %>%
  ggplot(aes(x = year, y = over_under_rank, fill = basin))+
  geom_col(position = "dodge")+
  facet_grid(basin ~ .)+
  scale_x_continuous(expand = c(0,0), breaks = seq(1988,this_year,by=3), limits = c(1999, this_year+1))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual("Basin", values = c("#51C1AB", "#FFD56B", "#6371C9", "#FFA56B"))+
  geom_hline(aes(yintercept = 0))+
  labs(x="", y="Percentile rank (%)", title = "Round Goby")+
  theme(axis.title = element_text(size=16, face = "bold"), axis.text.y=element_text(size=16, color = "black"), 
        axis.text.x=element_text(size=16, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
        panel.background = element_blank(), text = element_text(family = "Times"),
        legend.text = element_text(size = 14, face = "bold"), legend.title = element_text(size = 14, face = "bold"), legend.key = element_blank(),
        strip.background = element_rect(fill = "white", color = "black"), strip.text = element_text(face = "bold", size = 12),
        rect = element_rect(fill = "transparent"), legend.position = "top", plot.title = element_text(color = "grey70", size = 10),
        panel.spacing.y = unit(5, "mm"))-> goby_percent

ggsave(plot = goby_percent, here("figures", "whole lake","percentile - goby.png"), width = 10, height = 6.5, units = "in",dpi = 600)




### Powerpoint graphs (for dark background)   #######################

library(export); library(ggpubr)
source("FTG graphs.R") # for density_by_group graph


# theme with inverse colors
inverse_theme <- function(){theme_transparent()+
    theme(legend.position = "top",
          axis.title = element_text(size=25), 
          axis.text.y=element_text(size=18, vjust=-.1), 
          axis.text.x=element_text(size=18, vjust=.1), 
          axis.line = element_line(color = "white"),
          axis.ticks = element_line(color = "white"),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank(),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18),
          strip.background = element_blank(),
          strip.text = element_text(size = 18, color = "white"),
          text = element_text(color = "white"),
          axis.text = element_text(color = "white"))}

# add each graph to PPT file - first graph overwrites, remainder append
graph2ppt(x = density_by_group + inverse_theme(), width = 10, height = 6.5, file = here("figures", "whole lake", "LEC figs.pptx"), append = FALSE)
graph2ppt(x = smelt_yoy + inverse_theme(), width = 10, height = 6.5, file = here("figures", "whole lake", "LEC figs.pptx"), append = TRUE)
graph2ppt(x = smelt_adult + inverse_theme(), width = 10, height = 6.5, file = here("figures", "whole lake", "LEC figs.pptx"), append = TRUE)
graph2ppt(x = shiner_yoy + inverse_theme(), width = 10, height = 6.5, file = here("figures", "whole lake", "LEC figs.pptx"), append = TRUE)
graph2ppt(x = shiner_adult + inverse_theme(), width = 10, height = 6.5, file = here("figures", "whole lake", "LEC figs.pptx"), append = TRUE)
graph2ppt(x = shad_yoy + inverse_theme(), width = 10, height = 6.5, file = here("figures", "whole lake", "LEC figs.pptx"), append = TRUE)
graph2ppt(x = goby_all + inverse_theme(), width = 10, height = 6.5, file = here("figures", "whole lake", "LEC figs.pptx"), append = TRUE)
graph2ppt(x = smelt_yoy_percent + inverse_theme() + geom_hline(aes(yintercept = 0), col = "white"), 
          width = 10, height = 6.5, file = here("figures", "whole lake", "LEC figs.pptx"), append = TRUE)
graph2ppt(x = smelt_adult_percent + inverse_theme() + geom_hline(aes(yintercept = 0), col = "white"), 
          width = 10, height = 6.5, file = here("figures", "whole lake", "LEC figs.pptx"), append = TRUE)
graph2ppt(x = shiner_yoy_percent + inverse_theme() + geom_hline(aes(yintercept = 0), col = "white"), 
          width = 10, height = 6.5, file = here("figures", "whole lake", "LEC figs.pptx"), append = TRUE)
graph2ppt(x = shiner_adult_percent + inverse_theme() + geom_hline(aes(yintercept = 0), col = "white"), 
          width = 10, height = 6.5, file = here("figures", "whole lake", "LEC figs.pptx"), append = TRUE)
graph2ppt(x = shad_yoy_percent + inverse_theme() + geom_hline(aes(yintercept = 0), col = "white"), 
          width = 10, height = 6.5, file = here("figures", "whole lake", "LEC figs.pptx"), append = TRUE)
graph2ppt(x = goby_percent + inverse_theme() + geom_hline(aes(yintercept = 0), col = "white"), 
          width = 10, height = 6.5, file = here("figures", "whole lake", "LEC figs.pptx"), append = TRUE)

