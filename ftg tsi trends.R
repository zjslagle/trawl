source("ftg whole lake trends.R")

# read TSi data (medians)

tsi_data <- read_excel("data sources/FTG TSI data.xlsx", na = c("NA"))

# add TSI targets
tsi_targets <- tibble("trophic_state" = c("Oligotrophic",
                                          "Mesotrophic",
                                          "Eutrophic",
                                          "Hypereutrophic"),
                      "tsi_value" = c(36.5,
                                      45.5,
                                      59.2,
                                      65), # technically, hyper is > 59.2; this value is just to get the label above
                      "x_value" = c(2010,
                                    2010,
                                    2007,
                                    2005)) 


# make graphs
tsi_plot <- tsi_data %>%
  ggplot(aes(x = year, y = tsi_median, color = basin))+
    geom_hline(data = tsi_targets,
               aes(yintercept = tsi_value),
               color = "grey70",
               size = 3,
               lty = 3)+
    geom_line(lwd = 2)+
    scale_x_continuous(expand = c(0,.5), 
                       breaks = seq(2000, this_year, by=5), 
                       limits = c(2000, this_year))+
    scale_y_continuous(expand = c(0,0), 
                       limits = c(30, 60))+
    geom_text(data = tsi_targets,
               aes(x = x_value, 
                   y = tsi_value,
                   label = trophic_state),
               size = 6,
               color = "black",
               face = "bold",
               nudge_y = -3)+
    scale_color_manual("Basin", values = c("#51C1AB", "#FFD56B", "#6371C9", "#FFA56B"))+
    labs(x="", y="Trophic state index")+
    theme(axis.title = element_text(size=16, face = "bold"), axis.text.y=element_text(size=16, color = "black"), 
          axis.text.x=element_text(size=16, vjust=1, color = "black"), axis.line = element_line(color = "black"), 
          panel.background = element_blank(), text = element_text(family = "Times"),
          legend.text = element_text(size = 14, face = "bold"), legend.title = element_text(size = 14, face = "bold"), legend.key = element_blank(),
          strip.background = element_rect(fill = "white", color = "black"), strip.text = element_text(face = "bold", size = 12),
          rect = element_rect(fill = "transparent"), legend.position = c(.9, .9), plot.title = element_text(color = "grey70", size = 10))

tsi_plot

ggsave(plot = tsi_plot, "figures/whole lake/TSI SOLE plot.png", width = 10, height = 6.5, units = "in",dpi = 600)

graph2ppt(x = tsi_plot + 
            inverse_theme(), width = 10, height = 6.5, 
          file = "figures/whole lake/LEC TSI fig.pptx", append = FALSE)
