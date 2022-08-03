source("2_abundance summary.R")

# Plot 2021 catches, highlighting site 253 (hypoxic-induced high catch)
all_catch %>%
  filter(year == 2021, agecode == 1) %>%
  filter(species %in% c(301, 334)) %>%
  mutate(outlier = if_else(crn == 253, "outlier", ""), 
         common = case_when(species == 301 ~"YOY White Perch", 
                            species == 331 ~ "YOY Yellow Perch", 
                            T ~ "YOY Walleye")) %>%
  ggplot(aes(x = catch, fill = outlier))+
    geom_dotplot(dotsize = 2)+
    facet_wrap(common ~ ., scales = "free_x")+
    labs(title = "2021 Interagency Catches",
         subtitle = "One outlier site (253) for selected species shown in teal",
         x = "Catches", y = "Count")+
    inverse_theme()+
    scale_fill_manual(values = c("#ef8a62", "#67a9cf"))+
    theme(legend.position = "none")

ggsave("figures/outlier catch 2021/outlier plot dark.png",
       width = 10, height = 6.5, units = "in",dpi = 600)

# top catches, all time
all_catch %>%
  #filter(species == 334) %>%
  arrange(-catch)

