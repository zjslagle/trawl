source("spatial_1 - make simple features.R")
require(gganimate)
require(gifski)
library(magick)
require(beepr)

# Combined animation - Map of IA Walleye catch per site combined with bar graph index
# Run this on personal laptop, may not work on work laptop

### Animated Map of walleye catch per site over years - Interagency Index   #############
#read in data
wae_site_data <- read_csv("data derived/Interagency CPHA by site.csv") %>%
  filter(year > 1987, agecode == 1, species == 334) %>%
  left_join(all_effort %>% select(yrsam, latitude, longitude))

# Static Map Setup 
wae_map <- ggplot(data = wae_site_data)+          # CHANGED THIS
  scale_x_continuous(limits = c(wb_bounds["xmin"], wb_bounds["xmax"]))+
  scale_y_continuous(limits = c(wb_bounds["ymin"], wb_bounds["ymax"]))+
  geom_sf(data = us_and_can, fill = "lightyellow2")+
  geom_sf(data = lake_erie, fill = "#c5dfed")+
  geom_point(aes(x = longitude, y = latitude,
                 size = cpha, fill = cpha, group = year), 
             color = "black", shape = 21)+
  geom_sf(data = state_border, lty = 3)+
  geom_label(aes(x = -83.4, y = 42.12), label = "{closest_state}",
             size = 7, fontface = "bold")+
  scale_size(name = "Fish per\nhectare",
             range = c(.3, 10))+
  scale_fill_gradientn(name = "Fish per\nhectare",
                       colours = c('white', "#ffc500", "orangered"),
                       values = c(0, .2, 1))+
  guides(fill = guide_legend(), size = guide_legend())+ #combines legends
  ggsn::scalebar(dist = 15, model = "WGS84",location = "topright", dist_unit = "km", 
                 x.min = wb_bounds["xmin"], y.min = wb_bounds["ymin"], 
                 x.max = wb_bounds["xmax"], y.max = wb_bounds["ymax"],
                 st.dist = .03, anchor = c(x = -83.2, y = 41.4), transform = TRUE,
                 border.size = .5, st.size = 4)+
  ggsn::north(x.min = wb_bounds["xmin"], y.min = wb_bounds["ymin"], 
              x.max = wb_bounds["xmax"], y.max = wb_bounds["ymax"],
              location = "topright", symbol = 3, anchor = c(x = -83.32, y = 41.5))+
  labs(title = "Interagency Western Basin Young-of-year Walleye Index")+
  theme_minimal()+
  theme(axis.text = element_blank(), axis.title = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.title.position = "plot",
        legend.text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.title.align = 0.5)

#wae_map

# save static map - use this to fine tune edits for map appearance
ggsave(wae_map,
       filename = "figures/animations/test - age-0 Walleye map.png",
       width=7, height=5, units="in", dpi=400, pointsize=12)

### Animated map   #####################################################
# set up animation variables - used across multiple graphs
trans_len = 2                                             
state_len = 2
fps = 10
nframes = 250
duration = 25
start_pause = 5
end_pause = 0

#n_frames = (fps*duration)+start_pause+end_pause

# initialize animation
wae_animation <- wae_map+
  transition_states(year, 
                    transition_length = trans_len, 
                    state_length = state_len,
                    wrap = FALSE) +
  ease_aes('cubic-in-out')+
  enter_fade()+ 
  exit_shrink()

# Animate! this will take a couple of minutes
wae_map_gif <- animate(wae_animation,
                 fps = fps,
                 #duration = duration,
                 detail = 2,
                 nframes = nframes,
                 start_pause = start_pause,
                 width=7, height=5, units="in", res = 400,
                 renderer = magick_renderer(loop = FALSE)); beep(2)

#Save animation - DON'T FEED UNNECC. ARGUMENTS, there's a bug!
anim_save("figures/animations/animated wae map.gif") 
#unique(attr(wae_map_gif, "frame_vars")$nframes) #use to doublecheck 

### Bargraph - annual index    ################################################
yearly_abundance_tall <- read_csv("data derived/West Basin absolute abundance tall format.csv")

wae_index <- yearly_abundance_tall %>%
  filter(year > 1987, agecode == 1, species == 334)

wae_bar <- wae_index %>%
  ggplot(aes(x = year, y = abundance_per_ha, group = year))+
  geom_col(color = "black", fill = "orangered")+
  scale_x_continuous(expand = c(0,.5), breaks = seq(1988, this_year, by=5))+
  scale_y_continuous(expand = c(0,0))+
  labs(x="", y="Fish/ha",
       caption = paste0("Data from OMNRF and ODNR-DOW August trawl surveys, 1988–", this_year, "."))+
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white", color = NA),
        axis.title = element_text(size=16, face = "bold"), 
        axis.text.y=element_text(size=16, color = "black", face = "bold"), 
        axis.text.x=element_text(size=16, vjust=-1, color = "black", face = "bold"), 
        axis.line = element_line(color = "black"),
        plot.caption = element_text(size = 14, hjust = -.05),
        plot.caption.position = "plot")
  
ggsave(wae_bar,
       filename = "figures/animations/test age-0 Walleye bargraph.png",
       width=7, height=2, units="in", dpi=400, pointsize=12)

### Animated bargraph     #############################

bar_animation <- wae_bar+
  transition_states(year, 
                    transition_length = trans_len, 
                    state_length = state_len,
                    wrap = FALSE)+
  ease_aes('cubic-in-out')+
  enter_grow()+
  shadow_mark() #keep bars in

wae_bar_gif <- animate(bar_animation,
                       fps = fps,
                       #duration = duration,
                       detail = 2,
                       nframes = nframes,
                       start_pause = start_pause,
                       width=7, height=2, units="in", res = 400,
                       renderer = magick_renderer(loop = FALSE)); beep(2) 

anim_save("figures/animations/animated wae bargraph.gif")


unique(attr(wae_bar_gif, "frame_vars")$nframes)

### Combine both   ######################
# follows https://www.connorrothschild.com/post/tidy-tuesday-powerlifting

# try image_quantize() to reduce number of colors

length(wae_map_gif); length(wae_bar_gif)

new_gif <- image_append(c(wae_map_gif[1], wae_bar_gif[1]), stack = TRUE)
for(i in 2:length(wae_map_gif)){
  combined <- image_append(c(wae_map_gif[i], wae_bar_gif[i]), stack = TRUE)
  new_gif <- c(new_gif, combined)
}

image_write(new_gif, path = "figures/animations/combined GIF.gif", format = "gif"); beep(2)
