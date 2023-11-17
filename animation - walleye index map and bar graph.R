source("spatial_1 - make simple features.R")
#source("2_abundance summary.R")
require(gganimate)
require(gifski)
library(magick)

### Animated Map of walleye catch over years      #############

wae_site_data <- read_csv("data derived/Interagency CPHA by site.csv") %>%
  filter(year > 1987, agecode == 1, species == 334) %>%
  left_join(all_effort %>% select(yrsam, latitude, longitude))

# Map Setup - catch by site    ######################################
wae_map <- ggplot(data = wae_site_data)+          # CHANGED THIS
  scale_x_continuous(limits = c(wb_bounds["xmin"], wb_bounds["xmax"]))+
  scale_y_continuous(limits = c(wb_bounds["ymin"], wb_bounds["ymax"]))+
  geom_sf(data = us_and_can, fill = "lightyellow2")+
  geom_sf(data = lake_erie, fill = "#c5dfed")+
  geom_point(aes(x = longitude, y = latitude,
                 size = cpha, fill = cpha, group = year), 
             color = "black", shape = 21)+
  geom_sf(data = state_border, lty = 3)+
  scale_size(name = "Fish per hectare",
             range = c(.3, 10))+
  scale_fill_gradientn(name = "Fish per hectare",
                       colours = c('white', "#ffc500", "orangered"),
                       values = c(0, .2, 1))+
  guides(fill = guide_legend(), size = guide_legend())+ #combines legends
  ggsn::scalebar(dist = 15, model = "WGS84",location = "topright", dist_unit = "km", 
                 x.min = wb_bounds["xmin"], y.min = wb_bounds["ymin"], 
                 x.max = wb_bounds["xmax"], y.max = wb_bounds["ymax"],
                 st.dist = .03, anchor = c(x = -83.2, y = 41.4), transform = TRUE,
                 border.size = .5, st.size = 3)+
  ggsn::north(x.min = wb_bounds["xmin"], y.min = wb_bounds["ymin"], 
              x.max = wb_bounds["xmax"], y.max = wb_bounds["ymax"],
              location = "topright", symbol = 3, anchor = c(x = -83.33, y = 41.5))+
  labs(title = "{closest_state} Young-of-year Walleye Index")+
  theme_minimal()+
  theme(axis.text = element_blank(), axis.title = element_blank(),
        plot.background = element_rect(fill = "white", color = NA))

wae_map

ggsave(wae_map,
       filename = "figures/animations/age-0 Walleye test map.png",
       width=7, height=5, units="in", dpi=400, pointsize=12)

### Animated map    ###############################
# set up animation variables - used across multiple graphs
trans_len = 2                                             # CHANGED THIS
state_len = 2
fps = 10
nframes = 250
duration = 25
start_pause = 5
end_pause = 0

#n_frames = (fps*duration)+start_pause+end_pause

wae_animation <- wae_map+
  transition_states(year, 
                    transition_length = trans_len, 
                    state_length = state_len,
                    wrap = FALSE) +
  ease_aes('cubic-in-out')+
  enter_fade()+ 
  exit_shrink()

wae_map_gif <- animate(wae_animation,
                 fps = fps,
                 #duration = duration,
                 detail = 2,
                 nframes = nframes,
                 start_pause = start_pause,
                 width=7, height=5, units="in", res = 400,
                 renderer = magick_renderer(loop = FALSE)); beep(2)

anim_save("figures/animations/animated wae map.gif", animation = wae_map_gif)

unique(attr(wae_map_gif, "frame_vars")$nframes)

### Bargraph - annual index    ################################################

wae_index <- yearly_abundance_tall %>%
  filter(year > 1987, agecode == 1, species == 334)

wae_bar <- wae_index %>%
  ggplot(aes(x = year, y = abundance_per_ha, group = year))+
  geom_col(color = "black", fill = "orangered")+
  scale_x_continuous(expand = c(0,.5), breaks = seq(1988, this_year, by=5))+
  scale_y_continuous(expand = c(0,0))+
  labs(x="", y="Fish/ha")+
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white", color = NA),
        axis.title = element_text(size=14, face = "bold"), 
        axis.text.y=element_text(size=14, color = "black"), 
        axis.text.x=element_text(size=14, vjust=-1, color = "black"), 
        axis.line = element_line(color = "black"))
  
ggsave(wae_bar,
       filename = "figures/animations/age-0 Walleye test bargraph.png",
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

anim_save("figures/animations/animated wae bargraph.gif", animation = wae_bar_gif)


unique(attr(wae_bar_gif, "frame_vars")$nframes)

### Combine both   ######################
# follows https://www.connorrothschild.com/post/tidy-tuesday-powerlifting

length(wae_map_gif); length(wae_bar_gif)

new_gif <- image_append(c(wae_map_gif[1], wae_bar_gif[1]), stack = TRUE)
for(i in 2:length(wae_map_gif)){
  combined <- image_append(c(wae_map_gif[i], wae_bar_gif[i]), stack = TRUE)
  new_gif <- c(new_gif, combined)
}

image_write(new_gif, path = "figures/animations/combined GIF.gif", format = "gif"); beep(2)
