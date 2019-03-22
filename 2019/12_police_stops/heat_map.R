# Inspired by the tutorial by Stanford researchers
library(tidyverse)
library(lubridate)
library(gridExtra)
stops <- read_rds("2019/12_police_stops/tr137st9964_pa_philadelphia_2019_02_25.rds")

# Get an even sample from whites and others groups
set.seed(24)
stops_sample <- 
  stops %>% 
  mutate(is_white = if_else(subject_race == "white", T, F),
         month = month(date, label = T)) %>% 
  filter(month %in% c("Jul","Aug")) %>%
  group_by(is_white) %>% 
  sample_n(50000) %>% 
  ungroup()

week_counts <- 
  stops_sample %>% 
  select(date, time, is_white) %>% 
  drop_na() %>% 
  mutate(hour     = hour(time) %>% factor(),
         hour_reg = hour(time),
         week_day = wday(date, label = T) %>% factor(),
         hour     = fct_rev(hour))

heat_map_minorities <- 
  week_counts %>% 
  filter(is_white == FALSE) %>% 
  count(week_day, hour) %>% 
  ggplot(aes(week_day, hour, fill = n)) +
  geom_raster(show.legend = F) +
  scale_fill_gradient(low = "lightsteelblue1", high = "#003366") +
  scale_x_discrete(position = "top") +
  coord_equal() +
  labs(x = "Minorities",
       y = NULL,
       title = "Philadelphia Police Stops") +
  theme_minimal() +
  theme(plot.margin = margin(t = .3, r = -0.5, b = .72, l = -1.5, unit = "cm"),
        axis.text.x.top = element_text(angle = 90),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 1))

hours_am_pm <- c("12 AM", 
                 paste(1:11,"AM"),
                 "12 PM",
                 paste(1:11,"PM"))

heat_map_white <- 
  week_counts %>%  
  filter(is_white == TRUE) %>% 
  count(week_day, hour) %>% 
  ggplot(aes(week_day, hour, fill = n)) +
  geom_raster(show.legend = F) +
  scale_fill_gradient(low = "lightsteelblue1", high = "#003366") +
  scale_x_discrete(position = "top") +
  scale_y_discrete(breaks = seq(0,23,1),
                   labels = hours_am_pm) +
  coord_equal() +
  labs(x = "White",
       y = NULL) +
  theme_minimal() +
  theme(plot.margin = margin(t = .95, r = -0.5, b = .72, l = -5.5, unit = "cm"),
        axis.text.x.top = element_text(angle = 90),
        panel.grid = element_blank())

  
med_white <- 
  week_counts %>% 
  filter(is_white == TRUE) %>% 
  pull(hour_reg) %>% 
  median()

med_black <- 
  week_counts %>% 
  filter(is_white == FALSE) %>% 
  pull(hour_reg) %>% 
  median() 

white_density <- 
  week_counts %>% 
  filter(is_white == TRUE) %>% 
  count(hour) %>%
  ggplot(aes(hour, n)) + 
  geom_col(fill  = "#003366", 
           width = 1, 
           alpha = .8) +
  geom_segment(aes(x = med_white-6, xend = med_white-6, y = 0, yend = 4000),
               col  = "white",
               size = 1) +
  coord_flip(ylim = c(0,5000)) +
  scale_y_continuous(labels = scales::comma,
                     breaks = seq(0,5000,1000)) +
  labs(x = NULL,
       y = NULL,
       title = NULL) +
  theme_minimal() +
  theme(panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.margin        = margin(t = 2.3, r = 3, b = .2, l = -3.68, unit = "cm"),
        axis.text.y        = element_blank())

minorities_density <- 
  week_counts %>% 
  filter(is_white == FALSE) %>% 
  count(hour) %>% 
  ggplot(aes(hour,n)) +
  geom_col(fill  = "#003366", 
           width = 1, 
           alpha = .8) +
  geom_vline(xintercept = med_black-10,
             col  = "white",
             size = 1) +
  annotate(geom  = "text",
           x     = 7.7,
           y     = 1300,
           label = "Median",
           col   = "white",
           size  = 4) +
  scale_y_reverse(labels = scales::comma) +
  coord_flip() +
  labs(title = NULL,
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.margin        = margin(t = 2.28, r = -1.195, b = .2, l = .5, unit = "cm"),
        axis.text.y        = element_blank()
        )

grid.arrange(minorities_density,
             heat_map_minorities,
             heat_map_white,
             white_density,
             nrow = 1)
