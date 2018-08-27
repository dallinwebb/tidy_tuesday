library(tidyverse)
library(ggthemes)

fire <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week21/week21_calfire_frap.csv")
# Data dictionary http://frap.fire.ca.gov/projects/fire_data/fire_perimeters_data_description

fire %>% 
  select(year_, state, alarm_date, gis_acres) %>% 
  rename(year = year_) %>% 
  filter(state == "CA") %>% 
  mutate(month    = month(alarm_date, label = T),
         year_cut = cut(year, breaks = c(1949, 1959, 1969, 1979, 
                                         1989, 1999, 2009, 2017),
                              labels = c(seq(1950,2010, 10)))) %>% 
  group_by(year_cut, month) %>%
  summarise(n = n(),
            acres = sum(gis_acres),
            acres_log = log10(sum(gis_acres))) %>% 
  filter(!is.na(month)) %>% 
ggplot(aes(month, fct_rev(year_cut), fill = n)) +
  geom_tile(show.legend = T) +
  # geom_text(data = . %>% filter(!(n %in% c(727, 684, 622))),
  #           aes(label = n),
  #           col = "grey80",
  #           nudge_x = -.33, 
  #           nudge_y = -.33,
  #           size = 2.5) +
  # geom_text(data = . %>% filter(n %in% c(727, 684, 622)),
  #           aes(label = n),
  #           col = "black",
  #           nudge_x = -.33, 
  #           nudge_y = -.33,
  #           size = 2.5) +
  scale_fill_viridis_c(option = "inferno") +
  labs(x = NULL, 
       y = NULL, 
       title = "California Fires",
       fill = "Count") +
  theme_minimal() +
  theme(panel.grid      = element_blank(),
        plot.background = element_rect(fill = "grey20"),
        text            = element_text(size = 24, 
                                       color = "grey85"),
        axis.text       = element_text(size = 20, 
                                       color = "grey60"))
