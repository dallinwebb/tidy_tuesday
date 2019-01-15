library(tidyverse)
library(gganimate)
agen <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/2019/2019-01-15/agencies.csv")
laun <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/2019/2019-01-15/launches.csv")

rank_launch <- laun %>%
  filter(category == "O") %>% 
  count(agency, launch_year) %>% 
  group_by(launch_year) %>% 
  mutate(rank = rank(-n, ties.method = "first") - 1) %>% 
  ungroup() %>% 
  arrange(launch_year) %>% 
  drop_na(agency)

p <- ggplot(rank_launch, aes(rank, group = agency, fill = agency)) +
  geom_tile(aes(y = n/2, height = n, width = .8),
            alpha = .8, show.legend = F) +
  geom_text(aes(y = 0, label = paste(agency, " ")), vjust = .2, hjust = 1) +
  coord_flip(clip = "off", expand = T) +
  scale_y_continuous(breaks = seq(0, 70, 10)) +
  scale_x_reverse() +
  labs(title = 'Year: {closest_state}',
       x = NULL,
       y = "\nNumber of Successful Launches") +
  theme_light() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1,1,1,1, "cm")) +
  transition_states(launch_year, 
                    transition_length = 3, 
                    state_length = 1) +
  ease_aes('cubic-in-out')

animate(p, fps = 25, duration = 45, width = 800, height = 600)

#https://stackoverflow.com/questions/53162821/animated-sorted-bar-chart-with-bars-overtaking-each-other/53163549
