library(tidyverse)
tv <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv")

fav_shows <- c("24","Prison Break","Sherlock","Stranger Things")

is_returned <- tv %>% 
  filter(seasonNumber <= 2) %>% 
  group_by(title) %>% 
  nest(seasonNumber) %>% 
  ungroup() %>% 
  mutate(nrow = map_dbl(data, sum)) %>% 
  arrange(desc(nrow)) %>% 
  filter(nrow == 3) %>% 
  pull(title)

sequal <- tv %>% 
  filter(title %in% is_returned, 
         seasonNumber <= 2) %>% 
  spread(seasonNumber, av_rating) %>% 
  group_by(title) %>% 
  fill(`2`, .direction = "up") %>% 
  ungroup() %>% 
  drop_na(`1`) %>% 
  rename(season_1_av = `1`, season_2_av = `2`) %>% 
  mutate(av_diff = season_2_av - season_1_av)

top_5    <- sequal %>% top_n( 10, av_diff)
bottom_5 <- sequal %>% top_n(-10, av_diff)

top_5 %>% 
  bind_rows(bottom_5) %>% 
  select(title, av_diff) %>% 
  mutate(title = fct_reorder(title, av_diff, max),
         color = if_else(av_diff > 0, "#1C86EE", "#EE3B3B")) %>% 
  ggplot(aes(title, av_diff)) +
  geom_segment(aes(x = title, xend = title, y = 0, yend = av_diff), 
               col = "grey70", size = 1) +
  geom_point(aes(col = color), size = 3, show.legend = F) +
  coord_flip() +
  geom_hline(yintercept = 0) +
  geom_rect(aes(xmin = 10.2, xmax = 10.8, ymin = -.2, ymax = .2), 
            fill = "white") +
  geom_text(aes(x = 10.7, y = 0, label = "...")) +
  labs(x = NULL,
       y = "(Season 2 Avg. Rating - Season 1 Avg. Rating)",
       title = "Change in Average Rating at Season 2") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())
