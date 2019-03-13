library(tidyverse)
library(lubridate)
tv <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv")

# Find all TV shows that came back for a second season
is_returned <- tv %>% 
  filter(seasonNumber <= 2) %>% 
  group_by(title) %>% 
  nest(seasonNumber) %>% 
  ungroup() %>% 
  mutate(nrow = map_dbl(data, sum)) %>% 
  arrange(desc(nrow)) %>% 
  filter(nrow == 3) %>% 
  pull(title)

# Return all unique genres
genres_vec <- tv %>% 
  select(genres) %>% 
  mutate(genres = strsplit(genres, ",")) %>% 
  unnest(genres) %>% 
  unique() %>% 
  pull(genres)

# Wrangle dataset
sequal <- tv %>% 
  filter(title %in% is_returned, 
         seasonNumber <= 2) %>% 
  spread(seasonNumber, av_rating) %>% 
  group_by(title) %>% 
  fill(`2`, .direction = "up") %>% 
  ungroup() %>% 
  drop_na(`1`) %>% 
  rename(season_1_av = `1`, season_2_av = `2`) %>% 
  mutate(av_diff        = season_2_av - season_1_av,
         is_increase    = av_diff > 0,
         is_decrease    = av_diff < 0,
         is_drama       = str_detect(genres, "Drama"),
         is_mystery     = str_detect(genres, "Mystery"),
         is_sci_fi      = str_detect(genres, "Sci-Fi"),
         is_adventure   = str_detect(genres, "Adventure"),
         is_action      = str_detect(genres, "Action"),
         is_crime       = str_detect(genres, "Crime"),
         is_fantasy     = str_detect(genres, "Fantasy"),
         is_family      = str_detect(genres, "Family"),
         is_romance     = str_detect(genres, "Romance"),
         is_comedy      = str_detect(genres, "Comedy"),
         is_thriller    = str_detect(genres, "Thriller"),
         is_biography   = str_detect(genres, "Biography"),
         is_horror      = str_detect(genres, "Horror"),
         is_music       = str_detect(genres, "Music"),
         is_sport       = str_detect(genres, "Sport"),
         is_history     = str_detect(genres, "History"),
         is_documentary = str_detect(genres, "Documentary"),
         is_animation   = str_detect(genres, "Animation"),
         is_western     = str_detect(genres, "Western"),
         is_war         = str_detect(genres, "War"),
         is_reality_tv  = str_detect(genres, "Reality-TV"),
         is_musical     = str_detect(genres, "Musical")
         ) %>%
  select(-genres)

# Example of effect of second season rating of one genre
ggplot(sequal, aes(av_diff, fill = is_action)) +
  geom_vline(xintercept = 0) +
  geom_density(col = "white", alpha = .6)

# Differnece in means
sequal %>% group_by(is_comedy) %>% summarise(mean = mean(av_diff))

ggplot(sequal %>% filter(date > date("2017-01-01")), aes(season_1_av, fill = "season 1")) + 
  geom_density(col = "white", alpha = .6) +
  geom_density(aes(season_2_av, fill = "season 2"), 
               col = "white", alpha = .6)

sequal %>% 
  gather(genre, yes_no, is_drama:is_musical) %>% 
  ggplot(aes(av_diff, genre, fill = yes_no)) + 
  geom_density_ridges(alpha = .3)