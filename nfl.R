library(tidyverse)
library(XML)
df <- read_csv("https://github.com/rfordatascience/tidytuesday/blob/master/data/2018-08-28/nfl_2010-2017.csv?raw=true")
#http://www.espn.com/nfl/news/story?id=2128923

pos <- readHTMLTable(doc = url, which = 4, as.data.frame = T) %>% 
  rename(Abbreviation = V1, Position = V2) %>% 
  slice(-1)

df <- df %>% 
  left_join(pos, by = c("position" = "Abbreviation"))

df %>% 
  group_by(game_year, game_week, team) %>% 
  summarise(rush_yds = mean(rush_yds, na.rm = T),
            pass_yds = mean(pass_yds, na.rm = T)) %>% 
  group_by(game_year, game_week) %>% 
  summarise(m = mean(rush_yds),
            n = mean(pass_yds)) %>% 
  ggplot(aes(n,m)) +
  geom_point()
