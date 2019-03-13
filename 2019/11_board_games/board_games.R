library(tidyverse)
library(lubridate)
library(ggthemes)

board_games <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv")

board_games %>% 
  count(year_published) %>% 
  mutate(cumulative = cumsum(n)) %>%
  ggplot(aes(year_published, cumulative)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1950, 2020, 10)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Year",
       y = "Cumulative No. of Board Games") +
  theme_fivethirtyeight()
  
# The total number of board games has increased exponentially over the years
# What about the mechanics of board games. How have those changed over time?

board_games %>% 
  count(year_published, mechanic) %>% 
  drop_na(mechanic) %>% 
  mutate(n_comma = str_count(mechanic, ",")) %>% 
  arrange(desc(n_comma))

# Looks like we need to slip up the nested categories
mechanics <- board_games %>% 
  select(mechanic) %>% 
  mutate(mechanic = strsplit(mechanic, ",")) %>% 
  unnest(mechanic) %>% 
  unique() %>% 
  na.omit() %>% 
  pull(mechanic)

# Count nested categories
mechanics_count <- 
  board_games %>% 
  select(year_published, mechanic) %>% 
  drop_na(mechanic) %>% 
  mutate(mechanic = strsplit(mechanic, ",")) %>% 
  unnest(mechanic)
  # mutate(count = 1) %>% 
  # count(year_published, mechanic) %>% 
  # mutate(year_published = as.character(year_published),
  #        year = ymd(str_c(year_published, "01-01")))

# Find 5 most popular mechanics
top_mechanics <- 
  mechanics_count %>% 
  count(mechanic, sort = T) %>% 
  top_n(n = 6, wt = n) %>% 
  pull(mechanic)

# column plot
mechanics_count %>%
  filter(mechanic %in% top_mechanics) %>% 
  ggplot(aes(year, n)) +
  geom_col(col = "white") +
  facet_wrap(~mechanic, ncol = 3) +
  theme_fivethirtyeight()

# histogram plot
mechanics_count %>% 
  filter(mechanic %in% top_mechanics) %>% 
  mutate(mechanic = factor(mechanic, levels = top_mechanics)) %>% 
  ggplot(aes(year_published, fill = mechanic)) +
  geom_density(col = "grey90", show.legend = F) +
  facet_wrap(~mechanic, ncol = 3) +
  scale_x_continuous(breaks = seq(1960, 2010, 10)) +
  scale_fill_tableau() +
  labs(title = "Most Occuring Board Game Mechanics over the years") +
  theme_fivethirtyeight() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90))
