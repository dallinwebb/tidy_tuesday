library(tidyverse)
phd_field <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv")

phd_field %>% 
  group_by(major_field, year) %>% 
  summarise(n_phds = sum(n_phds, na.rm = T)) %>% 
  ggplot(aes(year, n_phds, col = major_field)) +
  geom_line(show.legend = F) +
  theme_minimal()

phd_field %>% 
  select(field, year, n_phds) %>% 
  group_by(year) %>% 
  arrange(year, desc(n_phds)) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank <= 10) %>% 
  ggplot(aes(year, rank, group = field)) +
  geom_line() +
  geom_point()
