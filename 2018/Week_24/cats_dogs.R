library(tidyverse)
library(geofacet)
df <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/2018-09-11/cats_vs_dogs.csv")

df %>% select(state, starts_with("percent"),-percent_pet_households) %>% 
  mutate("No Pets" = 100 - (percent_dog_owners + percent_cat_owners)) %>% 
  rename("Dog Owners" = percent_dog_owners,
         "Cat Owners" = percent_cat_owners) %>% 
  gather(var, val, -state) %>% 
  arrange(state) %>%
  mutate(val = val/100) %>% 
  ggplot(aes(var, val, fill = var)) +
  geom_col() +
  geom_text(aes(y = val + .2, label = scales::percent(val,accuracy = 1)),size = 3) +
  scale_fill_manual(values = c("#336B62", "#5D405F", "#CDCDCD")) +
  facet_geo(~ state, grid = "us_state_grid2") +
  theme_bw() +
  coord_flip() +
  labs(title = "Pet Preference in U.S.",
       caption = "Data Source: https://data.world/datanerd/cat-vs-dog-popularity-in-u-s",
       x = NULL,
       y = "Percentage of Pet Ownership Type",
       fill = NULL) +
  theme(strip.text.x = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
