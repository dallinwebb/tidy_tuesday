library(tidyverse)

# .png of burger 
g <- png::readPNG(source = "burger.png") %>% grid::rasterGrob(interpolate = T)

# One pipeline 
df <- read_rds("fastfood_calories.rds") %>% 
  as_tibble() %>% 
  select(restaurant, item, calories) %>%
  mutate(is_over = ifelse(calories > 500, T, F)) %>% 
  group_by(restaurant, is_over) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n)) %>% 
  filter(!is_over == F) %>%
  ggplot(aes(fct_reorder(restaurant, freq, max, .desc = T), 
             weight = freq,
             fill = restaurant)) +
  geom_bar(show.legend = F) +
  geom_text(aes(y     = freq + .02, 
                label = scales::percent(freq))) +
  annotation_custom(g, 
                    ymin = .4,
                    ymax = .8,
                    xmin = 7,
                    xmax = 8) +
  scale_fill_manual(values = c("Mcdonalds"   = "#36454a",      "Arbys" = "#638c94",
                               "Burger King" = "#79ada0",      "Sonic" = "#adb563",
                               "Dairy Queen" = "#e7a521",     "Subway" = "#e78418",
                               "Taco Bell"   = "#ee5928","Chick Fil-A" = "#e73a3f")) +
  labs(x = NULL, 
       y = NULL,
       title = "Percentage of Menu Items Over 500 Calories\n\n") +
  theme_minimal() +
  theme(panel.grid  = element_blank(),
        axis.text.y = element_blank(),
        title       = element_text(size = 16),
        axis.text.x = element_text(size = 12))
