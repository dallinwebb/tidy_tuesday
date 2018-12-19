library(tidyverse)
library(ggridges)
maj <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-10-16/recent-grads.csv")

# Bar Chart of means
maj %>% 
  group_by(Major_category) %>% 
  summarise(m = mean(Unemployment_rate)) %>%
  ggplot(aes(reorder(Major_category,m, max), weight = m)) +
  geom_bar(show.legend = F) +
  geom_text(aes(y = m + .004, label = scales::percent(m))) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(x     = NULL,
       y     = "\n Unemployment Rate",
       title = "Unemployment by Major Categories") +
  theme_minimal() +
  theme(panel.grid = element_blank())


# Boxplot ordered by mean
maj %>%
  ggplot(aes(fct_reorder(Major_category, Unemployment_rate, mean), Unemployment_rate)) +
  geom_boxplot(fill = "grey",
               show.legend    = F,
               outlier.colour = "white") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(x     = NULL,
       y     = "\n Unemployment Rate",
       title = "Unemployment by Major Categories",
       subtitle = "Ordered by mean") +
  theme_light() +
  theme(panel.grid   = element_blank(),
        panel.border = element_blank())


# Boxplot ordered by median
maj %>%
  ggplot(aes(fct_reorder(Major_category, Unemployment_rate, median), Unemployment_rate)) +
  geom_boxplot(fill = "grey",
               show.legend    = F,
               outlier.colour = "white") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(x     = NULL,
       y     = "\n Unemployment Rate",
       title = "Unemployment by Major Categories",
       subtitle = "Ordered by median") +
  theme_light() +
  theme(panel.grid   = element_blank(),
        panel.border = element_blank())
