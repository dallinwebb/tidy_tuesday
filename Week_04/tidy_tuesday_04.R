library(tidyverse)
library(ggalt)

read_csv("ausi_salaries_gender.csv") %>% 
  select(-c(individuals, X1, gender_rank)) %>% 
  spread(gender, average_taxable_income) %>% 
  mutate(diff = Male - Female) %>% 
  arrange(desc(diff)) %>% 
  top_n(10) %>% 
  ggplot() +
    geom_dumbbell(aes(y = reorder(occupation, Male), x = Female, xend = Male),
                  colour_x = "pink",
                  colour_xend = "blue",
                  size = .75,
                  size_x = 3,
                  size_xend = 3,
                  show.legend = T) +
    scale_x_continuous(labels = scales::dollar) +
    coord_cartesian(xlim = c(0,650000)) +
    labs(x = "\nWage Gap",
         y = "Occupation",
         title = "Top 10 Gender Income Gap Occupations in Australia (2013-14)",
         subtitle = "Ophthalmologists see largest income gap",
         caption = "Source: data.gov.au") +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())