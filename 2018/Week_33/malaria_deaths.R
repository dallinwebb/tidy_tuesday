library(tidyverse)
library(janitor)
library(magrittr)
mal <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-11-13/malaria_deaths.csv")

mal %>% 
  clean_names() %>% 
  rename(deaths = deaths_malaria_sex_both_age_age_standardized_rate_per_100_000_people) %>% 
  select(-code) %>% 
  ggplot(aes(year, deaths, color = entity )) +
  geom_line(show.legend = F)
