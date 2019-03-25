library(tidyverse)
library(lubridate)
suppressPackageStartupMessages(library(genderizeR))

pets_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-26/seattle_pets.csv")

pets <- 
  pets_raw %>% 
  mutate(license_issue_date = mdy(license_issue_date),
         name = str_to_lower(animals_name)) %>% 
  filter(license_issue_date >= "2015-01-01") %>% 
  drop_na(animals_name)

# 9:13 AM
# Gives us name, gender, prob, count
names_to_join <- 
  babynames::babynames %>% 
  select(sex, name) %>%  
  mutate(name = str_to_lower(name)) %>% 
  distinct(name, .keep_all = T)

joined <- 
  pets %>% 
  left_join(names_to_join, by = "name")

with_gender <- joined %>% drop_na(sex)
with_out_gender <- joined %>% filter(is.na(sex)) %>% distinct(name)

with_out_gender %>% 
  mutate(file = rep(1:9, length.out = nrow(.))) %>% 
  nest(-file) %>% 
  pwalk(function(file, data) write_csv(data, path = paste0("to_genderize/", file, ".csv")))
