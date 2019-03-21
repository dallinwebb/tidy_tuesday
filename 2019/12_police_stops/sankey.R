library(tidyverse)
library(ggalluvial)
stops <- read_rds("2019/00_police_stops/tr137st9964_tn_nashville_2019_02_25.rds")

# type > subject_race > searched at all > outcome

stops_alluvial <- 
  stops %>% 
  select(subject_race, frisk_performed, contains("search"), outcome) %>% 
  mutate(searched = frisk_performed + 
                    search_conducted +
                    search_person +
                    search_vehicle,
         searched = if_else(searched >= 1, T, F)) %>% 
  select(-c(contains("search_"), frisk_performed)) %>% 
  filter(searched == T,
         subject_race %in% c("white","black")) %>% 
  count(searched, subject_race, outcome) %>%
  na.omit() %>% 
  mutate(pct = n / sum(n),
         outcome_label = str_c(outcome, " ", scales::percent(pct))) %>% 
  print(n = 44)

stops_alluvial %>% 
ggplot(aes(axis2 = subject_race, 
           axis3 = outcome,
           y     = pct)) +
  geom_alluvium() +
  geom_stratum() +
  geom_text(stat = "stratum", label.strata = T)
  # ggthemes::theme_fivethirtyeight() +
  # theme(axis.text.x = element_blank())

