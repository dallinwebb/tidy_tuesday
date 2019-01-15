library(tidyverse)
my_data <- read_csv("https://github.com/the-pudding/data/raw/master/cetaceans/allCetaceanData.csv")

my_data %>% 
  select(-X1) %>% 
  count(COD, sort = T) %>% 
  drop_na() %>% 
  filter(COD != "-") %>% 
  mutate(COD = str_to_title(COD),
         COD = ifelse(str_detect(COD,"Euthanasia"), "Euthanasia", COD)) %>% 
  group_by(COD) %>% 
  summarise(n = sum(n)) %>% 
  arrange(desc(n)) %>% 
  top_n(10) %>% 
  mutate(COD = fct_reorder(COD, n, max)) %>% 
  ggplot(aes(COD, n)) +
  geom_col(fill = "skyblue3") +
  geom_text(aes(x = COD, y = n, label = n), nudge_y = 2) +
  coord_flip() +
  labs(x = NULL,
       y = "Total Count Between 1938 & 2017",
       title = "Top 10 Causes of Captive Wale & Dolphin Deaths") +
  theme_minimal()  
