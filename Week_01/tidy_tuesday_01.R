library(tidyverse)
library(viridis)
library(readxl)

file_path <- "../Data/us_avg_tuition.xlsx"
r <- read_excel(file_path)

r <- r %>% 
  rename(state = State) %>% 
  mutate(state_abb = factor(state.abb))

r2 <- r %>% 
  mutate(tuitfee_5yr_pct_chg = (`2015-16` - `2010-11`) / `2010-11` * 100) %>% 
  gather(year, tuitfee, '2004-05':'2015-16') %>% 
  select(state_abb, year, tuitfee, tuitfee_5yr_pct_chg) %>% 
  filter(year == "2015-16")

ggplot(r2, aes(fct_reorder(state_abb, tuitfee), tuitfee, col = tuitfee_5yr_pct_chg)) +
  geom_point() +
  ggtitle("Average tuition and fees in the United States", subtitle = "Average tuition and fees for one year of full-time study at public institutions") +
  labs(x = "State", y = "/nAverage Total Tuition and Fees", color = "5-Yr % Chg/n", caption = "Sourse:") +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_viridis() +
  coord_flip() +
  theme_minimal()

pc_chg <- r %>% 
  mutate(`2005_pc` = (`2005-06` - `2004-05`) / `2004-05` * 100) %>% 
  mutate(`2006_pc` = (`2006-07` - `2005-06`) / `2005-06` * 100) %>% 
  mutate(`2007_pc` = (`2007-08` - `2006-07`) / `2006-07` * 100) %>% 
  mutate(`2008_pc` = (`2008-09` - `2007-08`) / `2007-08` * 100) %>% 
  mutate(`2009_pc` = (`2009-10` - `2008-09`) / `2008-09` * 100) %>% 
  mutate(`2010_pc` = (`2010-11` - `2009-10`) / `2009-10` * 100) %>%
  mutate(`2011_pc` = (`2011-12` - `2010-11`) / `2010-11` * 100) %>% 
  mutate(`2012_pc` = (`2012-13` - `2011-12`) / `2011-12` * 100) %>% 
  mutate(`2013_pc` = (`2013-14` - `2012-13`) / `2012-13` * 100) %>% 
  mutate(`2014_pc` = (`2014-15` - `2013-14`) / `2013-14` * 100) %>% 
  mutate(`2015_pc` = (`2015-16` - `2014-15`) / `2014-15` * 100) %>% 
  select(state, `2005_pc`:`2015_pc`) %>% 
  group_by(state) %>% 
  summarise(avg_pc_chg = mean(`2005_pc`:`2015_pc`)) %>% 
  arrange(desc(avg_pc_chg))

ggplot(pc_chg, aes(reorder(state, avg_pc_chg),avg_pc_chg)) +
  geom_point() +
  coord_flip() +
  labs(y = "Average Percent Change in Tuition", x = "State", title = "Change in Tuition from 2005 to 2015")


idaho <- r %>% 
  filter(state %in% c("Idaho","Utah","California")) %>% 
  gather(year, tuitfee, '2004-05':'2015-16')
idaho

ggplot(idaho, aes(factor(year),tuitfee, group = factor(state), col = factor(state))) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  labs(x = "Year", y = "Average State Tuition", col = "State", title = "Average tuition from 2005 to 2015") +
  scale_y_continuous(labels = scales::dollar) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))