library(tidyverse)
library(readxl)
library(lubridate)
data <- read_xlsx("2018/Week_02/tidy_tuesday_week2.xlsx")
colnames(data) <- c("year","CB","DE","LB","OL","QB","RB","S","DT","TE","WR")

order <- c("RB","QB","OL","TE","WR","CB","DE","DT","LB","S")
data_long <- data %>%
  gather(position, pay, -1) %>%
  filter(pay < 25000000,
         year %in% c(2012,2014,2016,2018)) %>% 
  mutate(off_def = case_when(
    position %in% c("RB","QB","OL","TE","WR") ~ 'OFFENSE',
    position %in% c("CB","DE","DT","LB","S") ~ 'DEFENSE')) %>% 
  mutate(off_def = as.factor(off_def),
         position = as.factor(position),
         pay = pay/1000000,
         year = parse_date(as.character(year), format = "%Y")) %>% 
  group_by(position, year) %>% 
  filter(row_number(desc(pay)) == c(1:16)) %>% 
  mutate(ord = factor(position, levels = order)) %>% 
  arrange(ord)

ggplot(data_long, aes(year, pay)) +
  geom_jitter(alpha = .3, width = 100) +
  geom_smooth(method = "loess") +
  facet_wrap(~position, ncol = 5) +
  scale_y_continuous(labels = c(0,5,10,15,20,"$25m")) +
  scale_x_date(date_labels = "'%y") +
  theme_gray() +
  theme(panel.spacing.x = unit(.5, "cm"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        panel.background = element_rect(fill = "gray95"),
        panel.grid.major = element_line(color = "gray75"),
        panel.grid.minor = element_line(color = "gray95"),
        strip.background = element_rect(fill = "gray95"),
        plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 18),
        plot.caption = element_text(color = "gray60"),
        plot.background = element_rect(fill = "gray95")) +
  labs(title = "The average pay for top running backs has stalled",
       subtitle = "Average cap value of 16 highest-paid players in each position\n",
       y = "Average cap value",
       x = "",
       caption = "SOURCE: ESPN STATS & INFORMATION GROUP")
