library(tidyverse)
library(readxl)

global_mortality <- read_xlsx("global_mortality.xlsx")

# I had my wife use photoshop to eye drop the original colors
my_colors <- c("#4D7BB8", "#D34744", "#58A754","#f3b43e",
               "#aa74b9","#47a6b0","#eb514d","#aed39e",
               "#fdd962","#c28bbd","#98cdd6","#ec5152",
               "#7fc453","#fa9071","#d5a0c7","#77c195",
               "#9a9199","#fcefa2","#b8cca5","#dde777",
               "#909bb1","#a26971","#7fc453","#fa9071",
               "#d5a0c7","#77c195","#9a9199","#fcefa2",
               "#b8cca5","#dde777","#909bb1","#a26971") %>% 
  rev()

global_mortality %>% 
  gather(disease, percentage, 4:35) %>% 
  filter(year == 2016) %>% 
  select(year, disease, percentage) %>% 
  mutate(disease = as.factor(disease),
         disease = gsub(" (%)", "", disease, fixed = T)) %>% 
  group_by(disease) %>% 
  summarise(percent_each = mean(percentage, na.rm = T)) %>% 
  arrange(desc(percent_each)) %>% 
ggplot(aes(reorder(disease, percent_each, FUN = max), percent_each)) +
  geom_bar(stat = "identity", 
           fill = my_colors, 
           width = .75) +
  geom_text(aes(label = paste0(round(percent_each,2),"%")),
            hjust = -.25,
            vjust = .125,
            size = 3) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), 
                     limits = c(0,35),
                     breaks = c(0,5,10,15,20,25,30)) +
  coord_flip() +
   labs(y = "",
       x = "",
       title = "Share of deaths by cause, World, 2016",
       subtitle = "Data refers to the specific cause of death, which is distinguished from risk factors for death, such as air pollution, diet and other lifestyle factors.\n",
       caption = "Source: IHME, Global Burden of Disease\n") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid = element_line(linetype = 'dashed'),
        plot.title = element_text(hjust = -.72, size = 16),
        plot.subtitle = element_text(hjust = 1.15),
        plot.caption = element_text(hjust = -.5))
 


