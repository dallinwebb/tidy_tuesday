library(tidyverse)
library(lubridate)
hor <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-10-23/movie_profit.csv")

hor <- hor %>% 
  select(-X1) %>% 
  mutate(release_date = mdy(release_date))



tib <- tibble(x = c(61,63,63,69,70,70,71,74,75))

tib %>% 
  mutate(x_minus_mean  = x - mean(x),
               squared = x_minus_mean^2) %>% 
  summarise(sum_square = sum(squared),
            variance   = sum_square/(9 - 1),
            sd         = sqrt(variance))

tib <- tibble(x = c(4,6,9,31,33,51,59,92))
num <- c(4,6,9,31,33,51,59,92)
quantile(num,type = 6)

tib <- tibble(HCG = c(.8,.8,2.7,3,37,78,1800,1800,2941,9111,23657,31616,48000),
              age = c(19,20,21,21,30,33,36,40,41,45,46,52,53),
              weight = c(115,131,137,149,169,171,183,196,196,200,217,234,241),
              sero = c(31,33,41,61,80,84,111,111,204,208,280, NA, NA))

tib %>%
  mutate(age_minus_mean       = age - mean(age),
         sq_age_minus_mean    = age_minus_mean^2,
         weight_minus_mean    = weight - mean(weight),
         sq_weight_minus_mean = weight_minus_mean^2) %>% 
  summarise(age_mean      = mean(age),
            age_median    = median(age),
            weight_mean   = mean(weight),
            weight_median = median(weight),
            
            )
