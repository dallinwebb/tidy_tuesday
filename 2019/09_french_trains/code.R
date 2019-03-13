library(tidyverse)

trains_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-26/full_trains.csv")
small_trains <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-26/small_trains.csv")

# Do the five lowest rated train stations have more delays?
# 2018 Had a lot of reported delays, how does it compare to the previous years?