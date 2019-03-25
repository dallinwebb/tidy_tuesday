suppressPackageStartupMessages(library(genderizeR))
library(tidyverse)

to_genderize <- read_csv("to_genderize/1.csv")
genderized <- genderizeR::findGivenNames(to_genderize$name)
write_rds(genderized, path = "genderized/1.csv")