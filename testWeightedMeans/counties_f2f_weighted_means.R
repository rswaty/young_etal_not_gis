

## weighted means take 2

## load packages
library(tidyverse)
library(stringr)

## start with f2f by counties


f2f_counties <- read_csv("testWeightedMeans/counties_f2f_combine.csv")
View(f2f_counties)

f2f_counties2 <- f2f_counties %>%
  group_by(FIPS) %>%
  summarize(county_sum = sum(Count),
            weighted_wfp = weighted.mean(WFP_IMP_R, Count, na.rm = TRUE))

write.csv(f2f_counties2, "data/f2fcounties2.csv")
