
# forest to faucets

# package(s)
library(tidyverse)

# set options no sci
options(scipen = 9999999)


# read in data
f2f <- read.csv('data/cnty_wfp_f2f.csv')
svi <- read.csv('data/SVI2018_US_COUNTY.csv') %>%
  select(FIPS, STATE)


# join in states
f2f <- f2f %>%
  left_join(svi, by = c("GEOID" = "FIPS"))


## add intervals to match ArcGIS quantiles (mostly for labels)
f2f <- f2f %>%
  mutate(quantiles =cut(weighted_wfp, 
                        breaks = c(
                         -Inf,
                          0.010,
                          0.045,
                          0.13,
                          56),
                        labels = c(
                          "0",
                          "0.010 - 0.044",
                          "0.045 - 1.3",
                          "1.4 - 56")
  ))

# group by quantiles for chart
f2f_pop_quantiles <- f2f %>%
  group_by(quantiles) %>%
  summarize(total_pop = sum(pop)) %>%
  mutate(percentage = round(total_pop/sum(total_pop)*100))


# explore states

f2f_grouped_states <- f2f %>%
  group_by(STATE) %>%
  summarise(state_wfp = mean(weighted_wfp))

