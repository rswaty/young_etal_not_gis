

# test calculating weighted means on 7 test counties in Texas

library(tidyverse)
library(stringr)


#test with 7 counties in Texas
testData  <- read_csv("testerCounties.csv")
View(testData)

# value will be "Value_1", weighting variable will be "Count", group by GEOID

testData <- testData %>%
  mutate(Value_1 = na_if(Value_1, 6)) %>%
  mutate(Value_1 = na_if(Value_1, 7)) %>%
  group_by(GEOID) %>%
  mutate(weighted_wfhpNA = weighted.mean(Value_1, Count, na.rm = TRUE))

write.csv(testData, file = "to_join.csv")

# try with CONUS wide data
us_cntys_wfhp <- read_csv("us_cntys_wfhp.csv")
View(us_cntys_wfhp)

us_cntys_wfhp <- us_cntys_wfhp  %>%
  mutate(whp2020_cls_conus = na_if(whp2020_cls_conus, 6)) %>%
  mutate(whp2020_cls_conus = na_if(whp2020_cls_conus, 7)) %>%
  group_by(GEOID) %>%
  mutate(weighted_wfhp = weighted.mean(whp2020_cls_conus, Count, na.rm = TRUE)) 

us_cntys_wfhp$GEOID <- str_pad(us_cntys_wfhp$GEOID, width = 5, side = "left", pad = "0")

write.csv(us_cntys_wfhp, row.names = FALSE, file = "to_join_US.csv")


# weighted means with forest to faucets data.  get weighted mean of WFP per county for joining

f2f_counties_raster <- read_csv("testWeightedMeans/f2f_counties_raster.csv")
View(f2f_counties_raster)

f2f_counties_raster2 <- f2f_counties_raster %>%
  group_by(GEOID) %>%
  summarize(county_sum = sum(Count),
  weighted_wfp = weighted.mean(WFP, Count, na.rm = TRUE))
    




