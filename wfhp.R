

## da packages
library(tidyverse)
library(scales)

## read data
wfhp <- read.csv("data/cnty_wfhp.csv")

## add intervals to match ArcGIS quantiles (mostly for labels)
wfhp <- wfhp %>%
  mutate(quantiles =cut(weighted_wfhp, 
                        breaks = c(
                          0,
                          1.7,
                          2.4,
                          3.2,
                          3.9,
                          4.7),
                        labels = c(
                          "1.0 - 1.7",
                          "1.7 - 2.4",
                          "2.4 - 3.2",
                          "3.2 - 3.9",
                          "3.9 - 4.6")
  ))

## create colors to match map
colors <- c(
  "#FFFFFF",
  "#FCD0C0",
  "#FB6A4A",
  "#DE2D26",
  "#7B0F15"
  
)


# group by quantiles for chart
wfhp_pop_quantiles <- wfhp %>%
  group_by(quantiles) %>%
  summarize(total_pop = sum(pop)) %>%
  mutate(percentage = round(total_pop/sum(total_pop)*100))

# make chart

wfhp_quantiles_pop_chart <-
  ggplot(wfhp_pop_quantiles, aes(x = quantiles, y = total_pop, fill = quantiles)) +
  geom_bar(stat = 'identity', color = '#3d3d3d') +
  coord_flip() +
  labs(
    x = "",
    y = "Total Population"
  ) +
  scale_fill_manual(values = c(
    "#FFFFFF",
    "#FCD0C0",
    "#FB6A4A",
    "#DE2D26",
    "#7B0F15"
    
  )) +
  scale_y_continuous(labels = comma) +
  geom_text(aes(label = paste0(percentage, "%")),
            vjust = -0.5, 
            hjust = -0.10, 
            color = "black",
            size = 6) + 
  theme_bw(base_size = 18) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = 'none')+
  # theme(plot.margin = margin(0, #top
  #                            3, #right
  #                            0, # bottom
  #                            0, #left
  #                            "cm")) + 
  expand_limits(y = 160000000) 


wfhp_quantiles_pop_chart






