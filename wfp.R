
## da packages
library(tidyverse)
library(scales)

## read data
wfp <- read.csv("data/cnty_wfp.csv")

## add intervals to match ArcGIS quantiles (mostly for labels)
wfp <- wfp %>%
  mutate(quantiles =cut(weighted_wfp, 
                        breaks = c(
                          -1,
                          20,
                          40,
                          60,
                          80,
                          100),
                        labels = c(
                          "0 - 20",
                          "20 - 40",
                          "40 - 60",
                          "60 - 80",
                          "80 - 100")
  ))

## create colors to match map
colors <- c(
  "#FFFFFF",
  "#FDBE85",
  "#FD8D3C",
  "#BD5A29",
  "#77371A"
  
)


# group by quantiles for chart
wfp_pop_quantiles <- wfp %>%
  group_by(quantiles) %>%
  summarize(total_pop = sum(e_pop)) %>%
  mutate(percentage = round(total_pop/sum(total_pop)*100))

# make chart

wfp_quantiles_pop_chart <-
  ggplot(wfp_pop_quantiles, aes(x = quantiles, y = total_pop, fill = quantiles)) +
  geom_bar(stat = 'identity', color = '#3d3d3d') +
  coord_flip() +
  labs(
    x = "",
    y = "Total Population"
  ) +
  scale_fill_manual(values =  c(
    "#FFFFFF",
    "#FDBE85",
    "#FD8D3C",
    "#BD5A29",
    "#77371A"
    
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
  expand_limits(y = 225000000) 


wfp_quantiles_pop_chart


# wrangle states

wfp_grouped_states <- wfp %>%
  group_by(state, quantiles) %>%
  summarise(state_wfp = mean(weighted_wfp),
            pop = sum(e_pop))

