

## da packages
library(tidyverse)
library(scales)

## read data
svi <- read.csv("data/rpl_themes.csv")

## add intervals to match ArcGIS quantiles (mostly for labels)
svi <- svi %>%
      mutate(quantiles =cut(rpl_themes, 
                            breaks = c(
                              0,
                              0.2,
                              0.4,
                              0.6,
                              0.8,
                              1.0),
                            labels = c(
                              "0 - 0.2",
                              "0.2 - 0.4",
                              "0.4 - 0.6",
                              "0.6 - 0.8",
                              "0.8 - 1.0")
                            ))

## create colors to match map
colors <- c(
  "#EDF8FB",
  "#B3CDE3",
  "#8C96C6",
  "#8856A7",
  "#632360"
  
)


# group by quantiles for chart
svi_pop_quantiles <- svi %>%
  group_by(quantiles) %>%
  summarize(total_pop = sum(e_pop)) %>%
  mutate(percentage = round(total_pop/sum(total_pop)*100))

# make chart

svi_quantiles_pop_chart <-
  ggplot(svi_pop_quantiles, aes(x = quantiles, y = total_pop, fill = quantiles)) +
  geom_bar(stat = 'identity', color = '#3d3d3d') +
  coord_flip() +
  labs(
    x = "",
    y = "Total Population"
    ) +
  scale_fill_manual(values = c(
    "#EDF8FB",
    "#B3CDE3",
    "#8C96C6",
    "#8856A7",
    "#632360"
    
  )) +
  scale_y_continuous(labels = comma) +
  geom_text(aes(label = paste0(percentage, "%")),
            vjust = -0.5, 
            hjust = -0.15, 
            color = "black",
            size = 6) + 
  theme_bw(base_size = 18) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = 'none')


svi_quantiles_pop_chart


# wrangle states

svi_grouped_states <- svi %>%
  group_by(state) %>%
  summarise(state_svi = mean(rpl_themes))



