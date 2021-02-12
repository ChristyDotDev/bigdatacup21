library(tidyverse)
library(ggrepel)
library(ggplot2)
library(jpeg)
library(grid)

pbp <- read.csv("source_data/hackathon_nwhl.csv")

img <- readJPEG("img/rink.jpg")

event_colours <- data.frame(event_type = c("Shot", "Goal"),
                            colour = c("#0000ff", "#00ff00"))

shots <- pbp %>%
  filter(Event == 'Shot' | Event == 'Goal') %>%
  filter(Home.Team.Skaters == 5 & Away.Team.Skaters == 5) %>%
  left_join(event_colours, by = c('Event' = 'event_type'))

ggplot(data = shots, aes(x = X.Coordinate, y = Y.Coordinate)) +
  annotation_custom(rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc")),
                    -Inf, Inf, -Inf, Inf) +
  labs(x = "", y = "", title = "5 on 5 shots (blue) and goals (green)",
       caption = "Data: https://www.stathletes.com/big-data-cup/") +
  geom_point(color = shots$colour) + xlim(0,200) + ylim(0,85)