library(tidyverse)
library(ggrepel)
library(ggplot2)
library(jpeg)
library(grid)
library(reticulate)
library(fmsb)

source_python('src/pass_angle.py')

#pbp <- list.files(path = "./source_data/", pattern = "*.csv", full.names = T) %>%
#  map_df(~read_csv(.))
pbp <- read.csv("source_data/hackathon_scouting.csv")

img <- readJPEG("img/rink.jpg")
event_colours <- data.frame(event_type = c("Shot", "Goal"),
                            colour = c("#0000ff", "#00ff00"))

augmented_pbp <- pbp %>%
  group_by(game_date, Home.Team, Away.Team) %>%
  mutate(possession_id = cumsum(lag(Team, default = first(Team)) != Team | Event == 'Faceoff Win'), pos_start = dplyr::first(Clock)) %>%
  ungroup() %>%
  group_by(game_date, Home.Team, Away.Team, possession_id) %>%
  mutate(pos_start = first(Clock)) %>%
  mutate(pos_first_event = first(Event)) %>%
  # this is a bit poor, should make this a bit smarter, factor in the next event, period end etc.
  mutate(pos_result = last(Event)) %>%
  mutate(pos_start_event = first(Event)) %>%
  mutate(pos_start_x = first(X.Coordinate)) %>%
  mutate(pos_start_y = first(Y.Coordinate)) %>%
  mutate(pos_passes = sum(Event == 'Play')) %>%
  mutate(pos_shots = sum(Event == 'Shot' | Event == 'Goal')) %>%
  mutate(pos_off_skaters = ifelse(first(Team) == first(Home.Team), Home.Team.Skaters, Away.Team.Skaters)) %>%
  mutate(pos_def_skaters = ifelse(first(Team) == first(Home.Team), Away.Team.Skaters, Home.Team.Skaters)) %>%
  mutate(pos_powerplay = pos_off_skaters > pos_def_skaters) %>%
  ungroup() %>%
  group_by(game_date, Home.Team, Away.Team, possession_id) %>%
  ungroup() %>%
  # for the next event, what's the result of the possession (temp variable to map result of next possession)
  mutate(next_event_pos_result = lead(pos_result)) %>%
  mutate(next_event_pos_first_event = lead(pos_first_event)) %>%
  group_by(game_date, Home.Team, Away.Team, possession_id) %>%
  mutate(next_pos_result = last(next_event_pos_result)) %>%
  mutate(next_pos_first_event = last(next_event_pos_first_event)) %>%
  ungroup()

passes <- augmented_pbp %>%
  filter(Event == 'Incomplete Play') %>%
  filter(next_pos_first_event != 'Faceoff Win') %>%
  select(Clock, Home.Team.Skaters, Away.Team.Skaters, Event, X.Coordinate, Y.Coordinate, X.Coordinate.2, Y.Coordinate.2, Detail.1, next_pos_result) %>%
  filter(Detail.1 == 'Direct') %>%
  mutate(pass_bearing = angle_between(X.Coordinate, Y.Coordinate, X.Coordinate.2, Y.Coordinate.2))

turnover_leading_to_shot <- turnovers %>%
  filter(next_pos_result == 'Goal' | next_pos_result == 'Shot') %>%
  left_join(event_colours, by = c('Event' = 'event_type'))

ggplot(data = turnover_leading_to_shot, aes(x = X.Coordinate, y = Y.Coordinate)) +
  annotation_custom(rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc")),
                    -Inf, Inf, -Inf, Inf) +
  labs(x = "", y = "", title = "Turnovers on direct passes which gave up shots",
       caption = "Data: https://www.stathletes.com/big-data-cup/") +
  xlim(0, 200) +
  ylim(0, 85) +
  geom_segment(color = '#992222',
               aes(
                 xend = X.Coordinate.2,
                 yend = Y.Coordinate.2
               ),
               arrow = arrow(length = unit(0.3, "cm"))
  )