library(tidyverse)
library(ggrepel)

pbp <- read.csv("source_data/hackathon_nwhl.csv")

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

turnovers <- augmented_pbp %>%
  filter(pos_result == 'Incomplete Play') %>%
  filter(Event == 'Incomplete Play') %>%
  filter(next_pos_first_event != 'Faceoff Win') %>%
  select(Clock, Home.Team.Skaters, Away.Team.Skaters, Event, X.Coordinate, Y.Coordinate, X.Coordinate.2, Y.Coordinate.2, Detail.1, next_pos_result) %>%
  filter(Detail.1 == 'Direct')

View(turnovers)