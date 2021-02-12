library(tidyverse)
library(ggrepel)

pbp <- read.csv("source_data/hackathon_nwhl.csv")

augmented_pbp <- pbp %>%
  group_by(game_date, Home.Team, Away.Team) %>%
  mutate(possession_id = cumsum(lag(Team, default = first(Team)) != Team | Event == 'Faceoff Win'), pos_start=dplyr::first(Clock)) %>%
  ungroup() %>%
  group_by(game_date, Home.Team, Away.Team, possession_id) %>%
  mutate(pos_start = first(Clock)) %>%
  # this is a bit poor, should make this a bit smarter, factor in the next event, period end etc.
  mutate(pos_result=last(Event)) %>%
  mutate(pos_start_event=first(Event)) %>%
  mutate(pos_start_x=first(X.Coordinate)) %>%
  mutate(pos_start_y=first(Y.Coordinate)) %>%
  mutate(pos_passes=sum(Event=='Play')) %>%
  mutate(pos_shots=sum(Event=='Shot' | Event=='Goal')) %>%
  mutate(pos_off_skaters=ifelse(first(Team)==first(Home.Team), Home.Team.Skaters, Away.Team.Skaters)) %>%
  mutate(pos_def_skaters=ifelse(first(Team)==first(Home.Team), Away.Team.Skaters, Home.Team.Skaters)) %>%
  mutate(pos_powerplay=pos_off_skaters>pos_def_skaters) %>%
  ungroup()

View(augmented_pbp)