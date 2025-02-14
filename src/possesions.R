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
  filter(Event == 'Incomplete Play' | Event=='Play') %>%
  select(Clock, Home.Team.Skaters, Away.Team.Skaters, Event, X.Coordinate, Y.Coordinate, X.Coordinate.2, Y.Coordinate.2, Detail.1, next_pos_result) %>%
  filter(Detail.1 == 'Direct') %>%
  # bearing relative to the y axis
  mutate(pass_bearing = angle_between(X.Coordinate, Y.Coordinate, X.Coordinate.2, Y.Coordinate.2)) %>%
  # face the goal, which is to the right of the x axis in this case
  mutate(bearing_relative_to_goal = ifelse(pass_bearing-90<0, pass_bearing+270, pass_bearing-90))

View(passes)

turnover_by_angle <- passes %>%
  group_by(bearing_relative_to_goal) %>%
  mutate(passes=n(), turnover_rate = ifelse(Event== 'Incomplete Play', 1,0)/n()*100)

View(turnover_by_angle)

#TODO - graph turnover rate by angle of pass
# Make the plot
graph <- ggplot(turnover_by_angle, aes(x=as.factor(bearing_relative_to_goal), y=turnover_rate)) +
  geom_bar(stat="identity", fill=alpha("blue", 0.3)) +
  ylim(-20,100) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm")
  ) +
  coord_polar(start = 0)

View(graph)