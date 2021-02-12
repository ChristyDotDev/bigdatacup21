library(tidyverse)
library(ggrepel)

pbp <- read.csv("source_data/hackathon_nwhl.csv")

View(pbp)

pbp <- pbp %>%
  mutate(possession_id = cumsum(lag(Team, default = first(Team)) != Team | Event == 'Faceoff Win'))

View(pbp)

pbp <- pbp %>%
  group_by(possession_id) %>%
  summarise(team=last(Team), result=last(Event))

View(pbp)