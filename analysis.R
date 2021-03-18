library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(glue)

setwd('C:/users/bradr/Python/fantasy_football')

season = '2020'

draft_picks <- read_csv(glue('output/{season}_draft_picks.csv'))
player_draft_info <- read_csv(glue('output/{season}_player_draft_info.csv'))
roster_week <- read_csv(glue('output/{season}_roster_week.csv'))
teams <- read_csv(glue('output/{season}_teams.csv'))
transactions <- read_csv(glue('output/{season}_transactions.csv'))
weekly_scoreboard <- read_csv(glue('output/{season}_weekly_scoreboard.csv'))

roster_week %>% 
  filter(team_id == '8',
         lineup_position != 'BN') %>% 
  group_by(team_id, week) %>% 
  summarise(total_points = round(sum(fantasy_points),4))

