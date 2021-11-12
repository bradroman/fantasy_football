library(tidyverse)
library(nflfastR)
library(nflreadr)
library(glue)
# library(Rcpp)

setwd('C:/users/bradr/Python/fantasy_football/yahoo')

#source("league_settings.R")

# TODO Defense extra point return points

#test
# fastr_player_stats <- nflfastR::load_player_stats()

# Reading in the data
season <- 2021

pbp <- nflfastR::load_pbp(seasons = season) %>%
  filter(season_type == 'REG')

pbp <- nflfastR::clean_pbp(pbp)

# Getting roster data for yahoo ID 
rosters <- nflfastR::fast_scraper_roster(season)

# PASSING FANTASY POINTS
get_passing_stats <- function(){
  qbs <- pbp %>% 
    filter(play_type == 'pass'
           , !is.na(passer)
           , down <= 4
    ) %>% 
    mutate(player = case_when(play_type == 'pass' ~ passer)) %>% 
    group_by(player
             , player_id = passer_id
             , game_id
             , posteam) %>% 
    summarise(attempts_test = sum(case_when(!grepl("sacked",desc) & play_type == 'pass' ~ pass_attempt, TRUE ~ 0))
              , completions = sum(complete_pass)
              , passing_yards = sum(case_when(play_type == 'pass' ~ yards_gained,
                                              TRUE ~ 0),
                                    case_when(grepl("sacked",desc) & play_type == 'pass' ~ -yards_gained,
                                              TRUE ~0))
              , passing_touchdowns = sum(case_when(play_type == 'pass' & touchdown == 1 & posteam == td_team ~ 1,
                                                   TRUE ~ 0))
              , interceptions = sum(interception)
              , fumbles = sum(case_when(
                (player == fumbled_1_player_name) & (fumble_lost == 1) ~ 1,
                TRUE ~ 0))
              , .groups = 'drop') %>% 
    select(-posteam)
  return(qbs)
}

# Total points for RBs and WRs and TEs and QB running stats on a per game basis 
get_flex_stats <- function(){
  flex_players <- pbp %>%
    mutate(player = case_when(play_type == 'pass' ~ receiver, 
                              (play_type == 'run') & (qb_scramble == 1) | (qb_kneel == 1) ~ name,
                              play_type == 'run'  ~ rusher
                              ),
           player_id = case_when(play_type == 'pass' ~ receiver_id, 
                                 (play_type == 'run') & (qb_scramble == 1) | (qb_kneel == 1) ~ id,
                                 play_type == 'run'  ~ rusher_id
           )) %>%
    filter(!is.na(player)) %>% 
    group_by(player
             , player_id
             , game_id) %>% 
    summarise(rushes = sum(rush_attempt)
              , rushing_yards = sum(case_when((play_type == 'run' | play_type == 'qb_kneel') & two_point_attempt == 0 ~ yards_gained,
                                              TRUE ~ 0))
              , targets = sum(case_when(play_type == 'pass' ~ 1,
                                      TRUE ~ 0))
              , catches = sum(complete_pass)
              , receiving_yards = sum(case_when(play_type == 'pass' & two_point_attempt == 0 ~ yards_gained,
                                                TRUE ~ 0))
              , rush_touchdowns = sum(case_when(play_type == 'run' & touchdown == 1 ~ 1,
                                                TRUE ~ 0))
              , receiving_touchdowns = sum(case_when(play_type == 'pass' & touchdown == 1 ~ 1,
                                                TRUE ~ 0))
              , total_touchdowns = rush_touchdowns + receiving_touchdowns
              , fumbles = sum(case_when(
                (player == fumbled_1_player_name) & (fumble_lost == 1) ~ 1,
                TRUE ~ 0))
              , .groups = 'drop')
  return(flex_players)
}

# two point conversions for fantasy
get_two_pt_conv <- function(){
  two_pt_conv <- pbp %>% 
    filter(week <= 17
           , two_point_conv_result == 'success') %>% 
    mutate(pass_name_id = paste0(passer, '_', passer_id),
           rush_name_id = paste0(rusher, '_', rusher_id),
           rec_name_id = paste0(receiver, '_', receiver_id)) %>% 
    select(game_id, pass_name_id, rush_name_id, rec_name_id) %>%
    gather(key = position, value = player, pass_name_id, rush_name_id, rec_name_id) %>% 
    filter(!(player == 'NA_NA')) %>%
    mutate(two_pt_conv = 1,
           player_id = sub('_','',substr(player, gregexpr(pattern ='_',player), 100)),
           player = sub('_','',substr(player, 0,gregexpr(pattern ='_',player)))) %>% 
    select(game_id, player_id, player, two_pt_conv)
  return(two_pt_conv)
}

# Defense stats
get_defense_stats <- function(){
  def <- pbp %>% 
    filter(!is.na(defteam),
           play_type != 'no_play') %>% 
    group_by(game_id, player = defteam, player_id = 'DEF') %>% 
    summarise(sacks = sum(sack, na.rm = TRUE),
              def_interceptions = sum(interception, na.rm = TRUE),
              fumble_recs = sum(fumble_lost[play_type != 'punt'], na.rm = TRUE),
              safeties = sum(safety, na.rm = TRUE),
              touchdowns = sum(return_touchdown, na.rm = TRUE),
              pa = max(ifelse(defteam == home_team, max(away_score), max(home_score))),
              pa_points = (case_when(
                pa == 0  ~ 10,
                pa <= 6  ~ 7,
                pa <= 13 ~ 4,
                pa <= 20 ~ 1,
                pa <= 27 ~ 0,
                pa <= 34 ~ -1,
                TRUE ~ -4
              )),
              def_fantasy_points = sacks*1 + def_interceptions*2 + fumble_recs*2 + touchdowns*6
              + safeties*2 + pa_points,
              .groups = 'drop')
  return(def)
}

# Kicker Stats
get_kicker_stats <- function(){
  kicker_stats <- pbp %>% 
    filter(field_goal_attempt == 1 | extra_point_attempt == 1) %>%
    mutate(fg_result = ifelse(field_goal_attempt == 1 & 
                                field_goal_result %in% c('missed','blocked',NA), 0, 1),
           xp_result = ifelse(extra_point_attempt == 1 & 
                                extra_point_result %in% c('missed','blocked',NA), 0, 1),
           fantasy_points = case_when(
             extra_point_result == 'good' ~ 1,
             field_goal_result == 'made' & kick_distance <= 19 ~ 1.5,
             field_goal_result == 'made' & kick_distance <= 29 ~ 2,
             field_goal_result == 'made' & kick_distance <= 39 ~ 2.5,
             field_goal_result == 'made' & kick_distance <= 49 ~ 3,
             field_goal_result == 'made' & kick_distance > 50 ~ 5,
             TRUE ~ 0
           )) %>% 
    group_by(game_id, kicker_player_id, kicker_player_name) %>% 
    summarise(fg_attempts = sum(field_goal_attempt),
              fg_makes = sum(ifelse(field_goal_attempt == 1, fg_result, 0)),
              xp_attempts = sum(extra_point_attempt), 
              xp_makes = sum(ifelse(extra_point_attempt == 1, xp_result, 0)),
              kick_fantasy_points = sum(fantasy_points),
              .groups = 'drop') %>% 
    select(player = kicker_player_name, 
           player_id = kicker_player_id, 
           game_id,
           fg_attempts,
           fg_makes,
           xp_attempts,
           xp_makes,
           kick_fantasy_points)
  return(kicker_stats)
}

# Joining QBs and flex players
get_fantasy_points <- function(passing, flex, two_point, kicker, defense){
  fantasy_points <- passing %>% 
    full_join(flex, by = c('player','player_id','game_id')) %>% 
    full_join(two_point, by = c('player','player_id','game_id')) %>%
    full_join(kicker, c('player','player_id','game_id')) %>% 
    full_join(defense, c('player','player_id','game_id')) %>% 
    replace(is.na(.), 0) %>%
    mutate(fumbles = fumbles.x + fumbles.y
           , fantasy_points = passing_yards*0.04 + passing_touchdowns*4 + (rush_touchdowns+receiving_touchdowns)*6 
           + interceptions*-1 + fumbles*-2 + rushing_yards*0.1 + catches*0.5 + receiving_yards*0.1 + two_pt_conv*2
           + kick_fantasy_points + def_fantasy_points) %>% 
    select(-fumbles.x, -fumbles.y) %>% 
    arrange(-fantasy_points)
  return(fantasy_points)
}

# Joins tables together and calculates fantasy points for each game
get_game_points <- function(){
  p <- get_passing_stats()
  f <- get_flex_stats()
  t <- get_two_pt_conv()
  k <- get_kicker_stats()
  d <- get_defense_stats()
  fp <- get_fantasy_points(p,f,t,k,d)
  return(fp)
}

player_week_points <- get_game_points() %>% 
  left_join(rosters %>% select(gsis_id, yahoo_id, position, headshot_url), 
            by = c('player_id' = 'gsis_id')) %>% 
  mutate(position = ifelse(is.na(position), 'DEF', position))

player_season_points <- player_week_points %>% 
  distinct() %>% 
  select(-game_id) %>% 
  group_by(player, player_id, yahoo_id, position, headshot_url) %>% 
  summarise(across(everything(), list(sum)),
            .groups = 'drop',
            total_fantasy_points = fantasy_points_1,
            avg_fantasy_points = mean(fantasy_points),
            med_fantasy_points = median(fantasy_points))

write_csv(player_week_points, glue('output/{season}_player_week_points.csv'))
write_csv(player_season_points, glue('output/{season}_player_season_points.csv'))
