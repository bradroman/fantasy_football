library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)

# Reading in the data
seasons <- 2020
pbp <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  )
})

# Getting roster data
rosters <- nflfastR::fast_scraper_roster(seasons)

# using to view samples of data
#pbp %>% filter(home_team == "BUF") %>% select(game_id) %>% distinct() %>% View()

# PASSING FANTASY POINTS
get_passing_stats <- function(){
  qbs <- pbp %>% 
    filter(week <= 17 
           , play_type == 'pass'
           , !is.na(passer)
           , down <= 4
    ) %>% 
    mutate(player = case_when(play_type == 'pass' ~ passer)) %>% 
    group_by(posteam
             , player
             , game_id) %>% 
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
              , .groups = 'drop')
  return(qbs)
}

# Total points for RBs and WRs and TEs and QB running stats on a per game basis 
get_flex_stats <- function(){
  flex_players <- pbp %>%
    filter(week <= 17
           )%>%
    mutate(player = case_when(play_type == 'pass' ~ receiver, 
                              (play_type == 'run') & (qb_scramble == 1) | (qb_kneel == 1) ~ name,
                              play_type == 'run'  ~ rusher
                              )) %>%
    filter(!is.na(player)) %>% 
    group_by(posteam
             , player
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
    select(game_id, posteam, passer, rusher, receiver) %>%
    gather(key = position, player, passer, rusher, receiver) %>% 
    filter(!is.na(player)) %>% 
    mutate(two_pt_conv = 1) %>% 
    select(game_id, posteam, player, two_pt_conv)
  return(two_pt_conv)
}

# Joining QB's and flex players
get_fantasy_points <- function(passing, flex, two_point){
  fantasy_points <- passing %>% 
    full_join(flex, by = c('posteam','player','game_id')) %>% 
    full_join(two_point, by = c('posteam','player','game_id')) %>%
    replace(is.na(.), 0) %>%
    mutate(fumbles = fumbles.x + fumbles.y
           , fantasy_points = passing_yards*0.04 + passing_touchdowns*4 + (rush_touchdowns+receiving_touchdowns)*6 
           + interceptions*-1 + fumbles*-2 + rushing_yards*0.1 + catches*0.5 + receiving_yards*0.1 + two_pt_conv*2) %>% 
    select(-fumbles.x, -fumbles.y) %>% 
    arrange(-fantasy_points)
  return(fantasy_points)
}

# Joins tables together and calculates fantasy points for each game
get_game_points <- function(){
  p <- get_passing_stats()
  f <- get_flex_stats()
  t <- get_two_pt_conv()
  fp <- get_fantasy_points(p,f,t)
  return(fp)
}

player_week_points <- get_game_points()

player_season_points <- player_week_points %>% 
  select(-posteam, -game_id) %>% 
  group_by(player) %>% 
  summarise(across(everything(), list(sum))) %>% 
  View()

player_week_points %>% 
  group_by(posteam, player) %>% 
  select(-game_id) %>% 
  summarise(across(everything(), ~ sum(., is.na(.), 0))
            , .groups = 'drop') %>% 
  arrange(-fantasy_points) %>% 
  View()

# df_2020 %>% 
#   filter(player == 'R.Wilson') %>% 
#   arrange(game_id) %>% 
#   View()
# 
# pbp %>%
#   filter(game_id == '2020_02_NE_SEA'
#          , posteam == 'SEA'
#          , play_type == 'run') %>%
#   View()
# 
# ggplot(df_2020 %>% select(fantasy_points), aes(x = fantasy_points)) +
#   geom_dotplot()
