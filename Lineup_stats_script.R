# # You can install using the pacman package using the following code:
# if (!requireNamespace('pacman', quietly = TRUE)){
#   install.packages('pacman')
# }
# pacman::p_load_current_gh("sportsdataverse/hoopR", dependencies = TRUE, update = TRUE)

##This code came from:
##https://github.com/ramirobentes/NBA-in-R/blob/master/hoopR%20pbp%20past%20seasons.R

##Functions to use
#nba_boxscoretraditionalv3()
#nba_boxscoreadvancedv3()

#################################
#################################
##Data Collection and Wrangling##
#################################
#################################
library(tidyverse)
library(hoopR)
library(janitor)
library(future)
library(zoo)
library(Matrix)
library(glmnet)

lineup_level_stats <- function(year){
  
  ##Pull player boxscore data
  player_logs <- nba_leaguegamelog(season = 2022, player_or_team = "P") %>%  # change season here
    pluck("LeagueGameLog") %>%
    clean_names() %>%
    mutate(team_location = ifelse(str_detect(matchup, "\\@"), "away", "home"),
           across(c(player_id, team_id), as.numeric))
  
  ##Generic function that pulls play by play data for a user specified game, then adds a game_id column
  function_pbp <- function(x){
    nba_data_pbp(x) %>%
      mutate(game_id = x)
  }
  
  ##Note event_type definitions:
  # 1 -> MAKE
  # 2 -> MISS
  # 3 -> FreeThrow
  # 4 -> Rebound
  # 5 -> Turnover
  # 6 -> Foul
  # 7 -> Violation
  # 8 -> Substitution
  # 9 -> Timeout
  # 10 -> JumpBall
  # 11 -> Ejection
  # 12 -> StartOfPeriod
  # 13 -> EndOfPeriod
  # 14 -> Empty
  
  ##Extract unique game log values
  games <- player_logs %>%
    distinct(game_id) %>%
    pull(game_id)
  
  few_games <- c(games[1:2], "0022200025")
  ##Pulls PBP data for all game ID's in the specified season (takes forever)
  plan(multicore)
  #nba_pbp_raw <- map_df(games, function_pbp)
  nba_pbp_raw <- map(games, function_pbp) %>% list_rbind()
  
  nba_pbp <- nba_pbp_raw %>%
    left_join(player_logs %>%
                distinct(player1_id = player_id, player1 = player_name)) %>%
    left_join(player_logs %>%
                distinct(player2_id = player_id, player2 = player_name)) %>%
    left_join(player_logs %>%
                distinct(player3_id = player_id, player3 = player_name)) %>%
    left_join(player_logs %>%
                distinct(team_id = team_id, slug_team = team_abbreviation)) %>%
    left_join(player_logs %>%
                distinct(offense_team_id = team_id, off_slug_team = team_abbreviation)) %>%
    select(game_id, period, clock, number_event = event_num, msg_type = event_type, 
           act_type = event_action_type, slug_team,
           off_slug_team, player1, player2, player3, description, desc_value = opt1,
           opt2, home_score, away_score, order, locX, locY) %>%
    mutate(game_id = as.integer(game_id)) %>%
    left_join(player_logs %>%
                distinct(game_id = as.integer(game_id), slug_team = team_abbreviation, team_location) %>%
                pivot_wider(names_from = team_location,
                            values_from = slug_team,
                            names_prefix = "team_")) %>%
    as_tibble() 
  
  nba_pbp <- nba_pbp %>%
    mutate(number_original = number_event) %>%
    separate(clock, into = c("min_remain", "sec_remain"), sep = ":", remove = FALSE, convert = TRUE) %>%
    mutate(secs_left_qtr = (min_remain * 60) + sec_remain) %>%                       
    mutate(secs_start_qtr = case_when(                                                                        
      period %in% c(1:5) ~ (period - 1) * 720,
      TRUE ~ 2880 + (period - 5) * 300
    )) %>%
    mutate(secs_passed_qtr = ifelse(period %in% c(1:4), 720 - secs_left_qtr, 300 - secs_left_qtr),  
           secs_passed_game = secs_passed_qtr + secs_start_qtr) %>%
    arrange(game_id, secs_passed_game) %>%
    filter(msg_type != 18) %>%     # instant replay
    group_by(game_id) %>%
    mutate(number_event = row_number()) %>%  # new numberEvent column with events in the right order
    ungroup() %>%
    select(-c(contains("remain"), secs_left_qtr, secs_start_qtr, secs_passed_qtr)) %>%
    arrange(game_id, number_event) %>%
    mutate(shot_pts = desc_value * ifelse(msg_type %in% c(1:3) & !str_detect(description, "Missed"), 1, 0)) %>%
    group_by(game_id) %>%
    mutate(hs = cumsum(coalesce(if_else(slug_team == team_home, shot_pts, 0), 0)),
           vs = cumsum(coalesce(if_else(slug_team == team_away, shot_pts, 0), 0))) %>%
    ungroup()
  
  ##First, track players subbed in or out in a quarter
  players_subbed <- nba_pbp %>%
    filter(msg_type == 8) %>%
    select(game_id, period, number_event, player_in = player2, player_out = player1, description) %>%
    pivot_longer(cols = starts_with("player"),
                 names_to = "in_out",
                 values_to = "player_name",
                 names_prefix = "player_") %>%
    arrange(game_id, period, number_event) %>%
    distinct(game_id, period, player_name, .keep_all = TRUE) %>%
    distinct(game_id, period, player_name, in_out) %>%
    #mutate(first_occ = !duplicated(player_name)) %>% # added this so starters only get marked when its the first occurrence of the name
    mutate(starter = ifelse(in_out == "out" , 1, 0)) 
  
  
  ##Then, find every player that contributed to PBP in a quarter and remove those that were subbed
  ##Everyone else played the entire quarter
  ##Finally, add the players that were subbed and started (first sub was out)
  starters_quarters <- nba_pbp %>%
    filter(!(msg_type == 6 & act_type %in% c(11, 12, 16, 18, 30))) %>%
    filter(!msg_type %in% c(9, 11)) %>% # timeout and ejection
    select(game_id, period, starts_with("player")) %>%
    pivot_longer(cols = starts_with("player")) %>%
    filter(!is.na(value), value != 0) %>%
    distinct(game_id, period, player_name = value) %>%
    anti_join(players_subbed) %>%
    bind_rows(players_subbed %>%
                filter(starter == 1)) %>%
    transmute(game_id, period, player_name) %>%
    left_join(player_logs %>%
                distinct(game_id = as.integer(game_id), player_name, slug_team = team_abbreviation))
  
  
  ##Check whether there are quarters when couldn't find 5 starters for team 
  ##This implies the player played entire quarter and didn't contribute to PBP
  qtrs_missing <- starters_quarters %>%
    count(game_id, period, slug_team) %>%
    filter(n != 5)
  
  # Find missing starter from period
  missing_st_qtr <- function(game_id_miss, period_miss){
    nba_boxscoretraditionalv2(game_id = game_id_miss, start_period = period_miss, 
                              end_period = period_miss, range_type = 1) %>%
      pluck("PlayerStats") %>%
      mutate(period = period_miss) %>%
      #    filter((period_miss > 4 & MIN == "5:00") | (period_miss <= 4 & MIN == "12:00")) %>%\
      filter(MIN == "5.000000:00" | MIN == "12:00") %>%
      mutate(across(c(FGM:PLUS_MINUS), as.integer)) %>%
      filter(FGA + FTA + REB + AST + STL + BLK + TO + PF == 0)
  }
  
  missing_starters <- map2_df(qtrs_missing$game_id, qtrs_missing$period, missing_st_qtr)
  #missing_starters <- map2_df(few_games, c(1:3), missing_st_qtr)
  ##Put together table with starters for every team in every period, adding missing starters
  if (nrow(missing_starters > 0)) {
    starters_quarters <- starters_quarters %>%
      bind_rows(missing_starters %>%
                  clean_names() %>%
                  transmute(game_id = as.numeric(game_id), period, player_name, 
                            slug_team = team_abbreviation)) %>%
      arrange(game_id, period, slug_team) %>%
      group_by(game_id, period, slug_team) %>%
      summarise(lineup_start = paste(sort(unique(player_name)), collapse = ", ")) %>% #create a string of the 5 players on the court
      ungroup() %>%
      left_join(player_logs %>%
                  distinct(game_id = as.integer(game_id), slug_team = team_abbreviation, team_location))
  } else {
    starters_quarters <- starters_quarters %>%
      arrange(game_id, period, slug_team) %>%
      group_by(game_id, period, slug_team) %>%
      summarise(lineup_start = paste(sort(unique(player_name)), collapse = ", ")) %>% #create a string of the 5 players on the court
      ungroup() %>%
      left_join(player_logs %>%
                  distinct(game_id = as.integer(game_id), slug_team = team_abbreviation, team_location))
  }
  ##starters_quarters %>% count(str_count(lineup_start, ","))
  
  ##Find lineup before and after every sub
  lineup_subs <- nba_pbp %>%
    filter(msg_type == 8) %>% #substitution
    left_join(starters_quarters) %>% 
    select(game_id, number_event, period, clock, slug_team, player_out = player1, player_in = player2, 
           team_location, lineup_before = lineup_start) %>%
    group_by(game_id, period, slug_team) %>%
    mutate(lineup_before = ifelse(row_number() == 1, lineup_before, NA)) %>% #note lineup to start quarter
    ungroup() %>%
    mutate(lineup_before = str_split(lineup_before, ", ")) %>% 
    arrange(game_id, number_event) %>%
    group_by(game_id, period, slug_team) %>%
    mutate(lineup_after = accumulate2(player_in, player_out, ~setdiff(c(..1, ..2), ..3), .init = lineup_before[[1]])[-1],
           lineup_before = coalesce(lineup_before, lag(lineup_after))) %>% #note the substitution change by looking at player in vs out and comparing it to the string of players in lineup before
    ungroup() %>% 
    mutate(across(starts_with("lineup"), ~ map_chr(., ~ paste(.x, collapse = ", "))))
  
  
  ##Add lineup to every event of PBP
  lineup_game <- nba_pbp %>%
    left_join(starters_quarters %>%#convert starters to one row per period and add msg_type 12 for an easy join
                select(-slug_team) %>%
                pivot_wider(names_from = team_location,
                            values_from = lineup_start,
                            names_prefix = "lineup_start_") %>%
                mutate(msg_type = 12)) %>% 
    left_join(lineup_subs %>%#repeat the same thing for subs, keep the lineups before and after in one row for both teams
                select(-c(clock, starts_with("player"))) %>%
                pivot_wider(names_from = team_location,
                            values_from = starts_with("lineup"))) %>% 
    mutate(across(c(lineup_before_home, lineup_after_home), ~ ifelse(!is.na(lineup_start_home), lineup_start_home, .)), #if there is a starting lineup use it for the before and away lineups for that row
           across(c(lineup_before_away, lineup_after_away), ~ ifelse(!is.na(lineup_start_away), lineup_start_away, .))) %>%
    group_by(game_id, period) %>%
    mutate(lineup_home = na.locf(lineup_after_home, na.rm = FALSE),#if lineup is missing, use the most recent observed lineup
           lineup_away = na.locf(lineup_after_away, na.rm = FALSE),
           lineup_home = coalesce(lineup_home, na.locf(lineup_before_home, fromLast = TRUE, na.rm = FALSE)),
           lineup_away = coalesce(lineup_away, na.locf(lineup_before_away, fromLast = TRUE, na.rm = FALSE))) %>%
    ungroup() %>%
    mutate(lineup_home = map_chr(str_split(lineup_home, ", "), ~ paste(sort(.), collapse = ", ")),
           lineup_away = map_chr(str_split(lineup_away, ", "), ~ paste(sort(.), collapse = ", "))) %>%
    select(-c(starts_with("lineup_start"), starts_with("lineup_before"), starts_with("lineup_after")))
  
  ###############
  ###############
  ##Possessions##
  ###############
  ###############
  
  poss_initial <- lineup_game %>%
    mutate(possession = case_when(msg_type %in% c(1, 2, 5) ~ 1, #field goals or change of possession
                                  msg_type == 3 & act_type %in% c(12, 15) ~ 1, #2nd out of 2 free throws or 3rd out of three
                                  TRUE ~ 0))
  
  ##Finding lane violations that are not specified
  lane_description_missing <- poss_initial %>%
    group_by(game_id, secs_passed_game) %>%
    filter(sum(msg_type == 3 & act_type == 10) > 0, #1 of 1 free throw
           sum(msg_type == 6 & act_type == 2) > 0, #shooting foul
           sum(msg_type == 7 & act_type == 3) > 0,
           sum(msg_type == 1) == 0) %>%
    ungroup() %>%
    mutate(possession = ifelse(msg_type == 3 & act_type == 10, 1, possession)) %>%
    select(game_id, number_event, off_slug_team, possession)
  
  ##Identify turnovers from successful challenge + jump ball that are not specified
  jumpball_turnovers <- poss_initial %>%
    group_by(game_id, period) %>%
    mutate(prev_poss = zoo::na.locf0(ifelse(possession == 1, off_slug_team, NA)),
           next_poss = zoo::na.locf0(ifelse(possession == 1, off_slug_team, NA), fromLast = TRUE)) %>%
    ungroup() %>%
    group_by(game_id, secs_passed_game) %>%
    mutate(team_reb_chall = sum(msg_type == 9) > 0 & sum(msg_type == 4 & is.na(player1)) > 0) %>% # identify if a timeout occurs at the same time as a team rebound
    ungroup() %>%
    filter(msg_type == 10 & act_type == 1 & 
             lag(msg_type) == 9 & #jump ball following timeout/challenge
             slug_team == lag(slug_team) & 
             prev_poss == next_poss &
             lag(team_reb_chall) == FALSE) %>%
    mutate(possession = 1) %>% #indicate a change of possession after these challenges
    transmute(game_id, number_event, off_slug_team = ifelse(slug_team == team_home, team_away, team_home), possession) %>%
    mutate(slug_team = off_slug_team)
  
  if (nrow(jumpball_turnovers) == 0 & nrow(lane_description_missing) == 0) {
    
    ##Identify and change consecutive possessions
    change_consec <- poss_initial %>%
      filter(possession == 1 | (msg_type == 6 & act_type == 30)) %>%
      group_by(game_id, period) %>%
      filter(possession == lead(possession) & off_slug_team == lead(off_slug_team)) %>%
      ungroup() %>%
      mutate(possession = 0) %>% #indicating that there is no change of possession off of rebounds, jump balls, and other potential reasons
      select(game_id, number_event, possession)
    
    ##Replace in data, update the possession column to indicate change of possession
    poss_non_consec <- poss_initial %>%
      rows_update(change_consec, by = c("game_id","number_event"))
  } else if (nrow(jumpball_turnovers) != 0 & nrow(lane_description_missing) == 0) {
    
    ##Identify and change consecutive possessions
    change_consec <- poss_initial %>%
      rows_update(jumpball_turnovers, by = c("game_id", "number_event")) %>%
      filter(possession == 1 | (msg_type == 6 & act_type == 30)) %>%
      group_by(game_id, period) %>%
      filter(possession == lead(possession) & off_slug_team == lead(off_slug_team)) %>%
      ungroup() %>%
      mutate(possession = 0) %>% #indicating that there is no change of possession off of rebounds, jump balls, and other potential reasons
      select(game_id, number_event, possession)
    
    ##Replace in data, update the possession column to indicate change of possession
    poss_non_consec <- poss_initial %>%
      rows_update(jumpball_turnovers, by = c("game_id", "number_event")) %>%
      rows_update(change_consec, by = c("game_id","number_event"))
    
  } else if (nrow(jumpball_turnovers) == 0 & nrow(lane_description_missing)!= 0) {
    
    ##Identify and change consecutive possessions
    change_consec <- poss_initial %>%
      rows_update(lane_description_missing, by = c("game_id", "number_event")) %>%
      filter(possession == 1 | (msg_type == 6 & act_type == 30)) %>%
      group_by(game_id, period) %>%
      filter(possession == lead(possession) & off_slug_team == lead(off_slug_team)) %>%
      ungroup() %>%
      mutate(possession = 0) %>% #indicating that there is no change of possession off of rebounds, jump balls, and other potential reasons
      select(game_id, number_event, possession)
    
    ##Replace in data, update the possession column to indicate change of possession
    poss_non_consec <- poss_initial %>%
      rows_update(lane_description_missing, by = c("game_id", "number_event")) %>%
      rows_update(change_consec, by = c("game_id","number_event"))
    
  } else {
    
    ##Identify and change consecutive possessions
    change_consec <- poss_initial %>%
      rows_update(lane_description_missing, by = c("game_id", "number_event")) %>%
      rows_update(jumpball_turnovers, by = c("game_id", "number_event")) %>%
      filter(possession == 1 | (msg_type == 6 & act_type == 30)) %>%
      group_by(game_id, period) %>%
      filter(possession == lead(possession) & off_slug_team == lead(off_slug_team)) %>%
      ungroup() %>%
      mutate(possession = 0) %>% #indicating that there is no change of possession off of rebounds, jump balls, and other potential reasons
      select(game_id, number_event, possession)
    
    ##Replace in data, update the possession column to indicate change of possession
    poss_non_consec <- poss_initial %>%
      rows_update(lane_description_missing, by = c("game_id", "number_event")) %>%
      rows_update(jumpball_turnovers, by = c("game_id", "number_event")) %>%
      rows_update(change_consec, by = c("game_id","number_event"))
    
  }
  
  ##Find start of possession/ or consecutive possessions
  #poss = 1 off of a make, turnover or jump ball, poss = 1 off of a made free throw
  #or rebound
  #or after 1 of 1 free throw
  #or away from play foul
  start_possessions <- poss_non_consec %>%
    filter((possession == 1 & (msg_type %in% c(1, 5, 10) | (msg_type == 3 & shot_pts > 0))) | (msg_type == 4 & act_type == 0 & desc_value == 0) | (msg_type == 3 & act_type == 10) | (msg_type == 6 & act_type == 6)) %>%
    filter(!(msg_type == 3 & act_type == 10 & lag(msg_type) == 6 & lag(act_type) == 6)) %>% #remove 1 of 1 free throw from an away from play foul
    filter(!(msg_type == 6 & act_type == 6)) %>% #remove away from play foul
    group_by(game_id, secs_passed_game, slug_team) %>%
    mutate(and1 = sum(msg_type == 1) > 0 & sum(msg_type == 3) > 0) %>% #and 1 indicator if its off of a made shot and theres a free throw
    ungroup() %>%
    mutate(start_poss = ifelse(and1 & msg_type %in% c(1, 3), NA, clock),
           number_event = ifelse(msg_type == 4, number_event, number_event + 1)) %>%
    ungroup() %>%
    filter(!is.na(start_poss))
  
  ##Add start of possession column to table
  poss_non_consec <- poss_non_consec %>%
    left_join(start_possessions %>%
                select(game_id, number_event, start_poss)) %>%
    group_by(game_id, period) %>%
    mutate(start_poss = ifelse(row_number() == 1, clock, start_poss),
           start_poss = na.locf(start_poss)) %>% # set the start possession time 
    ungroup()
  
  
  ##Adding extra possessions
  
  addit_poss <- poss_non_consec %>%
    filter(msg_type %in% c(1:5) & !(msg_type == 3 & act_type %in% c(16, 18:19, 20, 27:29, 25:26)) & !(msg_type == 4 & act_type == 1)) %>%# plays that are make, miss, free throws, rebounds, and turnovers, but can't be team rebounds or techincal or flagrant free throws
    group_by(game_id, period) %>%
    filter(row_number() == max(row_number())) %>% # last play of each quarter
    ungroup() %>%
    filter(clock != "00:00.0" & !(msg_type == 4 & desc_value == 1)) %>%
    transmute(game_id, period, start_poss = clock, possession = 1,
              off_slug_team = ifelse(msg_type == 4 | msg_type == 3 & act_type %in% c(19, 20, 29, 26), 
                                     slug_team, 
                                     ifelse(slug_team == team_home, team_away, team_home)),
              msg_type = 99, act_type = 0, number_original = 0, description = "Last possession of quarter") %>% #indicate the last possession
    left_join(poss_non_consec %>% #rejoin the original data of when the end of game occurs
                filter(msg_type == 13) %>% #select end of period
                select(-c(number_original, msg_type, act_type, start_poss,
                          description, possession, off_slug_team))) %>%
    mutate(number_event = number_event - 0.5, #make it the event before the end of games
           slug_team = off_slug_team)
  
  ##Adding extra possessions
  pbp_poss <- poss_non_consec %>%
    bind_rows(addit_poss) %>%
    arrange(game_id, number_event)
  
  
  ##Editing free throws PTS and poss position
  
  ##Connecting free throws to fouls
  
  ##Find unidentified double technical fouls 
  ##B/C instead of description showing double technical, there's one event for each but no FT's
  unident_double_techs <- lineup_game %>%
    filter(!msg_type %in% c(9, 11)) %>%   # ejection or timeout
    filter((game_id == lead(game_id) & secs_passed_game == lead(secs_passed_game) & msg_type == 6 & act_type == 11 & msg_type == lead(msg_type) & act_type == lead(act_type) & slug_team != lead(slug_team)) | (game_id == lag(game_id) & secs_passed_game == lag(secs_passed_game) & msg_type == 6 & act_type == 11 & msg_type == lag(msg_type) & act_type == lag(act_type) & slug_team != lag(slug_team))) %>%
    transmute(game_id, secs_passed_game, slug_team, number_event, description = str_replace(description, "Technical", "Double Technical"))
  
  techs <- lineup_game %>%
    rows_update(unident_double_techs, by = c("game_id", "secs_passed_game", "slug_team", "number_event")) %>% #wont work if there are no double techs
    filter(str_detect(description, "Technical|Defense 3 Second") & !str_detect(description, "Double Technical")) %>% #remove double techs keep all other
    group_by(game_id, secs_passed_game, msg_type) %>%
    mutate(sequence_num = row_number()) %>%
    ungroup() %>%
    transmute(game_id, secs_passed_game, number_event, msg_type = ifelse(msg_type == 3, "ft", "foul"), sequence_num) %>% #Indicate if event was ft or foul
    pivot_wider(names_from = msg_type,
                values_from = number_event,
                names_prefix = "number_event_")
  
  ##FLAGRANT - CLEAR PATH
  flagrant_clear <- lineup_game %>%
    filter(msg_type == 3 & act_type %in% c(18:20, 25:26, 27:29)) %>% #picking out flagrant free throws
    select(game_id, secs_passed_game, number_event_ft = number_event, slug_team) %>%
    left_join(lineup_game %>% #joining it to flagrant fouls
                filter(msg_type == 6 & act_type %in% c(9, 14, 15)) %>%
                transmute(game_id, secs_passed_game, number_event_foul = number_event, 
                          slug_team = ifelse(slug_team == team_home, team_away, team_home)))
  
  ##REGULAR FOULS
  other_fouls <- lineup_game %>% #regular fouls
    filter(msg_type %in% c(3, 6)) %>%
    filter(!str_detect(description, "Technical | Defense 3 Second"),
           !(msg_type == 3 & act_type %in% c(18:20, 25:26, 27:29)),
           !(msg_type == 6 & act_type %in% c(9, 14, 15)))
  
  regular_fouls <- other_fouls %>%
    filter(msg_type == 3) %>% #select free throws
    select(game_id, secs_passed_game, number_event_ft = number_event, slug_team, player_fouled = player1) %>%
    left_join(other_fouls %>%
                filter(msg_type == 6 & str_detect(description, "FT")) %>% #only fouls that lead to free throws
                transmute(game_id, secs_passed_game, number_event_foul = number_event, player_fouled = player3,
                          slug_team = ifelse(slug_team == team_home, team_away, team_home))) %>%
    left_join(other_fouls %>% #second join for when player fouled is diff than the player shooting free throws
                filter(msg_type == 6 & str_detect(description, "FT")) %>%
                transmute(game_id, secs_passed_game, number_event_foul_y = number_event, 
                          number_event_foul = NA,
                          slug_team = ifelse(slug_team == team_home, team_away, team_home))) %>%
    mutate(number_event_foul = coalesce(number_event_foul, number_event_foul_y)) %>%
    select(-number_event_foul_y)
  ##Putting everything together
  # keep a total per chain of possessions
  # track points, fta, and number of possessions
  fouls_stats <- bind_rows(regular_fouls, flagrant_clear, techs) %>%
    select(game_id, secs_passed_game, number_event_ft, number_event_foul) %>%
    left_join(pbp_poss %>%
                select(game_id, number_event_ft = number_event, slug_team, shot_pts, team_home, team_away, possession)) %>%
    # filter(is.na(shot_pts))  # test to see if there's nothing missing
    group_by(game_id, slug_team, number_event = number_event_foul, team_home, team_away) %>%
    summarise(total_fta = n(),
              total_pts = sum(shot_pts),
              total_poss = sum(possession)) %>%
    ungroup() %>% 
    mutate(shot_pts_home = ifelse(slug_team == team_home, total_pts, 0),
           shot_pts_away = ifelse(slug_team == team_away, total_pts, 0),
           poss_home = ifelse(slug_team == team_home, total_poss, 0),
           poss_away = ifelse(slug_team == team_away, total_poss, 0)) %>%
    select(game_id, number_event, total_fta, shot_pts_home:poss_away)
  
  
  #keep running track of results of possessions
  pbp_poss_final <- pbp_poss %>%
    # mutate(possession = ifelse(start_poss == "00:00.0", 0, possession)) %>%   # considering nba.com bug when play has wrong clock at 00:00.0 (correct would be to not have this line)
    left_join(fouls_stats) %>%
    mutate(shot_pts_home = coalesce(shot_pts_home, ifelse(msg_type == 1 & slug_team == team_home, shot_pts, 0)),#track points and possessions in each chain of possessions
           shot_pts_away = coalesce(shot_pts_away, ifelse(msg_type == 1 & slug_team == team_away, shot_pts, 0)),
           poss_home = coalesce(poss_home, ifelse(msg_type != 3 & possession == 1 & slug_team == team_home, possession, 0)),
           poss_away = coalesce(poss_away, ifelse(msg_type != 3 & possession == 1 & slug_team == team_away, possession, 0))) %>%
    group_by(game_id, period) %>%
    mutate(secs_played = lead(secs_passed_game) - secs_passed_game,
           secs_played = coalesce(secs_played, 0)) %>% #track duration of chain of posesssions
    ungroup() %>%
    left_join(player_logs %>%
                distinct(game_id = as.numeric(game_id), game_date = as.Date(game_date)))
  
  ##Add garbage time
  pbp_final_gt <- pbp_poss_final %>%
    left_join(starters_quarters %>%
                filter(period == 1) %>%
                select(-c(period, slug_team)) %>%
                pivot_wider(names_from = team_location,
                            values_from = lineup_start,
                            names_prefix = "lineup_start_")) %>%
    mutate(across(c(contains("lineup")), ~ str_split(., ", "), .names = "{.col}_list")) %>% #separate lineup string into vector
    mutate(total_starters_home = map_int(map2(lineup_home_list, lineup_start_home_list, intersect), length),
           total_starters_away = map_int(map2(lineup_away_list, lineup_start_away_list, intersect), length)) %>% #keep count of total number of starters
    select(-contains("list")) %>%
    mutate(margin_before = case_when(shot_pts > 0 & slug_team == team_home ~ abs(hs - shot_pts - vs),
                                     shot_pts > 0 & slug_team == team_away ~ abs(vs - shot_pts - hs),
                                     TRUE ~ abs(hs - vs))) %>%
    mutate(garbage_time = case_when( #criteria for garbage time indicator
      # score differential >= 25 for minutes 12-9:
      secs_passed_game >= 2160 & secs_passed_game < 2340 & margin_before >= 25 & total_starters_home + total_starters_away <= 2 & period == 4 ~ 1,
      # score differential >= 20 for minutes 9-6:
      secs_passed_game >= 2340 & secs_passed_game < 2520 & margin_before >= 20 & total_starters_home + total_starters_away <= 2 & period == 4 ~ 1,
      # score differential >= 10 for minutes 6 and under:
      secs_passed_game >= 2520 & margin_before >= 10 & total_starters_home + total_starters_away <= 2 & period == 4 ~ 1,
      TRUE ~ 0)) %>%
    group_by(game_id) %>%
    mutate(max_nongarbage = max(number_event[which(garbage_time == 0)])) %>%#number of events that occur outside of garbage time
    ungroup() %>%
    mutate(garbage_time = ifelse(garbage_time == 1 & number_event < max_nongarbage, 0, garbage_time)) %>% #cant be garbage time when the event number is less than total number of non garbage time events (Check this though)
    select(-c(starts_with("lineup_start_"), max_nongarbage, opt2, order, locX, locY))
  
  
  
  
  ######################
  ######################
  ##Lineup Level Stats##
  ######################
  ######################
  
  # lineup_stats <- pbp_final_gt %>%
  #   group_by(game_id, slug_team) %>%
  #   mutate(stint_home = ifelse(slug_team == team_home, cumsum(msg_type == 8) + 1, NA),
  #          stint_away = ifelse(slug_team == team_away, cumsum(msg_type == 8) + 1, NA)) %>% #keep track of each time subs are made leading to a different 5 player stint
  #   group_by(game_id) %>%
  #   mutate(across(starts_with("stint"), ~ na.locf0(., fromLast = TRUE)),
  #          across(starts_with("stint"), ~ na.locf(.))) %>%
  #   ungroup() %>% #keep track of which stint is on the floor
  #   pivot_longer(cols = starts_with("lineup"),
  #                names_to = "lineup_location",
  #                values_to = "lineup",
  #                names_prefix = "lineup_") %>%
  #   mutate(pts_team = ifelse(lineup_location == "home", shot_pts_home, shot_pts_away),
  #          pts_opp = ifelse(lineup_location == "away", shot_pts_home, shot_pts_away),
  #          poss_team = ifelse(lineup_location == "home", poss_home, poss_away),
  #          poss_opp = ifelse(lineup_location == "away", poss_home, poss_away),
  #          slug_team = ifelse(lineup_location == "home", team_home, team_away),
  #          slug_opp = ifelse(lineup_location == "away", team_home, team_away),
  #          stint = ifelse(lineup_location == "home", stint_home, stint_away)) %>% #keep track of events per possession
  #   select(game_id, game_date, period, stint, number_event, msg_type, description, lineup, pts_team, pts_opp,
  #          poss_team, poss_opp, secs_played, slug_team, slug_opp, garbage_time) %>%
  #   group_by(game_id, game_date, period, stint, slug_team, slug_opp, lineup, garbage_time) %>%
  #   summarise(across(c(pts_team, pts_opp, poss_team, poss_opp, secs_played), sum)) %>%
  #   ungroup() %>% #accumulate stats by stint
  #   filter(secs_played + poss_opp + poss_team + pts_opp + pts_team > 0) %>%
  #   #remove stints where no event occurs (check if we can move stints with 0 secs to prev stint)
  #   group_by(game_id, slug_team) %>%
  #   mutate(stint = row_number()) %>%
  #   ungroup() %>% 
  #   mutate(zero_secs = ifelse(slug_team == lag(slug_team) & secs_played == 0, TRUE, FALSE),
  #          pts_team = pts_team + ifelse(lead(zero_secs), lead(pts_team), 0),
  #          pts_opp = pts_opp + ifelse(lead(zero_secs), lead(pts_opp), 0),
  #          poss_team = poss_team + ifelse(lead(zero_secs), lead(poss_team), 0),
  #          poss_opp = poss_opp + ifelse(lead(zero_secs), lead(poss_opp), 0)) %>% 
  #   filter(secs_played != 0)
  
  return(pbp_final_gt)
}

 
# lineup_stats_2 <- lineup_level_stats(2022)
# 
# 
# lineup_stats_test <- lineup_stats %>% 
#   mutate(zero_secs = ifelse(slug_team == lag(slug_team) & secs_played == 0, TRUE, FALSE),
#          pts_team = pts_team + ifelse(lead(zero_secs), lead(pts_team), 0),
#          pts_opp = pts_opp + ifelse(lead(zero_secs), lead(pts_opp), 0),
#          poss_team = poss_team + ifelse(lead(zero_secs), lead(poss_team), 0),
#          poss_opp = poss_opp + ifelse(lead(zero_secs), lead(poss_opp), 0)) %>% 
#   filter(secs_played != 0)
# 
# View(lineup_stats_test %>% filter(slug_team == 'BOS') %>%
#        mutate(running_score_team = cumsum(pts_team), 
#               running_score_opp = cumsum(pts_opp)))
##################
# Building the adjusted RAPM Model
##################
#pbp_final_gt <- readRDS("C:/Users/dhaks/OneDrive/Desktop/SMU/STAT 6341/pbp_final_gt.RDS")
test <- lineup_level_stats(2022) %>%
  group_by(game_id, slug_team) %>%
  mutate(stint_home = ifelse(slug_team == team_home, cumsum(msg_type == 8) + 1, NA),
         stint_away = ifelse(slug_team == team_away, cumsum(msg_type == 8) + 1, NA)) %>% #keep track of each time subs are made leading to a different 5 player stint
  group_by(game_id) %>%
  mutate(across(starts_with("stint"), ~ na.locf0(., fromLast = TRUE)),
         across(starts_with("stint"), ~ na.locf(.))) %>%
  ungroup() %>% #keep track of which stint is on the floor
  pivot_longer(cols = starts_with("lineup"),
               names_to = "lineup_location",
               values_to = "lineup",
               names_prefix = "lineup_") %>%
  mutate(pts_team = ifelse(lineup_location == "home", shot_pts_home, shot_pts_away),
         pts_opp = ifelse(lineup_location == "away", shot_pts_home, shot_pts_away),
         poss_team = ifelse(lineup_location == "home", poss_home, poss_away),
         poss_opp = ifelse(lineup_location == "away", poss_home, poss_away),
         slug_team = ifelse(lineup_location == "home", team_home, team_away),
         slug_opp = ifelse(lineup_location == "away", team_home, team_away),
         stint = ifelse(lineup_location == "home", stint_home, stint_away)) %>% 
  select(game_id, game_date, period, stint, number_event, msg_type, description, lineup, pts_team, pts_opp,
         poss_team, poss_opp, secs_played, slug_team, slug_opp, garbage_time)

LineupIDs = data.frame("Lineups" = unique(test$lineup), "LineupID" = seq(1:length(unique(test$lineup))))

df.home = test[seq(1, nrow(test), 2), ] %>% 
  rename('Lineup_home' = 'lineup')
df.away = test[seq(2, nrow(test), 2), ] %>% 
  rename('Lineup_away' = 'lineup')

lineups_home_perspective <- df.home %>% 
  left_join(df.away %>% select(game_id, number_event, Lineup_away, secs_played)) %>% 
  group_by(game_id, game_date, period, stint, slug_team, slug_opp, Lineup_home, Lineup_away, garbage_time) %>%
  summarise(across(c(pts_team, pts_opp, poss_team, poss_opp, secs_played), sum)) %>%
  ungroup() %>% #accumulate stats by stint
  filter(secs_played + poss_opp + poss_team + pts_opp + pts_team > 0) %>%
  #remove stints where no event occurs (check if we can move stints with 0 secs to prev stint)
  group_by(game_id, slug_team) %>%
  mutate(stint = row_number()) %>%
  ungroup() %>% 
  mutate(zero_secs = ifelse(slug_team == lag(slug_team) & secs_played == 0, TRUE, FALSE),
         pts_team = pts_team + ifelse(lead(zero_secs), lead(pts_team), 0),
         pts_opp = pts_opp + ifelse(lead(zero_secs), lead(pts_opp), 0),
         poss_team = poss_team + ifelse(lead(zero_secs), lead(poss_team), 0),
         poss_opp = poss_opp + ifelse(lead(zero_secs), lead(poss_opp), 0)) %>% 
  filter(secs_played != 0) %>% 
  left_join(LineupIDs, by = c("Lineup_home" = "Lineups"))


lineups_away_perspective <- df.away %>% 
  left_join(df.home %>% select(game_id, number_event, Lineup_home, secs_played)) %>% 
  group_by(game_id, game_date, period, stint, slug_team, slug_opp, Lineup_home, Lineup_away, garbage_time) %>%
  summarise(across(c(pts_team, pts_opp, poss_team, poss_opp, secs_played), sum)) %>%
  ungroup() %>% #accumulate stats by stint
  filter(secs_played + poss_opp + poss_team + pts_opp + pts_team > 0) %>%
  #remove stints where no event occurs (check if we can move stints with 0 secs to prev stint)
  group_by(game_id, slug_team) %>%
  mutate(stint = row_number()) %>%
  ungroup() %>% 
  mutate(zero_secs = ifelse(slug_team == lag(slug_team) & secs_played == 0, TRUE, FALSE),
         pts_team = pts_team + ifelse(lead(zero_secs), lead(pts_team), 0),
         pts_opp = pts_opp + ifelse(lead(zero_secs), lead(pts_opp), 0),
         poss_team = poss_team + ifelse(lead(zero_secs), lead(poss_team), 0),
         poss_opp = poss_opp + ifelse(lead(zero_secs), lead(poss_opp), 0)) %>% 
  filter(secs_played != 0) %>% 
  relocate('Lineup_home', .after = 'Lineup_away') %>% 
  left_join(LineupIDs, by = c("Lineup_away" = "Lineups"))

avgs_home <- lineups_home_perspective %>% 
  group_by(game_id, Lineup_home, LineupID) %>% 
  summarize(points_scored = sum(pts_team),
            points_allowed = sum(pts_opp),
            possessions_for = sum(poss_team),
            possessions_against = sum(poss_opp),
            secs = sum(secs_played)) %>% 
  mutate(ORTG = points_scored * (100/possessions_for),
         DRTG = points_allowed * (100/possessions_against))

avgs_away <- lineups_away_perspective %>% 
  group_by(game_id, Lineup_away, LineupID) %>% 
  summarize(points_scored = sum(pts_team),
            points_allowed = sum(pts_opp),
            possessions_for = sum(poss_team),
            possessions_against = sum(poss_opp),
            secs = sum(secs_played)) %>% 
  mutate(ORTG = points_scored * (100/possessions_for),
         DRTG = points_allowed * (100/possessions_against))

all_avgs <- rbind(avgs_home %>% rename("lineup" = 'Lineup_home'), 
                  avgs_away %>% rename("lineup" = 'Lineup_away')) %>% 
  arrange(game_id) %>% 
  group_by(lineup) %>% 
  mutate(ORTG_MA = cummean(ORTG), #cumsum for possessions and points scored
         DRTG_MA = cummean(DRTG),
         ORTG_RA = lag(ORTG_MA),
         DRTG_RA = lag(DRTG_MA),
         game_number = n()) %>% 
  ungroup()

combined_lineups <- rbind(lineups_home_perspective %>% 
                            rename('Lineup_team' = 'Lineup_home',
                                   'Lineup_opp' = 'Lineup_away',
                                   'LineupID_team' = 'LineupID'),
                          lineups_away_perspective%>% 
                            rename('Lineup_team' = 'Lineup_away',
                                   'Lineup_opp' = 'Lineup_home',
                                   'LineupID_team' = 'LineupID')) %>% 
  left_join(LineupIDs, by = c('Lineup_opp' = 'Lineups')) %>% 
  rename('LineupID_opp' = 'LineupID') %>% 
  left_join(all_avgs %>% select(game_id, LineupID, ORTG_RA, DRTG_RA),
            by = join_by(game_id == game_id,  LineupID_team == LineupID )) %>% 
  rename('ORTG_RA_team' = 'ORTG_RA',
         'DRTG_RA_team' = 'DRTG_RA') %>% 
  left_join(all_avgs %>% select(game_id, LineupID, ORTG_RA, DRTG_RA),
            by = join_by(game_id == game_id,  LineupID_opp == LineupID )) %>% 
  rename('ORTG_RA_opp' = 'ORTG_RA',
         'DRTG_RA_opp' = 'DRTG_RA') %>% 
  select(game_id:Lineup_team, LineupID_team, ORTG_RA_team:DRTG_RA_team, 
         Lineup_opp, LineupID_opp, ORTG_RA_opp:DRTG_RA_opp,
         garbage_time:zero_secs)%>% 
  separate_wider_delim(cols = Lineup_team, delim = ',',
                       names = c('P1', 'P2', 'P3', 'P4', 'P5')) %>% 
  separate_wider_delim(cols = Lineup_opp, delim = ',',
                       names = c('DP1', 'DP2', 'DP3', 'DP4', 'DP5')) %>% 
  mutate(across(P1:P5, str_trim)) %>% 
  mutate(across(DP1:DP5, str_trim)) %>% 
  mutate(netrt = (100/(poss_team + poss_opp))*(pts_team-pts_opp),
         ORTG = (100/poss_team)*pts_team,
         DRTG = (100/poss_opp)*pts_opp) %>% 
  filter(!is.na(netrt)) %>% 
  filter(!is.infinite(netrt)) %>% 
  filter(!(is.na(ORTG_RA_opp) | is.na(ORTG_RA_team) |
             is.na(DRTG_RA_opp) | is.na(DRTG_RA_team)))#checck

set.seed(1)
#combined_lineups <- combined_lineups %>% slice_sample(n = 10, by = game_id)

unique_players <- unique(c(combined_lineups$P1, combined_lineups$P2,
                           combined_lineups$P3, combined_lineups$P4, 
                           combined_lineups$P5, combined_lineups$DP1,
                           combined_lineups$DP2, combined_lineups$DP3,
                           combined_lineups$DP4, combined_lineups$DP5))

player_matrices <- function(current_lineup, all_players){
  x <- rep(0, length(all_players))
  x[which(all_players==current_lineup[1])] = 1
  x[which(all_players==current_lineup[2])] = 1
  x[which(all_players==current_lineup[3])] = 1
  x[which(all_players==current_lineup[4])] = 1
  x[which(all_players==current_lineup[5])] = 1
  x[which(all_players==current_lineup[6])] = -1
  x[which(all_players==current_lineup[7])] = -1
  x[which(all_players==current_lineup[8])] = -1
  x[which(all_players==current_lineup[9])] = -1
  x[which(all_players==current_lineup[10])] = -1
  return(x)
}
x <- nrow(combined_lineups)

firstdf <- combined_lineups[(x/2+1):x , ]

sample <- t(apply(combined_lineups %>% select(P1:P5, DP1:DP5), 1, function(x){player_matrices(x, unique_players)}))
sample <- as(as.matrix(sample), 'dgCMatrix')
sample <- cbind(sample, combined_lineups$ORTG_RA_team, combined_lineups$DRTG_RA_team, combined_lineups$ORTG_RA_opp, combined_lineups$DRTG_RA_opp)

trial1 <- cv.glmnet(sample, combined_lineups$netrt, alpha = 0, nfolds = 10, standardize = FALSE)
model_w_coeff <- glmnet(sample, combined_lineups$netrt, alpha = 0, standardize = FALSE, lambda = trial1$lambda.min)
Rapm <- model_w_coeff$beta
Rapm <- as.matrix(Rapm)
unique_players <- append(unique_players, c('ORTG_RA_team','DRTG_RA_team', 'ORTG_RA_opp', 'DRTG_RA_opp'))
final <- data.frame("Player Name" = unique_players, "RAPM" = Rapm)
final$rank <- dense_rank(desc(final$s0))
