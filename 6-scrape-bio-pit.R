

# notes ------------------------------------------------------------------------
# Author: Kevin Ferris
# Date: 2023-10-29
# 

# setup ------------------------------------------------------------------------

# R setup
options(stringsAsFactors = F, digits = 3, show.signif.stars = F, tidyverse.quiet = T, mc.cores = 4)
set.seed(42)

# package setup
library(here)
library(dplyr)
library(purrr)
library(baseballr)
library(mlbplotR)

# data setup
current_season <- 2023
load("2023-11-02-app/data/5-pitcher_aggregated.RData")
ids <- sort(unique(df_pit_stats$pitcher_id))
source(file.path(here(), "2023-11-02-app/0-helper-funs.R"))

# scraping with baseballr and mlbplotr -----------------------------------------

headshots <- load_headshots()
team_logos <- load_mlb_teams() %>% filter(!is.na(team_id_num))
pit_bio <- map(ids, mlb_people, .progress = T)
draft <- list_rbind(map(2002:current_season, mlb_draft, .progress = T))
player_team <- mlb_sports_players(sport_id = 1, season = current_season)
team_org <- mlb_teams(season = current_season)

# saving -----------------------------------------------------------------------

pit_bio_save <- pit_bio %>% 
  left_join(headshots, by = c("id" = "savant_id"), suffix = c("", "_y")) %>% 
  # left_join(draft, by = c("id" = "person_id"), suffix = c("", "_z")) %>% 
  # joins to map player -> org for org logos
  left_join(select(player_team, player_id, current_team_id), by = c("id" = "player_id")) |> 
  left_join(select(team_org, team_id, parent_org_id), by = c("current_team_id" = "team_id")) |> 
  mutate(parent_org_id = coalesce(parent_org_id, current_team_id)) %>% # weird bug for Miggy
  left_join(select(team_org, team_id, team_abbreviation), by = c("parent_org_id" = "team_id")) |> 
  left_join(select(team_logos, team_id_num, team_logo_espn), by = c("parent_org_id" = "team_id_num")) |>
  mutate(name_age_id = paste0(full_name, " (", current_age, ", ID: ", id, ")"), 
         bt = paste(bat_side_code, pitch_hand_code, sep = "/")) %>% 
  select(id, full_name, org = team_abbreviation, 
         primary_number, birth_date, current_age, height, weight, draft_year, 
         pos = primary_position_abbreviation, bt, espn_headshot, team_logo_espn, name_age_id)

saveRDS(pit_bio_save, file = "2023-11-02-app/data/6-person_bio_pit.rds")
