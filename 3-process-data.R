

# notes ------------------------------------------------------------------------
# Author: Kevin Ferris
# Date: 2023-10-29
# 

# setup ------------------------------------------------------------------------

# R setup
options(stringsAsFactors = F, digits = 3, show.signif.stars = F, tidyverse.quiet = T, mc.cores = 4)
set.seed(42)

# package setup
library(dplyr)
library(here)

df_mlb <- readRDS(file.path(here(), "data/2-cleaned_data_mlb.RDS"))
df_aaa <- readRDS(file.path(here(), "data/2-cleaned_data_aaa.RDS"))
df_aa <- readRDS(file.path(here(), "data/2-cleaned_data_aa.RDS"))
df_higha <- readRDS(file.path(here(), "data/2-cleaned_data_higha.RDS"))
df_lowa <- readRDS(file.path(here(), "data/2-cleaned_data_lowa.RDS"))
df_rookie <- readRDS(file.path(here(), "data/2-cleaned_data_rookie.RDS"))
df_all <- bind_rows(df_mlb, df_aaa, df_aa, df_higha, df_lowa, df_rookie)
rm(df_mlb, df_aaa, df_aa, df_higha, df_lowa, df_rookie)
gc()

# pa level data ----------------------------------------------------------------

df_pa <- df_all %>% 
  select(game_pk, game_date, season, level, home_league_id, home_team, bat_org, pit_org, 
         inning, ab_index, batter_id, pitcher_id, bathand, pithand, outs, pa_outcome) %>% 
  distinct() %>% 
  filter(pa_outcome %in% c("k", "bb", "b1", "b2", "b3", "hr", "bip_out")) %>% 
  mutate(raa600 = 600 * case_when(pa_outcome == "k" ~ -0.28, 
                                  pa_outcome == "bb" ~ 0.32, 
                                  pa_outcome == "b1" ~ 0.47,  
                                  pa_outcome == "b2" ~ 0.77,  
                                  pa_outcome == "b3" ~ 1.04,  
                                  pa_outcome == "hr" ~ 1.40,  
                                  pa_outcome == "bip_out" ~ -0.25, 
                                  TRUE ~ NA_real_), 
         is_ab = pa_outcome != "bb", 
         is_hit = pa_outcome %in% c("b1", "b2", "b3", "hr"), 
         is_obp = pa_outcome %in% c("b1", "b2", "b3", "hr", "bb"), 
         is_platoon = bathand == pithand, 
         bases = case_when(pa_outcome == "b1" ~ 1, 
                           pa_outcome == "bb" ~ 1, 
                           pa_outcome == "b2" ~ 2, 
                           pa_outcome == "b3" ~ 3, 
                           pa_outcome == "hr" ~ 4, 
                           TRUE ~ 0))

# bip level data ---------------------------------------------------------------

df_bip <- df_all %>% 
  filter(hit_type %in% c("", "ground_ball", "line_drive", "fly_ball", "popup")) %>% 
  mutate(valid_tm = has_tm & (hit_type != "bunt")) %>% 
  select(game_pk, game_date, season, level, home_league_id, play_id, home_team, 
         inning, ab_index, batter_id, pitcher_id, bathand, pithand, outs, 
         pa_outcome, hit_type, valid_tm, ev, la, dist, spray_chart) %>% 
  distinct()

# pitch level data -------------------------------------------------------------

df_pitch <- df_all %>% 
  select(game_pk, game_date, season, level, home_league_id, play_id, home_team, 
         inning, ab_index, batter_id, pitcher_id, bathand, pithand, outs, balls, strikes, 
         pitchresult, pitch_type, velo, spin, ext, bx, bz, ivb, hb, px, pz, px_chart, pz_chart) %>% 
  mutate(in_zone = between(coalesce(px, px_chart), -1.5, 1.5) & between(coalesce(pz, pz_chart), 1.5, 4.5), 
         swing = pitchresult %in% c("F", "H", "S"), 
         contact = pitchresult %in% c("F", "H"))

# aggregating pitch data to speed up computation
df_pitch_agg <- df_pitch %>% 
  group_by(season, level, home_league_id, bathand, pithand, batter_id, pitcher_id, home_team) %>% 
  summarise(
    n_pitches = n(), 
    n_iz = sum(in_zone), 
    n_ooz = n_pitches - n_iz, 
    n_swing = sum(swing), 
    n_iz_swing = sum(swing & in_zone), 
    n_ooz_swing = sum(swing & !in_zone), 
    n_contact = sum(contact), 
    .groups = "drop"
  )

# saving -----------------------------------------------------------------------

saveRDS(df_pa, file.path(here(), "data/3-pa_data.RDS"))
saveRDS(df_bip, file.path(here(), "data/3-bip_data.RDS"))
saveRDS(df_pitch, file.path(here(), "data/3-pitch_data.RDS"))
saveRDS(df_pitch_agg, file.path(here(), "data/3-pitch_data_agg.RDS"))

# cole <- filter(df_pitch, pitcher_id == 543037)
# write_csv(cole, file.path(here(), "data/3-cole_pitch_data.csv")



