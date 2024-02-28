

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
library(tidyr)
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

# count rvs from here https://www.thedrummeyangle.com/post/what-is-pitch-run-value-and-why-is-it-important
count_rvs <- tibble(read.table(text = "
Situation,wOBA,count_rv,strike_rv,ball_rv
3-0,0.622,0.239,-0.117,0.051
3-1,0.470,0.123,-0.066,0.168
2-0,0.436,0.097,-0.062,0.143
3-2,0.384,0.057,-0.294,0.234
1-0,0.355,0.035,-0.035,0.088
2-1,0.352,0.032,-0.069,0.064
0-0,0.310,0.000,-0.037,0.032
1-1,0.293,-0.013,-0.054,0.048
2-2,0.273,-0.028,-0.209,0.085
0-1,0.262,-0.037,-0.051,0.024
1-2,0.223,-0.067,-0.171,0.038
0-2,0.196,-0.087,-0.150,0.021
", header = T, skip = 1, sep = ",")) %>% 
  separate(Situation, c("balls", "strikes"), sep = "-", convert = T)
pa_rvs <- tribble(
  ~pa_outcome, ~pa_rv, 
  "k", -0.28, 
  "bb", 0.32, 
  "b1", 0.47, 
  "b2", 0.77, 
  "b3", 1.04, 
  "hr", 1.40, 
  "bip_out", -0.25
)

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
                           TRUE ~ 0)) %>% 
  group_by(season, level, home_league_id) %>% 
  mutate(raa600 = raa600 - mean(raa600, na.rm = T)) %>%
  ungroup()

# bip level data ---------------------------------------------------------------

df_bip <- df_all %>% 
  filter(hit_type %in% c("", "ground_ball", "line_drive", "fly_ball", "popup")) %>% 
  mutate(valid_tm = has_tm & (hit_type != "bunt")) %>% 
  select(game_pk, game_date, season, level, home_league_id, play_id, home_team, 
         inning, ab_index, batter_id, pitcher_id, bathand, pithand, outs, pitch_type, 
         pa_outcome, hit_type, valid_tm, ev, la, dist, spray_chart) %>% 
  distinct()

# pitch level data -------------------------------------------------------------

df_pitch <- df_all %>% 
  mutate(strikes = pmin(2, strikes), 
         balls = pmin(3, balls)) %>% 
  left_join(count_rvs, by = c("balls", "strikes")) %>% 
  left_join(pa_rvs, by = c("pa_outcome")) %>% 
  mutate(in_zone = between(coalesce(px, px_chart), -1.5, 1.5) & between(coalesce(pz, pz_chart), 1.5, 4.5), 
         swing = pitchresult %in% c("F", "H", "S"), 
         contact = pitchresult %in% c("F", "H"), 
         pitch_raa600 = -600 * case_when(strikes == 2 & pitchresult == "F" ~ 0, 
                                        pitchresult %in% c("F", "S", "T") ~ strike_rv, 
                                        pitchresult %in% c("B", "PO") ~ ball_rv, 
                                        pitchresult == "H" ~ pa_rv - count_rv, 
                                        TRUE ~ NA_real_)) %>% 
  group_by(season, level, home_league_id) %>% 
  mutate(pitch_raa600 = pitch_raa600 - mean(pitch_raa600, na.rm = T)) %>% 
  ungroup() %>% 
  select(game_pk, game_date, season, level, home_league_id, play_id, home_team, 
         inning, ab_index, batter_id, pitcher_id, bathand, pithand, outs, balls, strikes, 
         pitchresult, pitch_type, velo, spin, ext, bx, bz, ivb, hb, px, pz, px_chart, pz_chart, 
         in_zone, swing, contact, pitch_raa600)

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



