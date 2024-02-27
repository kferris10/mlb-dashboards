

# notes ------------------------------------------------------------------------
# Author: Kevin Ferris
# Date: 2024-01-15
# 

# setup ------------------------------------------------------------------------

# R setup
options(stringsAsFactors = F, digits = 3, show.signif.stars = F, tidyverse.quiet = T, mc.cores = 4)
set.seed(42)

# package setup
library(tidyverse)
library(here)

df_pitch <- readRDS(file.path(here(), "data/3-pitch_data.RDS"))

# grab recent pitches for each pitcher -----------------------------------------

# extract last 200 pitches from the last 365 days
# if pitcher hasn't thrown in last 365, they won't have data here
df_recent <- df_pitch %>% 
  # filter to last 365 + most recent 200
  filter(game_date >= Sys.Date() - 365, !is.na(pitch_type)) %>% 
  arrange(pitcher_id, desc(game_date), desc(ab_index)) %>% 
  group_by(pitcher_id, bathand) %>% 
  mutate(r = 1:n()) %>% 
  ungroup() %>% 
  filter(r <= 200) %>% 
  # filter out < 10% used pitches
  group_by(pitcher_id, bathand) %>%
  mutate(n = n()) %>%
  group_by(pitcher_id, pitch_type, bathand) %>%
  mutate(n_pit = n()) %>%
  ungroup() %>%
  filter(n_pit / n >= .1) %>%
  # clean
  mutate(px_clean = coalesce(px, px_chart), 
         pz_clean = coalesce(pz, pz_chart), 
         spin = round(spin / 10) * 10, 
         across(c(velo, bx, bz, ivb, hb), ~ round(.x, 1)), 
         across(c(ext, px_clean, pz_clean), ~ round(.x, 1))) %>% 
  select(game_pk, game_date, level, play_id, pitcher_id, bathand, pithand, 
         pitch_type:hb, px_clean, pz_clean)

# calculate MLB average pitch metrics 
df_mlb_avg <- df_recent %>% 
  filter(level == "MLB") %>% 
  group_by(level, pithand, pitch_type) %>% 
  summarise(across(velo:hb, ~ mean(.x, na.rm = T)), .groups = "drop") %>% 
  mutate(spin = round(spin / 10) * 10)

# saving -------------------------------------------------------------

save(df_recent, df_mlb_avg, file = file.path(here(), "data/5-pitcher-recent-pitches.RData"))


