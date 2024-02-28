
# notes ------------------------------------------------------------------------
# Author: Kevin Ferris
# Date: 2023-11-22
# 

# setup ------------------------------------------------------------------------

# R setup
options(stringsAsFactors = F, digits = 3, show.signif.stars = F, tidyverse.quiet = T, mc.cores = 4)
set.seed(42)

# package setup
library(tidyverse)
library(here)

df_pa <- readRDS(file.path(here(), "data/3-pa_data.RDS"))
df_bip <- readRDS(file.path(here(), "data/3-bip_data.RDS"))
df_pitch <- readRDS(file.path(here(), "data/3-pitch_data.RDS"))

# aggregating for batters ------------------------------------------------------

# pa stats
df_bat_pa <- df_pa %>% 
  group_by(batter_id, season, level) %>% 
  mutate(n_orgs = n_distinct(bat_org)) %>% 
  summarise(
    org = unique(ifelse(n_orgs > 1, "--", bat_org)), 
    pa = n(), 
    kpct = mean(pa_outcome == "k"), 
    bbpct = mean(pa_outcome == "bb"), 
    raa600 = mean(raa600), 
    avg = sum(pa_outcome %in% c("b1", "b2", "b3", "hr")) / sum(pa_outcome != "bb"), 
    obp = mean(pa_outcome %in% c("b1", "b2", "b3", "hr", "bb")), 
    slg = sum(case_when(pa_outcome == "b1" ~ 1, 
                        pa_outcome == "b2" ~ 2, 
                        pa_outcome == "b3" ~ 3, 
                        pa_outcome == "hr" ~ 4, 
                        pa_outcome == "bip_out" ~ 0, 
                        TRUE ~ NA_real_), na.rm = T) / 
      sum(pa_outcome != "bb"), 
    babip = sum(pa_outcome %in% c("b1", "b2", "b3")) / sum(pa_outcome %in% c("b1", "b2", "b3", "bip_out")), 
    iso = slg - avg, 
    .groups = "drop"
  ) %>% 
  arrange(desc(season), desc(level))
df_bat_pa %>% filter(batter_id == 545361)

# bip stats
df_bat_bip <- df_bip %>% 
  group_by(batter_id, season, level) %>% 
  summarise(
    bip = n(), 
    ev_avg = mean(ifelse(valid_tm, ev, NA), na.rm = T), 
    hh95 = mean(ifelse(valid_tm, ev, NA) >= 95, na.rm = T), 
    max_ev = max(ev, na.rm = T), 
    gbpct = mean(hit_type == "ground_ball"), 
    driven = mean(ifelse(valid_tm, ev >= 95 & between(la, 10, 40), NA), na.rm = T), 
    .groups = "drop"
  ) %>% 
  mutate(max_ev = ifelse(max_ev == -Inf, NA, max_ev)) %>% 
  arrange(desc(season), desc(level))

# pitch level stats
df_bat_pitch <- df_pitch %>% 
  filter(pitchresult != "PO") %>% 
  group_by(batter_id, season, level) %>% 
  summarise(
    n_pit = n(), 
    iz_swing = mean(ifelse(in_zone, swing, NA), na.rm = T), 
    chase = mean(ifelse(in_zone, NA, swing), na.rm = T), 
    contact = sum(contact) / sum(swing), 
    .groups = "drop"
  ) %>% 
  arrange(desc(season), desc(level))

# aggregating for pitchers ------------------------------------------------------

# pa stats
df_pit_pa <- df_pa %>% 
  mutate(is_platoon = bathand != pithand, 
         platoon_raa = ifelse(is_platoon, raa600, NA), 
         same_raa = ifelse(!is_platoon, raa600, NA)) %>% 
  group_by(pitcher_id, season, level) %>% 
  mutate(n_orgs = n_distinct(pit_org)) %>% 
  summarise(
    org = unique(ifelse(n_orgs > 1, "--", pit_org)), 
    pa = n(), 
    kpct = mean(pa_outcome == "k"), 
    bbpct = mean(pa_outcome == "bb"), 
    raa600_platoon = mean(platoon_raa, na.rm = T), 
    raa600_same = mean(same_raa, na.rm = T), 
    raa700 = mean(raa600) * 7 / 6, 
    .groups = "drop"
  ) %>% 
  mutate(raa700_split = (raa600_same - raa600_platoon) * 7 / 6) %>% 
  arrange(desc(season), desc(level))

# bip stats
df_pit_bip <- df_bip %>% 
  group_by(pitcher_id, season, level) %>% 
  summarise(
    bip = n(), 
    ev_avg = mean(ifelse(valid_tm, ev, NA), na.rm = T), 
    gbpct = mean(hit_type == "ground_ball"), 
    driven = mean(ifelse(valid_tm, ev >= 95 & between(la, 10, 40), NA), na.rm = T), 
    .groups = "drop"
  ) %>% 
  arrange(desc(season), desc(level))

# pitch level stats
df_pit_pitch <- df_pitch %>% 
  filter(pitchresult != "PO") %>% 
  group_by(pitcher_id, season, level) %>% 
  summarise(
    n_pit = n(), 
    iz = mean(in_zone), 
    contact = sum(contact) / sum(swing), 
    .groups = "drop"
  ) %>% 
  arrange(desc(season), desc(level))

# MLB average pich metrics
df_pit_metrics_mlb <- df_pitch %>% 
  filter(level == "MLB", 
         season == max(season), 
         !is.na(velo), 
         pitch_type %in% c("2FB", "4FB", "CB", "CH", "CT", "SL")) %>% 
  group_by(pithand, pitch_type) %>% 
  summarise(across(c(velo, hb, ivb), mean), .groups = "drop")

# pitch metrics
df_pit_metrics <- df_pitch %>% 
  filter(game_date >= Sys.Date() - 365, !is.na(velo)) %>% 
  group_by(pitcher_id, pithand, pitch_type) %>% 
  summarise(
    n_pre2k = sum(strikes < 2), 
    n_2k = sum(strikes >= 2), 
    n_vl = sum(bathand == "L"), 
    n_vr = sum(bathand == "R"), 
    n_swing = sum(swing), 
    n_miss = sum(contact), 
    velo = mean(velo), 
    hb = mean(hb), 
    ivb = mean(ivb), 
    raa700 = 7 / 6 * mean(pitch_raa600, na.rm = T), 
    .groups = "drop"
  ) %>% 
  group_by(pitcher_id) %>% 
  mutate(pct_pre2k = n_pre2k / sum(n_pre2k), 
         pct_2k = n_2k / sum(n_2k), 
         pct_vl = n_vl / sum(n_vl), 
         pct_vr = n_vr / sum(n_vr), 
         pct_miss = n_miss / n_swing) %>% 
  ungroup() %>% 
  arrange(pitcher_id, pitch_type)
df_pit_metrics_bip <- df_bip %>% 
  filter(game_date >= Sys.Date() - 365) %>% 
  group_by(pitcher_id, pitch_type) %>% 
  summarise(
    n_has_ev = sum(!is.na(ev)), 
    ev = mean(ev, na.rm = T), 
    pct_gb = mean(hit_type == "line_drive", na.rm = T), 
    .groups = "drop"
  ) %>% 
  mutate(ev = ifelse(n_has_ev <= 50, NA, ev))
  

# combining --------------------------------------------------------------------

### batter data
df_bat_results <- df_bat_pa %>% 
  mutate(bat = raa600 / 600 * pa) %>% 
  select(batter_id, season, level, org, pa, bat, raa600, 
         kpct, bbpct, iso, babip, avg, obp, slg) %>% 
  mutate(across(c(bat, raa600), ~ round(.x, 1)), 
         across(c(kpct, bbpct, iso, babip, avg, obp, slg), ~ round(.x, 3)))
df_bat_process <- df_bat_bip %>% 
  full_join(df_bat_pitch, by = c("batter_id", "season", "level")) %>% 
  select(batter_id, season, level, iz_swing, chase, contact, ev_avg, hh95, max_ev, gbpct, driven) %>% 
  mutate(across(c(ev_avg, max_ev), ~ round(.x, 1)), 
         across(c(iz_swing, chase, contact, hh95, gbpct, driven), ~ round(.x, 3)))

### pitcher data
df_pit_stats <- df_pit_pa %>% 
  full_join(df_pit_bip, by = c("pitcher_id", "season", "level")) %>% 
  full_join(df_pit_pitch, by = c("pitcher_id", "season", "level")) %>% 
  select(pitcher_id, season, level, org, bf = pa, raa700, raa700_split, kpct, bbpct, iz, contact, ev_avg, gbpct, driven) %>% 
  mutate(across(c(raa700, raa700_split, ev_avg), ~ round(.x, 1)), 
         across(c(kpct, bbpct, iz, contact, gbpct), ~ round(.x, 3)))
df_pit_metrics_display <- df_pit_metrics  %>% 
  left_join(df_pit_metrics_bip, by = c("pitcher_id", "pitch_type")) %>% 
  left_join(df_pit_metrics_mlb, by = c("pithand", "pitch_type"), suffix = c("", "_mlb")) %>% 
  mutate(across(c(velo, hb, ivb, velo_mlb, hb_mlb, ivb_mlb), ~ round(.x, 1)), 
         across(c(pct_pre2k, pct_2k), ~ round(.x, 3))) %>% 
  mutate(velo_label = paste0(velo, " (", velo_mlb, ")"), 
         hb_label = paste0(hb, " (", hb_mlb, ")"), 
         ivb_label = paste0(ivb, " (", ivb_mlb, ")")) %>% 
  select(pitcher_id, pitch_type, pct_vl, pct_vr, 
         velo_label, ivb_label, hb_label, 
         raa700, pct_miss, ev, pct_gb)


# saving
save(df_bat_results, df_bat_process, file = file.path(here(), "data/5-batter_aggregated.RData"))
save(df_pit_stats, df_pit_metrics_display, file = file.path(here(), "data/5-pitcher_aggregated.RData"))





