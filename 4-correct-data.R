# notes ------------------------------------------------------------------------
# Author: Kevin Ferris
# Date: 2023-11-10
# 

# setup ------------------------------------------------------------------------

# R setup
options(stringsAsFactors = F, digits = 3, show.signif.stars = F, tidyverse.quiet = T, mc.cores = 4)
set.seed(42)

# package setup
library(tidyverse)
library(lme4)
library(furrr)
plan(multisession, workers = getOption("mc.cores"))

source("2023-11-02-app/0-helper-funs.R")
df_pa <- readRDS("2023-11-02-app/data/3-pa_data.RDS")
df_bip <- readRDS("2023-11-02-app/data/3-bip_data.RDS")
df_pitch_agg <- readRDS("2023-11-02-app/data/3-pitch_data_agg.RDS")

# models adjusting for context -------------------------------------------------

# data to fit set of models to each season/level
df_nested <- df_pa %>% 
  select(season, level) %>% 
  distinct() %>% 
  group_by(season, level) %>% 
  mutate(pa_dat = map2(season, level, ~ filter(df_pa, season == .x, level == .y)), 
         bip_dat = map2(season, level, ~ filter(df_bip, season == .x, level == .y)), 
         pitch_dat = map2(season, level, ~ filter(df_pitch_agg, season == .x, level == .y))) %>% 
  ungroup()
rm(df_pa, df_bip, df_pitch_agg)
gc()

# fitting models in parallel
df_results <- df_nested %>% 
  mutate(m = future_pmap(list(pa_dat, bip_dat, pitch_dat), 
                         function(d1, d2, d3) fit_context_models(d1, d2, d3)))

# extracting and saving results
ranefs_extracted <- df_results %>% 
  mutate(b = map(m, ~ .x$bat_ranefs), 
         p = map(m, ~ .x$pit_ranefs))
ranefs_bat <- ranefs_extracted %>% 
  select(season, level, b) %>% 
  unnest(b)
ranefs_pit <- ranefs_extracted %>% 
  select(season, level, p) %>% 
  unnest(p)


# saving -----------------------------------------------------------------------

saveRDS(ranefs_bat, "2023-11-02-app/data/4-stat_ranefs_bat.RDS")
saveRDS(ranefs_pit, "2023-11-02-app/data/4-stat_ranefs_pit.RDS")


