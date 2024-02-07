

# notes ------------------------------------------------------------------------
# Author: Kevin Ferris
# Date: 2023-10-29
# takes 1-2 hours per level-season

# setup ------------------------------------------------------------------------

# R setup
options(stringsAsFactors = F, digits = 3, show.signif.stars = F, tidyverse.quiet = T, mc.cores = 4)
set.seed(42)

# package setup
library(tidyverse)
library(baseballr)
library(progress)

# params
params <- list(
  start_date = "2021-03-01" , 
  end_date = "2023-11-20", 
  level = "lowa"
)
level_num <- case_when(params$level == "mlb" ~ 1, 
                       params$level == "aaa" ~ 11, 
                       params$level == "aa" ~ 12, 
                       params$level == "higha" ~ 13, 
                       params$level == "lowa" ~ 14,  
                       params$level == "rookie" ~ 16, 
                       TRUE ~ NA_real_)
source("2023-11-02-app/0-helper-funs.R")

# load data --------------------------------------------------------------------

game_pks_i <- pull_baseballr_gamepks(level_num, params$start_date, params$end_date)
game_pks_clean <- game_pks_i |> 
  select(game_pk, gameType) |> 
  distinct()
pbp_i <- bind_rows(map(game_pks_clean$game_pk, mlb_pbp, .progress = T))

# save -------------------------------------------------------------

saveRDS(pbp_i, file = paste0("2023-11-02-app/data/1-scraped_data_", params$level, ".rds"))
saveRDS(game_pks_clean, file = paste0("2023-11-02-app/data/1-scraped_data_game_", params$level, ".rds"))


