

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
library(snakecase)
library(mlbplotR)
library(baseballr)

# load data
params <- list(
  level = "rookie"
)
df_scraped <- readRDS(paste0("2023-11-02-app/data/1-scraped_data_", params$level, ".RDS"))
game_info_scraped <- readRDS(paste0("2023-11-02-app/data/1-scraped_data_game_", params$level, ".rds"))
source("2023-11-02-app/0-helper-funs.R")

# clean data --------------------------------------------------------------------

df_clean <- clean_data(df_scraped, game_info_scraped)

# save -------------------------------------------------------------

saveRDS(df_clean, file = paste0("2023-11-02-app/data/2-cleaned_data_", params$level, ".RDS"))


