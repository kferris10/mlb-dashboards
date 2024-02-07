
pull_baseballr_gamepks <- function(levels, start_date, end_date) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  dates <- start_date:end_date
  
  game_pks_all <- tibble::tibble(NULL)
  pb <- progress::progress_bar$new(total = length(dates))
  for(i in dates) {
    pb$tick()
    date_i <- as.Date(i, origin = as.Date("1970-01-01"))
    game_pks_i <- suppressMessages(baseballr::mlb_game_pks(date_i, level_ids = levels))
    if("data.frame" %in% class(game_pks_i)) {
      game_pks_all <- dplyr::bind_rows(game_pks_all, game_pks_i)
    }
  }
  
  result <- dplyr::filter(game_pks_all, 
                          status.detailedState %in% c("Final", "Completed Early"))
  result
}

clean_data <- function(data, game_info = NULL) {
  # load misc metadata
  teams <- mlbplotR::load_mlb_teams()
  sport_codes <- baseballr::mlb_sports()
  
  # only certain levels have IVB columns
  if(!("pitchData.breaks.breakVerticalInduced" %in% names(data))) {
    data[["pitchData.breaks.breakVerticalInduced"]] <- NA
  }
  if(!("pitchData.breaks.breakHorizontal" %in% names(data))) {
    data[["pitchData.breaks.breakHorizontal"]] <- NA
  }
  
  df_clean <- data |> 
    dplyr::filter(isPitch) |>
    dplyr::mutate(
      game_date = as.Date(game_date), 
      has_tm = !is.na(pitchData.startSpeed), 
      pitch_type = factor(
        dplyr::case_when(details.type.code %in% c("FF", "FA") ~ "4FB",
                         details.type.code %in% c("FT", "SI") ~ "2FB",
                         details.type.code %in% c("FC") ~ "CT",
                         details.type.code %in% c("SL", "SV", "ST") ~ "SL",
                         details.type.code %in% c("CS", "CU", "KC") ~ "CB",
                         details.type.code %in% c("CH", "FO", "FS", "SC") ~ "CH",
                         TRUE ~ details.type.code), 
        levels = c("4FB", "2FB", "CT", "SL", "CB", "CH")
      ),
      pitchresult = dplyr::case_when(details.description %in% c("Ball", "Ball In Dirt", "Hit By Pitch", "Intent Ball") ~ "B", 
                                     substr(details.description, 1, 4) == "Foul" ~ "F", 
                                     substr(details.description, 1, 7) == "In play" ~ "H", 
                                     details.description == "Pitchout" ~ "PO", 
                                     substr(details.description, 1, 8) == "Swinging" | details.description == "Missed Bunt" ~ "S", 
                                     details.description == "Called Strike" ~ "T", 
                                     TRUE ~ NA_character_), 
      game_date = as.Date(game_date), 
      season = as.numeric(substr(game_date, 1, 4)), 
      pitching_team = ifelse(batting_team == home_team, away_team, home_team), 
      batting_org = dplyr::coalesce(ifelse(batting_team == home_team, home_parentOrg_name, away_parentOrg_name), batting_team), 
      pitching_org = dplyr::coalesce(ifelse(pitching_team == home_team,  home_parentOrg_name, away_parentOrg_name), pitching_team), 
      px_chart = 3.1 - 0.0265 * pitchData.coordinates.x, 
      pz_chart = 8.5 - 0.035 * pitchData.coordinates.y, 
      hit_x_chart = 2.7 * (hitData.coordinates.coordX - 125.5), 
      hit_y_chart = -1 * 2.7 * (hitData.coordinates.coordY - 200), 
      spray_chart = 180 / pi * atan(hit_y_chart / hit_x_chart), 
      pa_outcome = dplyr::case_when(
        result.eventType %in% c("strikeout", "strikeout_double_play") ~ "k", 
        result.eventType %in% c("walk", "hit_by_pitch", "intent_walk", "catcher_interf") ~ "bb", 
        result.eventType %in% c("single") ~ "b1", 
        result.eventType %in% c("double") ~ "b2", 
        result.eventType %in% c("triple") ~ "b3", 
        result.eventType %in% c("home_run") ~ "hr", 
        result.eventType %in% c(
          "field_out", "force_out", "grounded_into_double_play", 
          "sac_fly", "field_error", "sac_bunt", "double_play", 
          "fielders_choice", "fielders_choice_out", 
          "sac_fly_double_play", "other_out", "triple_play"
        ) ~ "bip_out", 
        TRUE ~ result.eventType
      ), 
      hit_type = dplyr::case_when(
        substr(hitData.trajectory, 1, 4) == "bunt" ~ "bunt", 
        TRUE ~ hitData.trajectory
      ), 
      ev = coalesce(hitData.launchSpeed, case_when(!has_tm ~ NA_real_, 
                                                   pa_outcome == "hr" ~ 104, 
                                                   hit_type %in% c("line_drive", "fly_ball") ~ 93, 
                                                   hit_type == "ground_ball" ~ 80, 
                                                   hit_type == "popup" ~ 72, 
                                                   TRUE ~ NA_real_)), 
      la = coalesce(hitData.launchAngle, case_when(!has_tm ~ NA_real_, 
                                                   hit_type == "ground_ball" ~ -25, 
                                                   hit_type == "line_drive" ~ 12, 
                                                   hit_type == "fly_ball" ~ 30, 
                                                   hit_type == "popup" ~ 60, 
                                                   TRUE ~ NA_real_))
      
    ) |>
    dplyr::left_join(dplyr::select(teams, team_name, team_abbr), 
                     by = c("batting_org" = "team_name")) |> 
    dplyr::left_join(dplyr::select(teams, team_name, team_abbr), 
                     by = c("pitching_org" = "team_name"), 
                     suffix = c("_bat", "_pit")) |> 
    dplyr::left_join(dplyr::select(sport_codes, sport_id, sport_abbreviation), 
                     by = c("home_level_id" = "sport_id")) |> 
    dplyr::select(game_pk,
                  game_date,
                  season, 
                  level = sport_abbreviation, 
                  play_id = playId,
                  ab_index = about.atBatIndex, 
                  index, 
                  home_league_id, 
                  home_team:away_team, away_parentOrg_id, away_parentOrg_name,
                  batting_team,
                  pitching_team, 
                  bat_org = team_abbr_bat, 
                  pit_org = team_abbr_pit, 
                  about.inning,
                  pitcher_id = matchup.pitcher.id,
                  batter_id = matchup.batter.id,
                  pithand = matchup.pitchHand.code,
                  bathand = matchup.batSide.code,
                  inning = about.inning, 
                  pa_outcome,
                  balls = count.balls.start,
                  strikes = count.strikes.start,
                  outs = count.outs.start,
                  pitch_outcome = details.description,
                  pitchresult, 
                  pitch_type,
                  pitch_type_raw = details.type.code, 
                  has_tm, 
                  velo = pitchData.startSpeed,
                  spin = pitchData.breaks.spinRate,
                  ext = pitchData.extension,
                  rx = pitchData.coordinates.x0,
                  rz = pitchData.coordinates.z0,
                  bx = pitchData.coordinates.pfxX,
                  bz = pitchData.coordinates.pfxZ,
                  ivb = pitchData.breaks.breakVerticalInduced, 
                  hb = pitchData.breaks.breakHorizontal, 
                  px = pitchData.coordinates.pX,
                  pz = pitchData.coordinates.pZ,
                  px_chart, 
                  pz_chart, 
                  hit_type,
                  hit_hard = hitData.hardness,
                  ev,
                  la,
                  dist = hitData.totalDistance,
                  hit_x_chart,
                  hit_y_chart,
                  spray_chart) |>
    dplyr::rename_with(snakecase::to_any_case) |> 
    dplyr::distinct()
  
  # return
  result <- df_clean |> 
    dplyr::select(game_pk:pz, px_chart, pz_chart, hit_type:dist, hit_x_chart:spray_chart) |>
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))
  # optionally add game info 
  if(!is.null(game_info)) {
    game_clean <- game_info |> 
      dplyr::filter(gameType == "R") |> 
      dplyr::select(game_pk, game_type = gameType) |> 
      dplyr::distinct()
    result <- dplyr::inner_join(game_clean, result, by = "game_pk")
  }
  
  result
}


fit_context_models <- function(pa_dat, 
                               bip_dat, 
                               pitch_dat, 
                               f_base = formula(~ factor(home_league_id) + bathand * pithand + (1|batter_id) + (1|pitcher_id) + (1|home_team))) {
  # setup
  season <- unique(pa_dat$season)
  level <- unique(pa_dat$level)
  stopifnot(length(season) == 1)
  stopifnot(length(level) == 1)
  print(paste("Fitting", season, level))
  
  # wrapper function to extract random effects from lme4 models
  extract_ranef <- function(m, slot, nm) {
    sigma <- attr(lme4::VarCorr(m)[[slot]], "stddev")
    d <- lme4::ranef(m)[[slot]] |>  
      tibble::rownames_to_column(slot) |> 
      tibble::tibble() |> 
      dplyr::mutate(`(Intercept)` = `(Intercept)` / sigma)
    d[[1]] <- as.numeric(d[[1]])
    names(d)[2] <- nm
    d
  }
  
  # dataframes for component models
  ab_dat <- subset(pa_dat, is_ab)
  babip_dat <- subset(pa_dat, pa_outcome %in% c("b1", "b2", "b3", "bip_out"))
  has_tm <- nrow(subset(bip_dat, !is.na(ev))) > 10
  
  # model specifications
  f_raa600 <- update(f_base, raa600 ~ .)
  f_k <- update(f_base, I(pa_outcome == "k") ~ .)
  f_bb <- update(f_base, I(pa_outcome == "bb") ~ .)
  f_iso <- update(f_base, pmax(bases - 1, 0) ~ .)
  f_babip <- update(f_base, I(pa_outcome %in% c("b1", "b2", "b3")) ~ .)
  f_avg <- update(f_base, is_hit ~ .)
  f_obp <- update(f_base, is_obp ~ .)
  f_slg <- update(f_base, bases ~ .)
  f_gb <- update(f_base, I(hit_type == "ground_ball") ~ .)
  f_ev <- update(f_base, ev ~ .)
  f_hh95 <- update(f_base, I(ev >= 95) ~ .)
  f_driven <-update(f_base, I(ev >= 95 & la >= 10 & la <= 40) ~ .)
  f_iz_swing <- update(f_base, cbind(n_iz_swing, n_iz - n_iz_swing) ~ .)
  f_chase <- update(f_base, cbind(n_ooz_swing, n_ooz - n_ooz_swing) ~ .)
  f_contact <- update(f_base, cbind(n_contact, n_swing - n_contact) ~ .)
  # dropping league for low A which only has one league with TM 
  # prevents silly lmer errors
  if(length(unique(bip_dat$home_league_id[!is.na(bip_dat$ev)])) == 1) {
    f_ev <- update(f_ev, . ~ . - factor(home_league_id))
    f_hh95 <- update(f_hh95, . ~ . - factor(home_league_id))
    f_driven <- update(f_driven, . ~ . - factor(home_league_id))
  }
  
  # fitting models
  m_raa600 <- lme4::lmer(f_raa600, data = pa_dat)
  m_k <- lme4::glmer(f_k, pa_dat, family = binomial())
  m_bb <- lme4::glmer(f_bb, pa_dat, family = binomial())
  m_iso <- lme4::lmer(f_iso, ab_dat)
  m_babip <- lme4::glmer(f_babip, babip_dat, family = binomial())
  m_avg <- lme4::glmer(f_avg, ab_dat, family = binomial())
  m_obp <- lme4::glmer(f_obp, pa_dat, family = binomial())
  m_slg <- lme4::lmer(f_slg, ab_dat)
  m_iz_swing <- lme4::glmer(f_iz_swing, pitch_dat, family = binomial(), subset = n_iz > 0)
  m_chase <- lme4::glmer(f_chase, pitch_dat, family = binomial(), subset = n_ooz > 0) 
  m_contact <- lme4::glmer(f_contact, pitch_dat, family = binomial(), subset = n_swing > 0) 
  m_gb <- lme4::glmer(f_gb, bip_dat, family = binomial())
  if(has_tm) {
    m_ev <- lme4::lmer(f_ev, data = bip_dat)
    m_hh95 <- lme4::glmer(f_hh95, bip_dat, family = binomial())
    m_driven <- lme4::glmer(f_driven, bip_dat, family = binomial())
  }
  
  # extracting player effects from models
  ranefs_bat <- extract_ranef(m_raa600, "batter_id", "z_raa600") %>% 
    full_join(extract_ranef(m_k, "batter_id", "z_kpct"), by = "batter_id") %>% 
    full_join(extract_ranef(m_bb, "batter_id", "z_bbpct"), by = "batter_id") %>% 
    full_join(extract_ranef(m_avg, "batter_id", "z_avg"), by = "batter_id") %>% 
    full_join(extract_ranef(m_obp, "batter_id", "z_obp"), by = "batter_id") %>% 
    full_join(extract_ranef(m_slg, "batter_id", "z_slg"), by = "batter_id") %>% 
    full_join(extract_ranef(m_iso, "batter_id", "z_iso"), by = "batter_id") %>% 
    full_join(extract_ranef(m_babip, "batter_id", "z_babip"), by = "batter_id") %>% 
    full_join(extract_ranef(m_iz_swing, "batter_id", "z_iz_swing"), by = "batter_id") %>% 
    full_join(extract_ranef(m_chase, "batter_id", "z_chase"), by = "batter_id") %>% 
    full_join(extract_ranef(m_contact, "batter_id", "z_contact"), by = "batter_id") %>% 
    full_join(extract_ranef(m_gb, "batter_id", "z_gb"), by = "batter_id")
  ranefs_pit <- extract_ranef(m_raa600, "pitcher_id", "z_raa600") %>% 
    full_join(extract_ranef(m_k, "pitcher_id", "z_kpct"), by = "pitcher_id") %>% 
    full_join(extract_ranef(m_bb, "pitcher_id", "z_bbpct"), by = "pitcher_id") %>% 
    full_join(extract_ranef(m_iz_swing, "pitcher_id", "z_iz_swing"), by = "pitcher_id") %>% 
    full_join(extract_ranef(m_chase, "pitcher_id", "z_chase"), by = "pitcher_id") %>% 
    full_join(extract_ranef(m_contact, "pitcher_id", "z_contact"), by = "pitcher_id") %>% 
    full_join(extract_ranef(m_gb, "pitcher_id", "z_gb"), by = "pitcher_id")
  
  if(has_tm) {
    ranefs_bat <- ranefs_bat %>% 
      full_join(extract_ranef(m_ev, "batter_id", "z_ev"), by = "batter_id") %>% 
      full_join(extract_ranef(m_hh95, "batter_id", "z_hh95"), by = "batter_id") %>% 
      full_join(extract_ranef(m_driven, "batter_id", "z_driven"), by = "batter_id")
    
    ranefs_pit <- ranefs_pit %>% 
      full_join(extract_ranef(m_ev, "pitcher_id", "z_ev"), by = "pitcher_id") %>% 
      full_join(extract_ranef(m_hh95, "pitcher_id", "z_hh95"), by = "pitcher_id") %>% 
      full_join(extract_ranef(m_driven, "pitcher_id", "z_driven"), by = "pitcher_id")
  }
  
  return(list(bat_ranefs = ranefs_bat, pit_ranefs = ranefs_pit))
  
}


# split a vector into smaller chunks
# mlb_people errors for too many ids
split_ids <- function(x, chunk = 500) {
  # set up points to split x up
  start <- min(chunk, length(x))
  breaks <- seq(start, length(x), by = chunk)
  if(max(breaks) < length(x)) {
    breaks <- c(breaks, length(x))
  }
  # split up x into chunks
  l <- list(1:breaks[1])
  if(length(breaks) > 1) {
    for(i in 2:length(breaks)) {
      new_seq <- (breaks[i-1]+1):breaks[i]
      l[[i]] <- new_seq
    }
  }
  l
}

# scrape biographical info using mlb_people
scrape_people <- function(x, ...) {
  ids_split <- split_ids(x, ...)
  r <- list()
  for(i in seq_along(ids_split)) {
    result_i <- baseballr::mlb_people(ids_split[[i]])
    r[[i]] <- result_i
  }
  purrr::list_rbind(r)
}
