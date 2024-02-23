
# notes ------------------------------------------------------------------------

### to do
# add run/def value
# add projection
# add xwoba
# make live web page
# set strikezone using top/bottom or strike%
# regions for pitch usage
# icelandic color scheme
# add splits
# add NCAA stats
# deploy
# park adjust pitch data
# RAA/GB/Miss to pitch data

# setup ------------------------------------------------------------------------

library(here)
library(dplyr)
library(tidyr)
library(gt)
library(gtExtras)
library(shiny)
library(bslib)

# app utils
source(file.path(here(), "0-app-funs.R"))
load(file.path(here(), "data/5-batter_aggregated.RData"))
bat_ranefs <- readRDS(file.path(here(), "data/4-stat_ranefs_bat.RDS"))
bat_bio <- readRDS(file.path(here(), "data/6-person_bio.rds"))

# set up parms
default <- "Shohei Ohtani (29, ID: 660271)"

# combine for global datasets
df_results_final <- df_bat_results |> 
  left_join(bat_ranefs, by = c("batter_id", "season", "level")) %>% 
  mutate(across(starts_with("z_"), ~ pmin(3, pmax(-3, .x))))
df_process_final <- df_bat_process |> 
  left_join(bat_ranefs, by = c("batter_id", "season", "level")) |> 
  mutate(across(starts_with("z_"), ~ pmin(3, pmax(-3, .x))))

# ui ---------------------------------------------------------------------------

player_cards <- list(
  results = card(
    full_screen = TRUE,
    card_header("Results"),
    gt_output("tbl_results")
  ), 
  process = card(
    full_screen = TRUE,
    card_header("Process"),
    gt_output("tbl_process")
  )
)
ui <- app_page("Batter dashboard", player_cards$results, player_cards$process)

# server -----------------------------------------------------------------------

server <- function(input, output, session) {
  updateSelectizeInput(session, 'player_label', choices = bat_bio$name_age_id, selected = default, server = TRUE)
  
  # data for selected player
  id <- reactive({ strip_id(input$player_label) })
  dat_bat_bio <- reactive({ filter(bat_bio, id == id()) })
  dat_bat_results <- reactive({ filter(df_results_final, batter_id == id()) })
  dat_bat_process <- reactive({ filter(df_process_final, batter_id == id()) })
  
  # generate outputs
  output$headshot = renderText({ output_headshot(dat_bat_bio()$espn_headshot) })
  output$org_logo = renderText({ output_logo(dat_bat_bio()$team_logo_espn) })
  output$bio <- render_gt( { output_bio(dat_bat_bio()) })
  output$tbl_results <- render_gt({
    dat_bat_results() |> 
      gt() |> 
      color_fun(z_raa600, raa600) |> 
      color_fun(z_kpct, kpct, r = F) |> 
      color_fun(z_bbpct, bbpct) |> 
      color_fun(z_iso, iso) |> 
      color_fun(z_babip, babip) |> 
      color_fun(z_avg, avg) |> 
      color_fun(z_obp, obp) |> 
      color_fun(z_slg, slg) |> 
      fmt_percent(c(kpct, bbpct), decimals = 1) |> 
      fmt_number(c(bat, raa600), decimals = 1) |> 
      fmt_number(c(iso, babip, avg, obp, slg), decimals = 3) |> 
      cols_hide(c(batter_id, starts_with("z_"))) |> 
      sub_missing(missing_text = "-") |> 
      tab_spanner(label = "Context", columns = season:bat) |>  
      tab_spanner(label = "PA Results", columns = raa600:slg) |> 
      tab_options(table.font.size = "80%") |> 
      cols_label(season = "Yr", 
                 level = "Lvl", 
                 org = "Org", 
                 pa = "PA", 
                 bat = with_tooltip("BAT", "Total Batting RAA"), 
                 raa600 = with_tooltip("B600", "Batting RAA/600 PA"), 
                 kpct = "K%", 
                 bbpct = with_tooltip("BB%", "IBB and HBP are included with BB"), 
                 iso = "ISO", 
                 babip = "BABIP", 
                 avg = "AVG", 
                 obp = "OBP", 
                 slg = "SLG")
  }, align = "right")
  output$tbl_process <- render_gt({
    dat_bat_process() |> 
      gt() |> 
      color_fun(z_iz_swing, iz_swing) |>
      color_fun(z_chase, chase, r = F) |> 
      color_fun(z_contact, contact) |> 
      color_fun(z_ev, ev_avg) |> 
      color_fun(z_hh95, hh95) |> 
      color_fun(z_gb, gbpct, r = F) |> 
      color_fun(z_driven, driven) |> 
      data_color(max_ev, method = "bin", bins = 9, palette = "RdBu", domain = c(98, 122), na_color = "white", reverse = T) |> 
      fmt_number(c(ev_avg, max_ev), decimals = 1) |> 
      fmt_percent(c(iz_swing, chase, contact, hh95, gbpct, driven), decimals = 1) |> 
      cols_hide(c(batter_id:level, starts_with("z_"))) |> 
      sub_missing(missing_text = "-") |> 
      tab_spanner(label = "Value", columns = c(season, level)) |> 
      tab_spanner(label = "Pitch Results", columns = iz_swing:contact) |> 
      tab_spanner(label = "On Contact", columns = ev_avg:driven) |> 
      tab_options(table.font.size = "80%") |> 
      cols_label(iz_swing = "zSw%", 
                 chase = "Chase%", 
                 contact = with_tooltip("Ct%", "Contact%"), 
                 ev_avg = "EV", 
                 hh95 = with_tooltip("HH95%", "Balls hit about 95mph"), 
                 max_ev = "Max EV", 
                 gbpct = "GB%", 
                 driven = with_tooltip("Dr%", "Driven%: Balls hit above 95mph and with a LA between 10 and 40 degrees"))
  }, align = "left")
}

shinyApp(ui, server)
