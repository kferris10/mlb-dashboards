
# notes ------------------------------------------------------------------------

# setup ------------------------------------------------------------------------

library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gt)
library(gtExtras)
library(shiny)
library(bslib)

# app utils
source(file.path(here(), "0-app-funs.R"))
load(file.path(here(), "data/5-pitcher_aggregated.RData"))
load(file.path(here(), "data/5-pitcher-recent-pitches.RData"))
pit_ranefs <- readRDS(file.path(here(), "data/4-stat_ranefs_pit.RDS"))
pit_bio <- readRDS(file.path(here(), "data/6-person_bio_pit.rds"))

# set up parms
default <- "Shohei Ohtani (29, ID: 660271)"

# combine for global datasets
df_stats_final <- df_pit_stats |> 
  left_join(pit_ranefs, by = c("pitcher_id", "season", "level")) |> 
  mutate(across(starts_with("z_"), ~ pmin(3, pmax(-3, .x))))

# ui ---------------------------------------------------------------------------

player_cards <- list(
  stats = card(
    full_screen = TRUE, card_header("Stats"), gt_output("tbl_results")
  ), 
  pitch_metrics = card(full_screen = TRUE, card_header("Pitch Metrics"), gt_output("tbl_metrics")), 
  plot_mvmt = card(full_screen = TRUE, card_header("Break Chart"), plotOutput("plot_mvmt")), 
  plot_vlhh = card(full_screen = TRUE, card_header("Locations vLHH"), plotOutput("plot_vlhh")), 
  plot_vrhh = card(full_screen = TRUE, card_header("Locations vRHH"), plotOutput("plot_vrhh"))
)

ui <- with(player_cards, app_page("Pitcher dashboard", stats, pitch_metrics, plot_mvmt, plot_vlhh, plot_vrhh))

# server -----------------------------------------------------------------------

server <- function(input, output, session) {
  updateSelectizeInput(session, 'player_label', choices = pit_bio$name_age_id, selected = default, server = TRUE)
  
  # data for selected player
  id <- reactive({ strip_id(input$player_label) })
  dat_pit_bio <- reactive({ subset(pit_bio, id == id()) })
  dat_pit_results <- reactive({ subset(df_stats_final, pitcher_id == id()) })
  dat_pit_metrics <- reactive({ subset(df_pit_metrics_display, pitcher_id == id()) })
  dat_pit_recent <- reactive({ subset(df_recent, pitcher_id == id()) })
  
  # generate outputs
  output$headshot = renderText({ output_headshot(dat_pit_bio()$espn_headshot) })
  output$org_logo = renderText({ output_logo(dat_pit_bio()$team_logo_espn) })
  output$bio <- render_gt( { output_bio(dat_pit_bio()) })
  output$tbl_results <- render_gt({
    dat_pit_results() |> 
      gt() |> 
      color_fun(z_raa600, raa700) |> 
      color_fun(z_raa600_split, raa700_split) |> 
      color_fun(z_kpct, kpct, r = F) |> 
      color_fun(z_bbpct, bbpct) |>
      color_fun(z_contact, contact) |>
      color_fun(z_ev, ev_avg) |>
      color_fun(z_gb, gbpct, r = F) |>
      color_fun(z_driven, driven) |>
      fmt_percent(c(kpct, bbpct, iz, contact, gbpct, driven), decimals = 1) |> 
      fmt_number(c(raa700, raa700_split, ev_avg), decimals = 1) |> 
      cols_hide(c(pitcher_id, starts_with("z_"))) |> 
      sub_missing(missing_text = "-") |> 
      tab_options(table.font.size = "80%") |> 
      cols_label(season = "Yr", 
                 level = "Lvl", 
                 org = "Org", 
                 bf = "TBF", 
                 raa700 = with_tooltip("P700", "Pitching RAA/700 BF"), 
                 raa700_split = with_tooltip("Spl", "Platoon split RAA/700 BF"), 
                 kpct = "K%", 
                 bbpct = with_tooltip("BB%", "IBB and HBP are included with BB"), 
                 iz = "Zone%", 
                 contact = with_tooltip("Ct%", "Contact%"), 
                 ev_avg = "EV",
                 gbpct = "GB%",
                 driven = with_tooltip("Dr%", "Driven%: Balls hit above 95mph and LA between 10 and 40 degrees"))
  })
  output$tbl_metrics <- render_gt({
    dat_pit_metrics() |> 
      gt() |> 
      fmt_percent(c(pct_vl, pct_vr), decimals = 1) |> 
      tab_options(table.font.size = "80%") |> 
      sub_missing(missing_text = "-") |> 
      cols_align("center") |> 
      cols_hide(pitcher_id) |> 
      cols_label(pitch_type = "Type", 
                 pct_vl = "vL%", 
                 pct_vr = "vR%", 
                 velo_label = "Velo", 
                 hb_label = "HB", 
                 ivb_label = "IVB")
  })
  output$plot_mvmt <- renderPlot({
    hand <- unique(dat_pit_recent()$pithand)
    pt <- unique(dat_pit_recent()$pitch_type)
    mlb_avgs <- subset(df_mlb_avg, pitch_type %in% pt & pithand %in% hand)
    
    dat_pit_recent() |> 
      ggplot(aes(x = pmax(pmin(25, hb), -25), 
                 y = pmax(pmin(ivb, 25), -25), 
                 colour = pitch_type)) + 
      geom_vline(aes(xintercept = 0), colour = "grey80") + 
      geom_hline(aes(yintercept = 0), colour = "grey80") + 
      geom_point(size = 2) + 
      geom_point(data = mlb_avgs, shape = 4, size = 4, stroke = 1.7, colour = "black") + 
      scale_x_continuous(limits = c(-25, 25), name = NULL) + 
      scale_y_continuous(limits = c(-25, 25), name = NULL) + 
      scale_color_brewer(name = NULL, type = "qual", palette = 7) +
      theme_bootswatch("lux") + 
      theme(panel.background = element_rect(fill=NA), 
            axis.text = element_text(size = 14), 
            axis.title = element_text(size = 16), 
            panel.grid.major = element_line(linewidth = 0.3, colour = "grey90"), 
            legend.position = c(.9, .8)) + 
      labs(caption = "Last 200 pitches, Pitcher's Perspective, X is MLB average")
  })
  output$plot_vlhh <- renderPlot({
    df_plot_vlhh <- subset(dat_pit_recent(), bathand == "L")
    p <- plot_locations(df_plot_vlhh)
    p
  })
  output$plot_vrhh <- renderPlot({
    df_plot_vrhh <- subset(dat_pit_recent(), bathand == "R")
    p <- plot_locations(df_plot_vrhh)
    p
  })
}

shinyApp(ui, server)
