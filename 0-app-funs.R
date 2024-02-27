
app_sidebar <- function() {
  bslib::sidebar(
    shiny::selectizeInput("player_label", "Player Search", choices = NULL), 
    shiny::fluidRow(
      shiny::column(6, shiny::htmlOutput("headshot")),
      shiny::column(6, shiny::htmlOutput("org_logo"))
    ),
    shiny::hr(), 
    shiny::h5("Bio"), 
    gt::gt_output("bio"), 
    shiny::hr()
  )
}

app_footer <- function() {
  bslib::card_footer(
    shiny::hr(), 
    shiny::HTML(
      "<span>
      Built in <a href='https://www.r-project.org/'>R</a> with <a href='https://www.rstudio.com/products/shiny/'>shiny</a>.  
      Data from the <a href='https://www.mlb.com/'>MLB Stats API</a> via <a href='https://billpetti.github.io/baseballr/'>baseballr</a>.  
      Images from <a href='https://www.espn.com/'>ESPN</a> via <a href='https://github.com/camdenk/mlbplotR'>mlbplotR</a>.  
      Data wrangling powered by the <a href='https://www.tidyverse.org/'>tidyverse</a>, models by <a href='https://cran.r-project.org/web/packages/lme4/index.html'>lme4</a>.
      </span>"
    )
  )
}

app_page <- function(title, ...) {
  bslib::page_sidebar(
    title = title,
    theme = bslib::bs_theme(bootswatch = "lux"),
    tags$head(
      tags$style(HTML("#card_footer {font-size: 60%}"))
    ), 
    sidebar = app_sidebar(), 
    shiny::helpText("Colored so red = more offense, blue is better for defense/pitching.  Color adjusts for offensive environment, competition, and sample size.", style = "font-size:75%;"), 
    bslib::layout_columns(col_widths = c(6, 6, 4, 3, 3), ...), 
    app_footer()
  )
}


# function to color columns based on their z-scores
color_fun <- function(g, z, target, r = TRUE, ...) {
  result <- gt::data_color(
    g, 
    enexpr(z), 
    target_columns = enexpr(target), 
    method = "bin", 
    bins = 11, 
    palette = "RdBu", 
    reverse = r, 
    domain = c(-3, 3), 
    na_color = "white", 
    ...
  )
  result
}

strip_id <- function(x) {
  shiny::req(x)
  n <- nchar(x)
  as.numeric(substr(x, n - 7, n - 1))
}

output_headshot <- function(url) {
  c('<img src="', 
    url, 
    '" width="115" height="85"', 
    '>')
}
output_logo <- function(url) {
  c('<img src="',
    url,
    '" width="60" height="60"',
    '>')
}
output_bio <- function(df) {
  dat <- df |> 
    dplyr::select(Age = current_age, 
           `B/T` = bt, 
           Ht = height, 
           Wt = weight, 
           Draft = draft_year, 
           Pos = pos, 
           Jersey = primary_number) |> 
    dplyr::mutate(dplyr::across(everything(), as.character)) |> 
    dplyr::mutate(Jersey = paste0("#", Jersey)) |> 
    tidyr::pivot_longer(everything())
  
  gt::gt(dat) |> 
    gt::cols_align("center") |> 
    gt::sub_missing(missing_text = "-") |>
    gt::tab_options(column_labels.hidden = TRUE)
}

generate_kzone <- function() {
  kzone <- data.frame(x = c(0.75, 0.75, -0.75, -0.75, 0.75), 
                      y = c(1.75, 3.25, 3.25, 1.75, 1.75))
  kzone
}
plot_locations <- function(data) {
  data$px_clean <- pmax(-2, pmin(2, data$px_clean))
  data$pz_clean <- pmax(0.5, pmin(4.5, data$pz_clean))
  kzone <- generate_kzone()
  
  p <- ggplot2::ggplot(data, ggplot2::aes(
    x = px_clean, 
    y = pz_clean, 
    colour = pitch_type, 
    fill = pitch_type
  )) + 
    ggplot2::geom_polygon(aes(x, y), data = kzone, fill = NA, colour = "black") + 
    # ggplot2::geom_point(size = 0.7) +
    ggplot2::stat_ellipse(geom = "polygon", linetype = 2, alpha = 0.2, level = 0.7) + 
    ggplot2::scale_x_continuous(limits = c(-2, 2)) + 
    ggplot2::scale_y_continuous(limits = c(0.5, 4.5)) + 
    ggplot2::scale_color_brewer(name = NULL, type = "qual", palette = 7) + 
    ggplot2::scale_fill_brewer(name = NULL, type = "qual", palette = 7) + 
    bslib::theme_bootswatch("lux") + 
    ggplot2::theme(panel.background = element_rect(fill=NA), 
          axis.title = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          legend.position = c(.9, .8)) + 
    ggplot2::labs(caption = "Last 200 pitches, catcher's perspective.  Only pitches used 10% get shown.")
  p
}