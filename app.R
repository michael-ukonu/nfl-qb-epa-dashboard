# ============================================================
# NFL QB EPA Dashboard
# Interactive Shiny app for exploring quarterback pass EPA.
# Data: nflverse play-by-play (see data-raw/build_data.R)
# ============================================================
library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(DT)
library(RColorBrewer)

options(warn = -1)
options(nflreadr.verbose = FALSE)

# 1. DATA LOADING
PBP_RAW <- readRDS("pbp_data.rds") %>%
  select(
    season, pass, epa, passer_player_name, posteam, 
    down, game_half, cpoe, complete_pass, air_yards,
    yardline_100, ydstogo
  ) %>%
  filter(pass == 1, !is.na(epa), !is.na(passer_player_name))

# 2. SHARED STYLE CONSTANTS
FONT_BASE  <- list(family = "Inter, Arial, sans-serif", size = 12, color = "#1e293b")
FONT_TITLE <- list(family = "Inter, Arial, sans-serif", size = 13, color = "#0f172a")

axis_style <- function(title_text, standoff_val = 30) {
  list(
    title      = list(text = title_text, font = FONT_TITLE, standoff = standoff_val),
    tickfont   = list(size = 11),
    gridcolor  = "rgba(0,0,0,0.05)",
    automargin = TRUE
  )
}

get_initials <- function(name) {
  parts <- unlist(strsplit(gsub("^[A-Z]\\.", "", name), " ")) 
  first_init <- substr(name, 1, 1)
  last_init <- substr(tail(parts, 1), 1, 1)
  paste0(first_init, last_init)
}

# ============================================================
# UI
# ============================================================
ui <- page_navbar(
  title  = "NFL QB EPA Analytics",
  fillable = FALSE,
  theme  = bs_theme(bootswatch = "flatly", primary = "#1d4ed8"),
  navbar_options = navbar_options(bg = "#0a1628", inverse = TRUE),

  sidebar = sidebar(
    width = 230,
    selectInput("season_filter", "Season", choices = c("2023", "2022", "Both (averaged)"), selected = "2023"),
    sliderInput("min_att", "Min. Total Season Attempts", min = 50, max = 600, value = 200, step = 50),
    selectInput("down_filter", "Down", choices = c("All", "1", "2", "3", "4"), selected = "All"),
    selectInput("half_filter", "Half", choices = c("Full game", "1st half", "2nd half"), selected = "Full game")
  ),

  nav_panel("Overview", icon = icon("chart-bar"),
    layout_columns(
      fill = FALSE,
      value_box(
        title    = tags$span("QBs Tracked", style = "font-size:11px; font-weight:600;"),
        value    = tags$span(textOutput("m_qbs"), style = "font-size:22px; font-weight:700;"),
        showcase = icon("users"), theme = "primary", height = "110px"
      ),
      value_box(
        title    = tags$span("Avg EPA/Play", style = "font-size:11px; font-weight:600;"),
        value    = tags$span(textOutput("m_epa"), style = "font-size:22px; font-weight:700;"),
        showcase = icon("chart-line"), theme = "success", height = "110px"
      ),
      value_box(
        title    = tags$span("Top QB", style = "font-size:11px; font-weight:600;"),
        value    = tags$span(textOutput("m_top"), style = "font-size:16px; font-weight:700; line-height:1.2;"),
        showcase = icon("star"), theme = "warning", height = "110px"
      ),
      value_box(
        title    = tags$span("Avg Comp %", style = "font-size:11px; font-weight:600;"),
        value    = tags$span(textOutput("m_cp"), style = "font-size:22px; font-weight:700;"),
        showcase = icon("football-ball"), theme = "info", height = "110px"
      )
    ),
    layout_columns(
      fill = FALSE,
      col_widths = c(8, 4),
      card(card_header("Pass EPA / Play — Top 16"), plotlyOutput("epa_bar", height = "700px")),
      card(card_header("EPA Distribution"), plotlyOutput("epa_hist", height = "700px"))
    ),
    card(card_header("Efficiency vs Accuracy"), plotlyOutput("epa_cpoe_scatter", height = "440px"))
  ),

  nav_panel("QB Comparison", icon = icon("users"),
    card(card_header("Select QBs (Up to 6)"), selectizeInput("qb_select", NULL, choices = NULL, multiple = TRUE, options = list(maxItems = 6))),
    layout_columns(
      col_widths = c(5, 7),
      card(card_header("Radar Metrics"), plotlyOutput("radar_chart", height = "430px")),
      card(card_header("Side-by-Side Comparison"), plotlyOutput("compare_bars", height = "430px"))
    )
  ),

  nav_panel("Play Explorer", icon = icon("magnifying-glass"),
    card(selectInput("qb_play_select", "Select QB", choices = NULL, width = "320px")),
    card(card_header("Field Position vs EPA"), plotlyOutput("field_scatter", height = "390px")),
    card(card_header("Pass Distance Trend"), plotlyOutput("air_epa_scatter", height = "370px"))
  ),

  nav_panel("Data Table", icon = icon("table"),
    card(card_header("Full QB Statistics"), DTOutput("qb_table"))
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {

  qb_stats <- reactive({
    season_totals <- PBP_RAW %>%
      filter(if (input$season_filter != "Both (averaged)") season == as.integer(input$season_filter) else TRUE) %>%
      group_by(passer_player_name) %>%
      summarise(total_att = n(), .groups = "drop")
    
    dat <- filtered_pbp() %>%
      group_by(passer_player_name, posteam) %>%
      summarise(
        attempts = n(),
        comp_pct = round(mean(complete_pass, na.rm = TRUE) * 100, 1),
        epa_per_play = round(mean(epa, na.rm = TRUE), 3),
        cpoe = round(mean(cpoe, na.rm = TRUE), 2),
        air_yards = round(mean(air_yards, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      inner_join(season_totals, by = "passer_player_name") %>%
      filter(total_att >= input$min_att) %>%
      rename(qb = passer_player_name, team = posteam) %>%
      mutate(initials = sapply(qb, get_initials)) %>%
      arrange(desc(epa_per_play))
  })

  filtered_pbp <- reactive({
    dat <- PBP_RAW
    if (input$season_filter != "Both (averaged)") dat <- dat %>% filter(season == as.integer(input$season_filter))
    if (input$down_filter != "All") dat <- dat %>% filter(down == as.integer(input$down_filter))
    if (input$half_filter == "1st half") dat <- dat %>% filter(game_half == "Half1")
    if (input$half_filter == "2nd half") dat <- dat %>% filter(game_half == "Half2")
    dat
  })

  observe({
    qbs <- qb_stats()$qb
    updateSelectizeInput(session, "qb_select", choices = qbs, selected = head(qbs, 4), server = TRUE)
    updateSelectInput(session, "qb_play_select", choices = qbs)
  })

  output$m_qbs <- renderText({ nrow(qb_stats()) })
  output$m_epa <- renderText({ round(mean(qb_stats()$epa_per_play, na.rm = TRUE), 3) })
  output$m_top <- renderText({ 
    top <- qb_stats()[1,]
    if(nrow(top) > 0) paste0(top$qb, " (", top$team, ")") else "N/A"
  })
  output$m_cp <- renderText({ paste0(round(mean(qb_stats()$comp_pct, na.rm = TRUE), 1), "%") })

  # ---- TAB 1: PASS EPA/PLAY (FIXED THICKNESS LOGIC) ----
  output$epa_bar <- renderPlotly({
    df <- qb_stats() %>% slice_max(epa_per_play, n = 16)
    n_qbs <- nrow(df)

    # Fix height so each bar always gets ~38px regardless of QB count
    plot_height <- max(200, n_qbs * 38 + 80)

    # Sort ascending so best QB is at top
    df <- df %>% arrange(epa_per_play) %>%
      mutate(
        # Unique numeric ID per row — prevents duplicate initials (e.g. JB for
        # Jake Browning AND Joe Burrow) from collapsing into the same bar slot
        bar_id = seq_len(n()),
        label  = initials
      )

    colors_palette <- RColorBrewer::brewer.pal(max(3, min(12, n_qbs)), "Paired")
    color_vec <- rep_len(colors_palette, n_qbs)
    df$color <- color_vec

    p <- plot_ly()
    for (i in seq_len(nrow(df))) {
      row <- df[i, ]
      p <- add_trace(p,
        type        = "bar",
        orientation = "h",
        x           = row$epa_per_play,
        y           = row$bar_id,   # unique numeric key — no collisions
        width       = 0.9,
        name        = row$qb,
        showlegend  = FALSE,
        marker      = list(color = row$color),
        hovertext   = paste0("<b>", row$qb, "</b><br>EPA: ", row$epa_per_play),
        hoverinfo   = "text"
      )
    }

    p %>% layout(
      font   = FONT_BASE,
      height = plot_height,
      xaxis  = axis_style("EPA / Play"),
      yaxis  = list(
        title      = "",
        automargin = TRUE,
        tickmode   = "array",
        tickvals   = df$bar_id,   # map numeric IDs back to initials as labels
        ticktext   = df$label
      ),
      bargap = 0,
      margin = list(l = 80, b = 70)
    )
  })

  output$epa_hist <- renderPlotly({
    df <- qb_stats()
    plot_ly(df, x = ~epa_per_play, type = "histogram", 
            marker = list(color = "#1d4ed8", line = list(color="white", width=1))) %>%
      layout(xaxis = axis_style("EPA / Play"), yaxis = axis_style("Count"), margin = list(b=70))
  })

  output$epa_cpoe_scatter <- renderPlotly({
    df <- qb_stats()
    plot_ly(df, x = ~cpoe, y = ~epa_per_play, type = "scatter", mode = "markers",
            marker = list(size = 15, opacity = 0.8, color = ~epa_per_play, colorscale = "RdBu", reversescale = TRUE, line = list(width=1, color="black")),
            hovertext = ~paste0("<b>", qb, "</b><br>EPA: ", epa_per_play, "<br>CPOE: ", cpoe, "%"),
            hoverinfo = "text") %>%
      layout(xaxis = axis_style("CPOE (%)", 35), yaxis = axis_style("EPA / Play", 35), margin = list(b=80, l=80))
  })

  # ---- TAB 2: SIDE-BY-SIDE ----
  output$compare_bars <- renderPlotly({
    req(input$qb_select)
    df <- qb_stats() %>% filter(qb %in% input$qb_select)
    colors <- c("#1d4ed8","#dc2626","#16a34a","#d97706","#7c3aed","#0891b2")

    make_p <- function(metric, title) {
      plot_ly(df, x = ~qb, y = as.formula(paste0("~", metric)), type = "bar", 
              color = ~qb, colors = colors, showlegend = FALSE, 
              width = 0.6) %>%
        layout(xaxis = list(title = ""), yaxis = axis_style("", 10), bargap = 0.2)
    }

    subplot(make_p("epa_per_play", "EPA"), make_p("cpoe", "CPOE%"), make_p("comp_pct", "Comp%"), 
            nrows = 1, margin = 0.08, titleY = FALSE) %>%
      layout(font = FONT_BASE, margin = list(b = 100, t = 50),
             annotations = list(
               list(x = 0.13, y = 1.15, text = "EPA", showarrow = F, xref='paper', yref='paper', font=FONT_TITLE),
               list(x = 0.50, y = 1.15, text = "CPOE %", showarrow = F, xref='paper', yref='paper', font=FONT_TITLE),
               list(x = 0.87, y = 1.15, text = "Comp %", showarrow = F, xref='paper', yref='paper', font=FONT_TITLE)
             ))
  })

  output$radar_chart <- renderPlotly({
    req(input$qb_select)
    df <- qb_stats() %>% filter(qb %in% input$qb_select)
    all_qbs <- qb_stats()
    metrics <- c("epa_per_play", "cpoe", "comp_pct", "air_yards")
    labels  <- c("EPA", "CPOE", "Comp%", "Air Yds")
    p <- plot_ly()
    for (i in seq_len(nrow(df))) {
      vals <- sapply(metrics, function(m) {
        rng <- range(all_qbs[[m]], na.rm = TRUE)
        (df[[m]][i] - rng[1]) / (rng[2] - rng[1])
      })
      p <- add_trace(p, type = "scatterpolar", r = c(vals, vals[1]), theta = c(labels, labels[1]),
                     name = df$qb[i], fill = "toself", mode = "lines")
    }
    p %>% layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, 1))), margin = list(b=80))
  })

  # ---- TAB 3 & 4 ----
  output$field_scatter <- renderPlotly({
    req(input$qb_play_select)
    df <- filtered_pbp() %>% filter(passer_player_name == input$qb_play_select)
    plot_ly(df, x = ~yardline_100, y = ~epa, type = "scatter", mode = "markers", 
            marker = list(color = "#1d4ed8", opacity = 0.4)) %>%
      layout(xaxis = axis_style("Yards from Goal"), yaxis = axis_style("EPA"), margin = list(b=80))
  })

  output$air_epa_scatter <- renderPlotly({
    req(input$qb_play_select)
    df <- filtered_pbp() %>% filter(passer_player_name == input$qb_play_select, !is.na(air_yards)) %>% arrange(air_yards)
    if(nrow(df) > 5) {
      l <- loess(epa ~ air_yards, data = df, span = 0.6)
      plot_ly(df, x = ~air_yards) %>% add_markers(y = ~epa, opacity = 0.3, name="Play") %>%
        add_lines(y = ~fitted(l), line = list(width = 3, color="red"), name="Trend") %>%
        layout(xaxis = axis_style("Air Yards"), yaxis = axis_style("EPA"), margin = list(b=80))
    } else {
      plot_ly(df, x = ~air_yards, y = ~epa, type="scatter", mode="markers") %>%
        layout(xaxis = axis_style("Air Yards"), yaxis = axis_style("EPA"), margin = list(b=80))
    }
  })

  output$qb_table <- renderDT({
    datatable(qb_stats() %>% select(-initials, -total_att), options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE) %>%
      formatStyle("epa_per_play", color = styleInterval(0, c("#dc2626", "#16a34a")), fontWeight = "bold")
  })
}

shinyApp(ui, server)
