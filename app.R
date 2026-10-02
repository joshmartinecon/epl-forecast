library(shiny)
library(DT)
library(plotly)
library(base64enc)
library(stringi)

# Keep LOCAL <- TRUE while developing. Flip to FALSE once the GitHub Action

LOCAL <- TRUE
REPO  <- "https://raw.githubusercontent.com/joshmartinecon/epl-forecast/main/"

src <- function(f) if (LOCAL) f else paste0(REPO, f)

# ---- leagues -----------------------------------------------------------
# id must match the file prefix in data/ and the folder name in logos/
leagues <- data.frame(
  id    = c("epl", "la-liga", "bundesliga", "serie-a", "ligue-1", "mls", "nwsl"),
  label = c("Premier League", "La Liga", "Bundesliga", "Serie A", "Ligue 1", "MLS", "NWSL"),
  title = c("Title", "Title", "Title", "Title", "Title", "Shield", "Shield"),  # p_title column label
  stringsAsFactors = FALSE
)

# ---- logos (same matching rules as the pipeline) -----------------------
slugify <- function(s) {
  s <- stri_trans_general(s, "Latin-ASCII")
  s <- gsub(" \\(W\\)", "", s)
  s <- tolower(s)
  s <- gsub("&", "and", s)
  s <- gsub("[^a-z0-9]+", "-", s)
  gsub("^-|-$", "", s)
}

logo_uris <- function(teams, dir) {
  files <- list.files(dir, pattern = "\\.png$")
  slug  <- sub("-logo-footylogos\\.png$", "", files)
  vapply(teams, function(team) {
    s   <- slugify(team)
    hit <- which(slug == s)
    if (!length(hit)) hit <- grep(s, slug, fixed = TRUE)
    if (length(hit) > 1) hit <- hit[which.min(nchar(slug[hit]))]
    if (!length(hit)) hit <- which(vapply(slug, grepl, logical(1), x = s, fixed = TRUE))
    if (length(hit) == 1) dataURI(file = file.path(dir, files[hit]), mime = "image/png")
    else NA_character_
  }, character(1), USE.NAMES = FALSE)
}

# ---- data --------------------------------------------------------------
read_csv_safe <- function(f) {
  tryCatch(read.csv(src(f), stringsAsFactors = FALSE), error = function(e) NULL)
}

tbls <- lapply(leagues$id, function(id) {
  t <- read_csv_safe(paste0("data/", id, "_table.csv"))
  if (!is.null(t)) t$uri <- logo_uris(t$team, file.path("logos", id))
  t
})
names(tbls) <- leagues$id

# drop any league whose table isn't there yet, so the app still loads
leagues <- leagues[!vapply(tbls, is.null, logical(1)), ]
tbls    <- tbls[leagues$id]

nxt_all <- do.call(rbind, lapply(seq_len(nrow(leagues)), function(i) {
  d <- read_csv_safe(paste0("data/", leagues$id[i], "_next_match_predictions.csv"))
  if (is.null(d) || !nrow(d)) return(NULL)
  if (is.null(d$kickoff_utc)) d$kickoff_utc <- NA_character_
  d$league <- leagues$label[i]
  d[, c("date", "kickoff_utc", "league", "home", "away", "win", "draw", "lose")]
}))

mt <- suppressWarnings(file.mtime(file.path("data", paste0(leagues$id, "_table.csv"))))
last_updated <- if (all(is.na(mt))) "unknown" else format(max(mt, na.rm = TRUE), "%d %b %Y")

league_choices <- setNames(leagues$id, leagues$label)

# ---- plot --------------------------------------------------------------
crest_plot <- function(d) {
  rx <- range(d$exp_rtg);    ry <- range(d$massey_rtg)
  xr <- c(rx[1], rx[2]) + c(-1, 1) * 0.15 * diff(rx)
  yr <- c(ry[1], ry[2]) + c(-1, 1) * 0.15 * diff(ry)
  
  sx <- 0.075 * diff(xr)      # crest size, in data units
  sy <- 0.075 * diff(yr)
  
  imgs <- lapply(seq_len(nrow(d)), function(i) {
    if (is.na(d$uri[i])) return(NULL)
    list(source = d$uri[i], xref = "x", yref = "y",
         x = d$exp_rtg[i], y = d$massey_rtg[i],
         sizex = sx, sizey = sy, sizing = "contain",
         xanchor = "center", yanchor = "middle", layer = "above")
  })
  imgs <- Filter(Negate(is.null), imgs)
  
  lim <- range(c(xr, yr))     # 45-degree line spans both axes
  
  plot_ly(
    d, x = ~exp_rtg, y = ~massey_rtg,
    type = "scatter", mode = "markers",
    marker = list(size = 34, opacity = 0),     # invisible, but catches hover
    hoverinfo = "text",
    text = ~paste0(
      "<b>", team, "</b>",
      "<br>Underlying rating: ", sprintf("%+.2f", exp_rtg),
      "<br>Massey rating: ",     sprintf("%+.2f", massey_rtg),
      "<br>Luck: ",              sprintf("%+.2f", luck),
      "<br>",
      "<br>Points now: ",        pts_now,
      "<br>Projected: ",         exp_pts, " pts (", exp_rank, ")")
  ) |>
    layout(
      images = imgs,
      xaxis  = list(title = "Underlying rating (xG + momentum)",
                    range = xr, zeroline = TRUE, zerolinecolor = "#bbb", zerolinewidth = 2),
      yaxis  = list(title = "Massey rating (goal difference)",
                    range = yr, zeroline = TRUE, zerolinecolor = "#bbb", zerolinewidth = 2),
      shapes = list(list(type = "line", x0 = lim[1], x1 = lim[2],
                         y0 = lim[1], y1 = lim[2],
                         line = list(dash = "dot", color = "#999", width = 2))),
      hoverlabel = list(align = "left"),
      margin = list(t = 20)
    ) |>
    config(displayModeBar = FALSE)
}

info_panel <- function() {
  wellPanel(
    tags$div("Created by ",
             tags$a("Josh Martin", href = "https://joshmartinecon.github.io/")),
    tags$div(paste("Updated:", last_updated)),
    tags$div(tags$a("Source Code & Data",
                    href = "https://github.com/joshmartinecon/epl-forecast"))
  )
}

league_picker <- function(id) {
  wellPanel(selectInput(id, "League", choices = league_choices,
                        selected = league_choices[1]))
}

# ---- ui ----------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .lede { color: #555; margin-bottom: 18px; }
      table.dataTable { width: auto !important; }
      table.dataTable th, table.dataTable td { white-space: nowrap; }
    ")),
    # send the browser's time zone to the server once connected
    tags$script(HTML(
      "$(document).on('shiny:connected', function() {
         Shiny.setInputValue('tz', Intl.DateTimeFormat().resolvedOptions().timeZone);
       });"))
  ),
  
  titlePanel("Soccer Ratings & Forecasts"),
  
  tabsetPanel(
    type = "tabs",
    
    tabPanel(
      "Forecast",
      fluidPage(
        fluidRow(
          column(3,
                 league_picker("lg_forecast"),
                 info_panel(),
                 wellPanel(
                   tags$div(tags$b("Massey"), " — strength from goal difference"),
                   tags$br(),
                   tags$div(tags$b("Underlying"), " — strength from xG and momentum"),
                   tags$br(),
                   tags$div(tags$b("Luck"), " — the gap between them")
                 )
          ),
          column(9,
                 div(class = "lede",
                     p("Team strength solved from every match played so far, then the",
                       "remaining fixtures simulated 10,000 times. Probabilities are the",
                       "share of simulated seasons ending in each outcome.")),
                 DTOutput("forecast")
          )
        )
      )
    ),
    
    tabPanel(
      "Next matchday",
      fluidPage(
        fluidRow(
          column(3, info_panel()),
          column(9,
                 div(class = "lede",
                     p("Win/draw/loss probabilities from an ordered logit on the rating",
                       "gap. All three are stated from the home team's perspective.",
                       "Dates and kickoff times are in your local time zone; type a",
                       "league name in the search box to filter.")),
                 DTOutput("nextday")
          )
        )
      )
    ),
    
    tabPanel(
      "Interactive Graph",
      fluidPage(
        fluidRow(
          column(3,
                 league_picker("lg_graph"),
                 info_panel()),
          column(9,
                 div(class = "lede",
                     p("Realised goal difference against what the underlying numbers",
                       "support. Clubs above the dotted line have collected more than",
                       "their performances merit; clubs below have collected less.",
                       "Hover a crest for detail.")),
                 plotlyOutput("crest", height = "560px")
          )
        )
      )
    )
  )
)

# ---- server ------------------------------------------------------------

server <- function(input, output, session) {
  
  # keep the two league pickers in sync
  observeEvent(input$lg_forecast, ignoreInit = TRUE,
               updateSelectInput(session, "lg_graph", selected = input$lg_forecast))
  observeEvent(input$lg_graph, ignoreInit = TRUE,
               updateSelectInput(session, "lg_forecast", selected = input$lg_graph))
  
  output$forecast <- renderDT({
    req(input$lg_forecast)
    id <- input$lg_forecast
    t  <- tbls[[id]]
    
    # probability columns differ by league (p_top4 vs p_top18; no p_releg in MLS/NWSL)
    top <- grep("^p_top", names(t), value = TRUE)[1]
    top <- top[!is.na(top)]
    rel <- intersect("p_releg", names(t))
    pc  <- c("p_title", top, rel)
    pl  <- c(leagues$title[leagues$id == id],
             if (length(top)) paste("Top", sub("^p_top", "", top)),
             if (length(rel)) "Relegation")
    
    d <- t[order(t$exp_rank),
           c("team", "pts_now", "exp_pts", "exp_rank", pc,
             "massey_rtg", "exp_rtg", "luck")]
    
    datatable(
      d, rownames = FALSE, class = "compact stripe hover", width = "auto",
      colnames = c("Team", "Pts", "Proj. pts", "Proj. rank", pl,
                   "Massey", "Underlying", "Luck"),
      options = list(pageLength = nrow(d), dom = "t", autoWidth = TRUE)
    ) |>
      formatCurrency(pc, currency = "%", before = FALSE, digits = 0)
  })
  
  # all leagues together, in the viewer's time zone, sorted by kickoff
  nxt_local <- reactive({
    tz <- if (is.null(input$tz) || !nzchar(input$tz)) "America/New_York" else input$tz
    d  <- nxt_all
    k  <- as.POSIXct(d$kickoff_utc, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    ok <- !is.na(k)
    
    d$date <- as.character(d$date)                 # fallback: Eastern date from the CSV
    d$date[ok] <- format(k[ok], "%Y-%m-%d", tz = tz)
    d$time <- ""
    d$time[ok] <- sub("^0", "", format(k[ok], "%I:%M %p", tz = tz))
    
    d <- d[order(d$date, ifelse(ok, as.numeric(k), Inf), d$league), ]
    d[, c("date", "time", "league", "home", "away", "win", "draw", "lose")]
  })
  
  output$nextday <- renderDT({
    datatable(
      nxt_local(), rownames = FALSE, class = "compact stripe hover", width = "auto",
      colnames = c("Date", "Time", "League", "Home", "Away", "Home win", "Draw", "Away win"),
      options = list(pageLength = 25, dom = "ftp", autoWidth = TRUE, ordering = FALSE)
    ) |>
      formatStyle(c("win", "draw", "lose"),
                  background = styleColorBar(c(0, 100), "#e8eef7"),
                  backgroundSize = "98% 70%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") |>
      formatCurrency(c("win", "draw", "lose"),
                     currency = "%", before = FALSE, digits = 0)
  })
  
  output$crest <- renderPlotly({
    req(input$lg_graph)
    crest_plot(tbls[[input$lg_graph]])
  })
}

shinyApp(ui, server)