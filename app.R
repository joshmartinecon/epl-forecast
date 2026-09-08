
library(shiny)
library(DT)
library(plotly)
library(base64enc)

# Keep LOCAL <- TRUE while developing. Flip to FALSE once the GitHub Action

LOCAL <- TRUE
REPO  <- "https://raw.githubusercontent.com/joshmartinecon/epl-forecast/main/"

src <- function(f) if (LOCAL) f else paste0(REPO, f)

tbl <- read.csv(src("data/epl_table.csv"),              stringsAsFactors = FALSE)
nxt <- read.csv(src("data/next_match_predictions.csv"), stringsAsFactors = FALSE)

last_updated <- tryCatch(format(file.mtime("data/epl_table.csv"), "%d %b %Y"),
                         error = function(e) "unknown")

slugify <- function(s) {
  s <- tolower(s)
  s <- gsub("&", "and", s)
  s <- gsub("[^a-z0-9]+", "-", s)
  gsub("^-|-$", "", s)
}

logo_files <- list.files("logos", pattern = "\\.png$")
logo_slug  <- sub("-logo-footylogos\\.png$", "", logo_files)

match_logo <- function(team) {
  s   <- slugify(team)
  hit <- which(logo_slug == s)
  if (!length(hit)) hit <- grep(s, logo_slug, fixed = TRUE)
  if (!length(hit)) hit <- which(vapply(logo_slug, grepl, logical(1), x = s, fixed = TRUE))
  if (length(hit) == 1) file.path("logos", logo_files[hit]) else NA_character_
}

tbl$uri <- sapply(tbl$team, function(t) {
  f <- match_logo(t)
  if (is.na(f)) NA_character_ else dataURI(file = f, mime = "image/png")
}, USE.NAMES = FALSE)

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

# ---- ui ----------------------------------------------------------------

ui <- fluidPage(
  tags$head(tags$style(HTML("
    .lede { color: #555; margin-bottom: 18px; }
    table.dataTable { width: auto !important; }
    table.dataTable th, table.dataTable td { white-space: nowrap; }
  "))),

  titlePanel("Premier League Ratings & Forecast"),

  tabsetPanel(
    type = "tabs",

    tabPanel(
      "Forecast",
      fluidPage(
        fluidRow(
          column(3,
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
                       "gap. All three are stated from the home team's perspective.")),
                 DTOutput("nextday")
          )
        )
      )
    ),

    tabPanel(
      "Interactive Graph",
      fluidPage(
        fluidRow(
          column(3, info_panel()),
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

  output$forecast <- renderDT({
    d <- tbl[order(tbl$exp_rank),
             c("team", "pts_now", "exp_pts", "exp_rank",
               "p_title", "p_top4", "p_releg",
               "massey_rtg", "exp_rtg", "luck")]

    datatable(
      d, rownames = FALSE, class = "compact stripe hover", width = "auto",
      colnames = c("Team", "Pts", "Proj. pts", "Proj. rank",
                   "Title", "Top 4", "Relegation",
                   "Massey", "Underlying", "Luck"),
      options = list(pageLength = 20, dom = "t", autoWidth = TRUE)
    ) |>
      formatCurrency(c("p_title", "p_top4", "p_releg"),
                     currency = "%", before = FALSE, digits = 0)
  })

  output$nextday <- renderDT({
    datatable(
      nxt, rownames = FALSE, class = "compact stripe hover", width = "auto",
      colnames = c("Date", "Home", "Away", "Home win", "Draw", "Away win"),
      options = list(pageLength = 10, dom = "t", autoWidth = TRUE)
    ) |>
      formatStyle(c("win", "draw", "lose"),
                  background = styleColorBar(c(0, 100), "#e8eef7"),
                  backgroundSize = "98% 70%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") |>
      formatCurrency(c("win", "draw", "lose"),
                     currency = "%", before = FALSE, digits = 0)
  })

  output$crest <- renderPlotly(crest_plot(tbl))
}

shinyApp(ui, server)
