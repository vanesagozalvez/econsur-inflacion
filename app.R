library(shiny)
library(bs4Dash)
library(dplyr)
library(tidyr)
library(stringr)
library(shinycssloaders)
library(waiter)
library(highcharter)
library(viridis)

cat("[STARTUP] All packages loaded OK\n")
cat("[STARTUP] Working directory:", getwd(), "\n")
cat("[STARTUP] Data file exists:", file.exists("data/serie_ipc_divisiones.csv"), "\n")

# ── Helpers ────────────────────────────────────────────────────────────────────

traducir_mes <- function(mes_ingles) {
  meses        <- c("january","february","march","april","may","june",
                    "july","august","september","october","november","december")
  meses_esp    <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                    "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
  idx <- match(tolower(mes_ingles), meses)
  if (!is.na(idx)) meses_esp[idx] else mes_ingles
}

# ── ETL: read & reshape serie_ipc_divisiones.csv ───────────────────────────────

load_ipc <- function(path = "data/serie_ipc_divisiones.csv") {

  raw <- read.csv2(path, encoding = "latin1", stringsAsFactors = FALSE)

  # Keep only Nacional + COICOP divisions (Nivel general y divisiones COICOP)
  df <- raw %>%
    filter(
      Region      == "Nacional",
      Clasificador == "Nivel general y divisiones COICOP",
      !is.na(Descripcion),
      Descripcion != ""
    ) %>%
    mutate(
      Periodo    = as.character(Periodo),
      periodos   = as.Date(paste0(substr(Periodo, 1, 4), "-",
                                  substr(Periodo, 5, 6), "-01")),
      # values come as "1,5" (comma decimal) → convert
      v_m   = as.numeric(gsub(",", ".", v_m_IPC)),
      v_ia  = as.numeric(gsub(",", ".", v_i_a_IPC)),
      year  = as.integer(format(periodos, "%Y")),
      Mes   = as.integer(format(periodos, "%m"))
    ) %>%
    select(Descripcion, periodos, year, Mes, v_m, v_ia) %>%
    filter(!is.na(periodos))

  # Pivot wide: one column per Descripcion, values = v_m (monthly variation %)
  wide <- df %>%
    select(periodos, year, Mes, Descripcion, v_m) %>%
    pivot_wider(names_from = Descripcion, values_from = v_m) %>%
    arrange(periodos)

  # Clean column names (remove accents / special chars)
  clean_names <- colnames(wide)
  clean_names <- gsub("[áàä]", "a", clean_names)
  clean_names <- gsub("[éèë]", "e", clean_names)
  clean_names <- gsub("[íìï]", "i", clean_names)
  clean_names <- gsub("[óòö]", "o", clean_names)
  clean_names <- gsub("[úùü]", "u", clean_names)
  clean_names <- gsub("[ñ]",   "n", clean_names)
  clean_names <- gsub(",",     "",  clean_names)
  clean_names <- trimws(clean_names)
  colnames(wide) <- clean_names

  wide
}

indec <- load_ipc()

# Column names that correspond to economic sectors (everything except meta cols)
META_COLS   <- c("periodos", "year", "Mes")
sector_cols <- sort(setdiff(colnames(indec), META_COLS))

# ── Calculation helpers ────────────────────────────────────────────────────────

# Rolling 12-month interannual rate for a single column
interanual <- function(df, col) {
  vals   <- pull(df[, col]) / 100 + 1
  n      <- nrow(df)
  result <- rep(NA_real_, n)
  for (i in 12:n) {
    result[i] <- (prod(vals[(i - 11):i]) - 1) * 100
  }
  result
}

# Interannual rate (scalar) for last available row
interanual_last <- function(df, col) {
  n    <- nrow(df)
  if (n < 12) return(NA_real_)
  vals <- pull(df[, col]) / 100 + 1
  round((prod(vals[(n - 11):n]) - 1) * 100, 1)
}

# Tibble of last-month values for all sectors
data_bar_mensual <- function(df, cols) {
  last_row <- df[nrow(df), ]
  tibble(
    sector = cols,
    tasa   = as.numeric(last_row[, cols])
  )
}

# Tibble of interannual values for all sectors
data_bar_interanual <- function(df, cols) {
  tibble(
    sector = cols,
    tasa   = sapply(cols, function(c) interanual_last(df, c))
  )
}

# Accumulated inflation per year for a column
acumulado_anual <- function(df, col) {
  df %>%
    mutate(
      factor = pull(df[, col]) / 100 + 1,
      Mes_actual = Mes[nrow(df)]
    ) %>%
    filter(Mes <= Mes[nrow(df)]) %>%
    group_by(year) %>%
    summarise(
      prod       = prod(factor, na.rm = TRUE),
      interanual = round((prod - 1) * 100, 2),
      .groups    = "drop"
    )
}

# ── Shared chart theme ─────────────────────────────────────────────────────────

hc_indec_theme <- function(hc) {
  hc %>%
    hc_credits(
      enabled      = TRUE,
      text         = "INDEC",
      href         = "https://www.indec.gob.ar/",
      align        = "right",
      verticalAlign = "bottom",
      style        = list(fontSize = "10px", color = "#555")
    ) %>%
    hc_tooltip(
      crosshairs      = TRUE,
      backgroundColor = "#F0F0F0",
      shared          = TRUE,
      borderWidth     = 3
    ) %>%
    hc_xAxis(
      labels = list(style = list(color = "black", fontWeight = "bold")),
      title  = list(text = "")
    ) %>%
    hc_legend(enabled = FALSE)
}

# ── UI ─────────────────────────────────────────────────────────────────────────

ui <- dashboardPage(
  options  = FALSE,
  preloader = list(
    html  = tagList(spin_6(), " Cargando datos..."),
    color = "#1a237e"
  ),

  header = dashboardHeader(
    skin  = "dark",
    fixed = TRUE,
    title = dashboardBrand(
      title = "EconSur · IPC Argentina",
      image = "https://upload.wikimedia.org/wikipedia/commons/6/64/Logo_Indec.png"
    )
  ),

  sidebar = dashboardSidebar(
    skin  = "light",
    width = 260,
    sidebarMenu(
      menuItem("Principal",       tabName = "dashboard", icon = icon("home")),
      menuItem("Por Sector",      tabName = "bpsector",  icon = icon("chart-bar")),
      menuItem("Comparar Rubros", tabName = "bp",        icon = icon("balance-scale")),
      menuItem("Acerca de",       tabName = "about",     icon = icon("info-circle"))
    )
  ),

  body = dashboardBody(

    tags$head(
      tags$style(HTML("
        .small-box .inner { text-align: center !important; font-size: 34px !important; }
        .nav-tabs .nav-link { font-weight: 600; }
        h3 { color: #1a237e; }
      "))
    ),

    tabItems(

      # ── Tab: Principal ────────────────────────────────────────────────────────
      tabItem(
        tabName = "dashboard",
        h3("Precios – Economía Argentina"),
        p(
          "Variación mensual e interanual del IPC por división COICOP.",
          span(" Datos: INDEC · Región Nacional.", style = "color:#555;")
        ),
        p(style = "color:#555; font-size:13px;",
          paste0(
            "Serie actualizada a ",
            traducir_mes(format(max(indec$periodos), "%B")),
            " de ", max(indec$year), "."
          )
        ),

        fluidRow(
          bs4ValueBoxOutput("vb_mensual",    width = 4),
          bs4ValueBoxOutput("vb_interanual", width = 4),
          bs4ValueBoxOutput("vb_mayor",      width = 4)
        ),

        fluidRow(
          column(
            width = 7,
            bs4Card(
              width    = 12,
              title    = "Serie Interanual",
              status   = "primary",
              solidHeader = FALSE,
              fluidRow(
                column(5,
                  selectInput(
                    "sel_sector", "Rubro:",
                    choices  = sector_cols,
                    selected = "NIVEL GENERAL",
                    width    = "100%"
                  )
                ),
                column(7,
                  radioButtons(
                    "rng_serie", "Período:",
                    choices  = c("Histórico", "Últimos 12 meses"),
                    selected = "Histórico",
                    inline   = TRUE
                  )
                )
              ),
              withSpinner(highchartOutput("plot_timeserie", height = "320px"), type = 4)
            )
          ),
          column(
            width = 5,
            bs4Card(
              width    = 12,
              title    = "Acumulado Anual",
              status   = "primary",
              solidHeader = FALSE,
              withSpinner(highchartOutput("plot_acumulado", height = "320px"), type = 4)
            )
          )
        )
      ),

      # ── Tab: Por Sector ───────────────────────────────────────────────────────
      tabItem(
        tabName = "bpsector",
        h3("Variación Mensual por Rubro"),
        p(paste0(
          "Datos a ", traducir_mes(format(max(indec$periodos), "%B")),
          " de ", max(indec$year), "."
        )),
        bs4Card(
          width  = 12,
          title  = "Evolución de un Rubro",
          status = "info",
          fluidRow(
            column(5,
              selectInput(
                "sel_sector2", "Rubro:",
                choices  = sector_cols,
                selected = "NIVEL GENERAL",
                width    = "100%"
              )
            ),
            column(7,
              radioButtons(
                "rng_sector", "Período:",
                choices  = c("Histórico", "Últimos 12 meses"),
                selected = "Histórico",
                inline   = TRUE
              )
            )
          ),
          withSpinner(highchartOutput("plot_sector_barras", height = "380px"), type = 4)
        )
      ),

      # ── Tab: Comparar Rubros ──────────────────────────────────────────────────
      tabItem(
        tabName = "bp",
        h3("Comparación de Rubros"),
        p(paste0(
          "Variación mensual e interanual a ",
          traducir_mes(format(max(indec$periodos), "%B")),
          " de ", max(indec$year), "."
        )),
        bs4Card(
          width  = 12,
          title  = "Todos los Rubros",
          status = "success",
          radioButtons(
            "tipo_comparar", "Mostrar:",
            choices  = c("Mensual", "Interanual"),
            selected = "Mensual",
            inline   = TRUE
          ),
          withSpinner(highchartOutput("plot_comparar", height = "420px"), type = 4)
        )
      ),

      # ── Tab: Acerca de ────────────────────────────────────────────────────────
      tabItem(
        tabName = "about",
        h2("Acerca de EconSur · IPC"),
        fluidRow(
          bs4Card(
            width       = 7,
            title       = strong("Dashboard IPC Argentina"),
            solidHeader = TRUE,
            status      = "primary",
            p("Este dashboard visualiza la variación mensual e interanual del ",
              strong("Índice de Precios al Consumidor (IPC)"),
              " publicado por el INDEC, para todas las divisiones COICOP a nivel Nacional."),
            tags$ul(
              tags$li("Variación mensual por rubro."),
              tags$li("Tasa interanual rodante (12 meses)."),
              tags$li("Inflación acumulada por año."),
              tags$li("Comparación de rubros en un período seleccionado.")
            ),
            p("Fuente oficial: ", tags$a(href = "https://www.indec.gob.ar/", "INDEC")),
            p("Archivo: ", code("serie_ipc_divisiones.csv"), " · Regiones disponibles: GBA, Pampeana, Noreste, Noroeste, Cuyo, Patagonia, Nacional.")
          ),
          bs4Card(
            width       = 5,
            title       = strong("Proyecto EconSur"),
            solidHeader = TRUE,
            status      = "info",
            p("Repositorio: ", tags$a(href = "https://github.com/econsur-inflacion", "GitHub")),
            p("Stack tecnológico:"),
            tags$ul(
              tags$li(tags$a(href = "https://shiny.rstudio.com/", "Shiny")),
              tags$li(tags$a(href = "https://rinterface.github.io/bs4Dash/", "bs4Dash")),
              tags$li(tags$a(href = "https://jkunst.com/highcharter/", "Highcharter")),
              tags$li(tags$a(href = "https://render.com/", "Render (deploy)"))
            )
          )
        )
      )
    )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # Reactive: filtered data for chosen sector (Principal tab)
  rv <- reactiveValues()

  observe({
    rv$df <- indec
  })

  # ── Time serie interanual (rolling 12m) ──────────────────────────────────────
  output$plot_timeserie <- renderHighchart({
    req(input$sel_sector, rv$df)
    col <- input$sel_sector
    df  <- rv$df

    ts_data <- tibble(
      x = df$periodos[12:nrow(df)],
      y = round(interanual(df, col)[12:nrow(df)], 1)
    ) %>% filter(!is.na(y))

    if (input$rng_serie == "Últimos 12 meses") ts_data <- tail(ts_data, 12)

    hchart(ts_data, "line", hcaes(x = x, y = y),
           color = "#1565c0", name = col) %>%
      hc_indec_theme() %>%
      hc_title(
        text  = paste0("Inflación Interanual · ", col),
        style = list(fontSize = "15px", fontWeight = "bold")
      ) %>%
      hc_subtitle(
        text  = "Tasa acumulada 12 meses – variación %",
        style = list(fontSize = "11px", color = "#555")
      ) %>%
      hc_xAxis(
        type                  = "datetime",
        dateTimeLabelFormats  = list(month = "%b %Y"),
        labels = list(style   = list(color = "black", fontWeight = "bold"))
      ) %>%
      hc_yAxis(
        title  = list(text = "% Interanual",
                      style = list(color = "black", fontWeight = "bold")),
        gridLineWidth = 0,
        labels = list(style = list(color = "black", fontWeight = "bold"))
      ) %>%
      hc_tooltip(pointFormat = "Interanual: <b>{point.y:.1f} %</b>")
  })

  # ── Acumulado anual (barras) ─────────────────────────────────────────────────
  output$plot_acumulado <- renderHighchart({
    req(input$sel_sector, rv$df)
    col  <- input$sel_sector
    data <- acumulado_anual(rv$df, col)

    hchart(data, "column", hcaes(x = year, y = interanual),
           color       = "#1a237e",
           showInLegend = FALSE,
           dataLabels  = list(enabled = TRUE, format = "{point.y:.1f} %")) %>%
      hc_indec_theme() %>%
      hc_title(
        text  = paste0("Acumulado anual · ", col),
        style = list(fontSize = "15px", fontWeight = "bold")
      ) %>%
      hc_subtitle(
        text  = paste0(
          "Hasta ", traducir_mes(format(max(rv$df$periodos), "%B")),
          " de ", max(rv$df$year)
        ),
        style = list(fontSize = "11px", color = "#555")
      ) %>%
      hc_yAxis(
        title  = list(text = "% Acumulado",
                      style = list(color = "black", fontWeight = "bold")),
        gridLineWidth = 0
      ) %>%
      hc_tooltip(pointFormat = "Acumulado: <b>{point.y:.1f} %</b>")
  })

  # ── Barras por sector – variación mensual ────────────────────────────────────
  output$plot_sector_barras <- renderHighchart({
    req(input$sel_sector2)
    col <- input$sel_sector2
    df  <- if (input$rng_sector == "Últimos 12 meses") tail(indec, 12) else indec

    plot_df <- tibble(
      periodos = df$periodos,
      valor    = pull(df[, col])
    ) %>% filter(!is.na(valor))

    hchart(plot_df, "column", hcaes(x = periodos, y = valor),
           color = "#0d47a1", name = "Variación %") %>%
      hc_indec_theme() %>%
      hc_title(
        text  = paste0("Variación mensual · ", col),
        style = list(fontSize = "15px", fontWeight = "bold")
      ) %>%
      hc_xAxis(
        type                 = "datetime",
        dateTimeLabelFormats = list(month = "%b %Y"),
        labels = list(style  = list(color = "black", fontWeight = "bold"))
      ) %>%
      hc_yAxis(
        title  = list(text = "% Mensual",
                      style = list(color = "black", fontWeight = "bold")),
        gridLineWidth = 0
      ) %>%
      hc_tooltip(pointFormat = "Variación: <b>{point.y:.2f} %</b>")
  })

  # ── Comparar todos los rubros ─────────────────────────────────────────────────
  output$plot_comparar <- renderHighchart({
    req(input$tipo_comparar)

    data <- if (input$tipo_comparar == "Mensual") {
      data_bar_mensual(indec, sector_cols)
    } else {
      data_bar_interanual(indec, sector_cols)
    }

    data <- data %>%
      filter(!is.na(tasa)) %>%
      arrange(desc(tasa))

    n_sectors <- nrow(data)
    colors    <- viridis::mako(n_sectors, direction = -1)

    hchart(data, "column",
           hcaes(x = sector, y = round(tasa, 2), color = colors),
           showInLegend = FALSE,
           dataLabels   = list(enabled = TRUE, format = "{point.y:.1f} %")) %>%
      hc_indec_theme() %>%
      hc_title(
        text  = paste0("IPC ", input$tipo_comparar, " por Rubro"),
        style = list(fontSize = "15px", fontWeight = "bold")
      ) %>%
      hc_subtitle(
        text  = paste0(
          traducir_mes(format(max(indec$periodos), "%B")),
          " de ", max(indec$year)
        ),
        style = list(fontSize = "11px", color = "#555")
      ) %>%
      hc_xAxis(
        labels = list(
          rotation = -35,
          style    = list(color = "black", fontWeight = "bold", fontSize = "11px")
        )
      ) %>%
      hc_yAxis(
        title  = list(text = paste0("% ", input$tipo_comparar),
                      style = list(color = "black", fontWeight = "bold")),
        gridLineWidth = 0
      ) %>%
      hc_tooltip(
        pointFormat = paste0(input$tipo_comparar, ": <b>{point.y:.2f} %</b>")
      )
  })

  # ── Value Boxes ───────────────────────────────────────────────────────────────
  output$vb_mensual <- renderbs4ValueBox({
    req(input$sel_sector, rv$df)
    col <- input$sel_sector
    val <- round(pull(rv$df[nrow(rv$df), col]), 1)
    bs4ValueBox(
      value    = paste0(val, " %"),
      subtitle = paste0("Mensual · ", col),
      icon     = icon("percent"),
      color    = "info",
      footer   = div(paste0(
        "Variación en ",
        traducir_mes(format(max(rv$df$periodos), "%B")),
        " ", max(rv$df$year)
      ))
    )
  })

  output$vb_interanual <- renderbs4ValueBox({
    req(input$sel_sector, rv$df)
    col <- input$sel_sector
    val <- interanual_last(rv$df, col)
    bs4ValueBox(
      value    = paste0(val, " %"),
      subtitle = paste0("Interanual · ", col),
      icon     = icon("chart-line"),
      color    = "primary",
      gradient = TRUE,
      footer   = div(paste0(
        "Acumulado hasta ",
        traducir_mes(format(max(rv$df$periodos), "%B")),
        " ", max(rv$df$year)
      ))
    )
  })

  output$vb_mayor <- renderbs4ValueBox({
    req(rv$df)
    last <- rv$df[nrow(rv$df), sector_cols]
    vals <- as.numeric(last)
    idx  <- which.max(vals)
    bs4ValueBox(
      value    = paste0(round(vals[idx], 1), " %"),
      subtitle = paste0("Mayor alza: ", sector_cols[idx]),
      icon     = icon("arrow-trend-up"),
      color    = "danger",
      footer   = div(paste0(
        "Mayor incremento mensual en ",
        traducir_mes(format(max(rv$df$periodos), "%B")),
        " ", max(rv$df$year)
      ))
    )
  })
}

shinyApp(ui = ui, server = server)

    
