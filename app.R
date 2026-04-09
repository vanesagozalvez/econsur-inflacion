library(shiny)
library(bs4Dash)
library(dplyr)
library(tidyr)
library(stringr)
library(shinycssloaders)
library(waiter)
library(highcharter)
library(viridis)
library(lubridate)

cat("[STARTUP] Packages loaded\n")
cat("[STARTUP] Working dir:", getwd(), "\n")
cat("[STARTUP] CSV exists:", file.exists("data/serie_ipc_divisiones.csv"), "\n")

traducir_mes <- function(mes_ingles) {
  meses    <- c("january","february","march","april","may","june",
                "july","august","september","october","november","december")
  meses_es <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
  idx <- match(tolower(mes_ingles), meses)
  if (!is.na(idx)) meses_es[idx] else mes_ingles
}

limpiar_nombre <- function(x) {
  x <- gsub("[áàä]","a",x); x <- gsub("[éèë]","e",x)
  x <- gsub("[íìï]","i",x); x <- gsub("[óòö]","o",x)
  x <- gsub("[úùü]","u",x); x <- gsub("[ñ]",  "n",x)
  x <- gsub(",","",x); trimws(x)
}

load_ipc <- function(path = "data/serie_ipc_divisiones.csv") {
  raw <- read.csv2(path, encoding = "latin1", stringsAsFactors = FALSE, na.strings = c("NA",""))
  parse_num <- function(x) as.numeric(gsub(",",".",x))
  base <- raw %>%
    mutate(
      periodos = as.Date(paste0(substr(as.character(Periodo),1,4),"-",substr(as.character(Periodo),5,6),"-01")),
      year = as.integer(format(periodos,"%Y")),
      Mes  = as.integer(format(periodos,"%m")),
      v_m  = parse_num(v_m_IPC),
      v_ia = parse_num(v_i_a_IPC)
    )

  div <- base %>%
    filter(Region=="Nacional", Clasificador=="Nivel general y divisiones COICOP", !is.na(Descripcion)) %>%
    select(periodos,year,Mes,Descripcion,v_m) %>%
    pivot_wider(names_from=Descripcion, values_from=v_m) %>%
    arrange(periodos)
  colnames(div) <- limpiar_nombre(colnames(div))

  cats <- base %>%
    filter(Region=="Nacional", Clasificador=="Categorias") %>%
    mutate(cat_nombre = limpiar_nombre(Codigo)) %>%
    select(periodos,year,Mes,cat_nombre,v_m,v_ia) %>%
    arrange(periodos)

  bys <- base %>%
    filter(Region=="Nacional", Clasificador=="Bienes y servicios") %>%
    mutate(bys_nombre = ifelse(Codigo=="B","Bienes","Servicios")) %>%
    select(periodos,year,Mes,bys_nombre,v_m,v_ia) %>%
    arrange(periodos)

  list(div=div, cats=cats, bys=bys)
}

datos  <- load_ipc()
indec  <- datos$div
cats   <- datos$cats
bys    <- datos$bys

META_COLS   <- c("periodos","year","Mes")
sector_cols <- sort(setdiff(colnames(indec), META_COLS))

cat("[STARTUP] Rows:", nrow(indec), "| Sectors:", length(sector_cols), "\n")
cat("[STARTUP] Ultimo periodo:", as.character(max(indec$periodos)), "\n")

interanual_rolling <- function(df, col) {
  vals   <- as.numeric(pull(df[,col])) / 100 + 1
  n      <- nrow(df)
  result <- rep(NA_real_, n)
  for (i in 12:n) {
    w <- vals[(i-11):i]
    if (!any(is.na(w))) result[i] <- (prod(w)-1)*100
  }
  round(result, 1)
}

interanual_last_wide <- function(df, col) {
  n <- nrow(df)
  if (n < 12) return(NA_real_)
  vals <- as.numeric(pull(df[,col])) / 100 + 1
  w <- vals[(n-11):n]
  if (any(is.na(w))) return(NA_real_)
  round((prod(w)-1)*100, 1)
}

data_bar_mensual <- function(df, cols) {
  last_row <- df[nrow(df),]
  tibble(sector=cols, tasa=sapply(cols, function(c) { v <- as.numeric(last_row[[c]]); if(is.na(v)) NA_real_ else v }))
}

data_bar_interanual_wide <- function(df, cols) {
  tibble(sector=cols, tasa=sapply(cols, function(c) interanual_last_wide(df,c)))
}

acumulado_anual <- function(df, col) {
  mes_max <- max(df$Mes)
  df %>%
    mutate(factor=as.numeric(pull(df[,col]))/100+1) %>%
    filter(!is.na(factor), Mes<=mes_max) %>%
    group_by(year) %>%
    summarise(prod=prod(factor,na.rm=TRUE),.groups="drop") %>%
    mutate(interanual=round((prod-1)*100,2))
}

hc_indec_theme <- function(hc) {
  hc %>%
    hc_credits(enabled=TRUE,text="INDEC",href="https://www.indec.gob.ar/",
               align="right",verticalAlign="bottom",style=list(fontSize="10px",color="#555")) %>%
    hc_tooltip(crosshairs=TRUE,backgroundColor="#F0F0F0",shared=TRUE,borderWidth=3) %>%
    hc_legend(enabled=FALSE)
}

COLORES_LINEAS <- c(
  "Nucleo"        = "#1565c0",
  "Regulados"     = "#c62828",
  "Estacional"    = "#2e7d32",
  "Bienes"        = "#f57f17",
  "Servicios"     = "#6a1b9a",
  "NIVEL GENERAL" = "#37474f"
)

ui <- dashboardPage(
  options=FALSE,
  preloader=list(html=tagList(spin_6()," Cargando..."),color="#1a237e"),
  header=dashboardHeader(
    skin="dark", fixed=TRUE,
    title=dashboardBrand(title="EconSur · IPC Argentina",
      image="https://upload.wikimedia.org/wikipedia/commons/6/64/Logo_Indec.png")
  ),
  sidebar=dashboardSidebar(skin="light",width=270,
    sidebarMenu(
      menuItem("Principal",         tabName="dashboard", icon=icon("home")),
      menuItem("Por Sector",        tabName="bpsector",  icon=icon("chart-bar")),
      menuItem("Comparar Rubros",   tabName="bp",        icon=icon("balance-scale")),
      menuItem("IPC por Categoria", tabName="catlineas", icon=icon("chart-line")),
      menuItem("Acerca de",         tabName="about",     icon=icon("info-circle"))
    )
  ),
  body=dashboardBody(
    tags$head(tags$style(HTML("
      .small-box .inner{text-align:center!important;font-size:34px!important;}
      h3{color:#1a237e;}
    "))),
    tabItems(

      # ── Principal ──
      tabItem(tabName="dashboard",
        h3("Precios – Economia Argentina"),
        p(paste0("Serie actualizada a ",traducir_mes(format(max(indec$periodos),"%B"))," de ",max(indec$year),". Fuente: INDEC.")),
        fluidRow(
          bs4ValueBoxOutput("vb_mensual",   width=4),
          bs4ValueBoxOutput("vb_interanual",width=4),
          bs4ValueBoxOutput("vb_mayor",     width=4)
        ),
        fluidRow(
          column(width=7,
            bs4Card(width=12,title="Serie Interanual",status="primary",
              fluidRow(
                column(5,selectInput("sel_sector","Rubro:",choices=sector_cols,selected="NIVEL GENERAL",width="100%")),
                column(7,radioButtons("rng_serie","Periodo:",choices=c("Historico","Ultimos 12 meses"),selected="Historico",inline=TRUE))
              ),
              withSpinner(highchartOutput("plot_timeserie",height="320px"),type=4)
            )
          ),
          column(width=5,
            bs4Card(width=12,title="Acumulado Anual",status="primary",
              withSpinner(highchartOutput("plot_acumulado",height="320px"),type=4)
            )
          )
        )
      ),

      # ── Por Sector ──
      tabItem(tabName="bpsector",
        h3("Variacion por Rubro"),
        p(paste0("Datos a ",traducir_mes(format(max(indec$periodos),"%B"))," de ",max(indec$year),".")),
        bs4Card(width=12,title="Evolucion de un Rubro",status="info",
          fluidRow(
            column(4,selectInput("sel_sector2","Rubro:",choices=sector_cols,selected="NIVEL GENERAL",width="100%")),
            column(4,radioButtons("tipo_sector","Metrica:",choices=c("Mensual","Interanual"),selected="Mensual",inline=TRUE)),
            column(4,radioButtons("rng_sector","Periodo:",choices=c("Historico","Ultimos 12 meses"),selected="Historico",inline=TRUE))
          ),
          withSpinner(highchartOutput("plot_sector_barras",height="400px"),type=4)
        )
      ),

      # ── Comparar Rubros ──
      tabItem(tabName="bp",
        h3("Comparacion de Rubros"),
        p(paste0("Variacion mensual e interanual a ",traducir_mes(format(max(indec$periodos),"%B"))," de ",max(indec$year),".")),
        bs4Card(width=12,title="Todos los Rubros",status="success",
          radioButtons("tipo_comparar","Mostrar:",choices=c("Mensual","Interanual"),selected="Mensual",inline=TRUE),
          withSpinner(highchartOutput("plot_comparar",height="520px"),type=4)
        )
      ),

      # ── IPC por Categoria ──
      tabItem(tabName="catlineas",
        h3("IPC por Categoria"),
        p("Evolucion del IPC: Nucleo, Regulados, Estacional, Bienes, Servicios y Nivel General."),
        bs4Card(width=12,title="Serie Temporal Multivariable",status="primary",
          fluidRow(
            column(5,radioButtons("rng_catlineas","Periodo:",
              choices=c("Historico","Ultimos 24 meses","Ultimos 12 meses"),
              selected="Historico",inline=TRUE)),
            column(4,radioButtons("metrica_cat","Metrica:",
              choices=c("Mensual","Interanual"),selected="Mensual",inline=TRUE))
          ),
          withSpinner(highchartOutput("plot_catlineas",height="460px"),type=4)
        ),
        fluidRow(
          column(6,
            bs4Card(width=12,title="Categorias – Ultimo mes",status="info",
              withSpinner(highchartOutput("plot_cat_barras",height="320px"),type=4)
            )
          ),
          column(6,
            bs4Card(width=12,title="Bienes vs Servicios – Ultimo mes",status="warning",
              withSpinner(highchartOutput("plot_bys_barras",height="320px"),type=4)
            )
          )
        )
      ),

      # ── Acerca de ──
      tabItem(tabName="about",
        h2("Acerca de EconSur - IPC"),
        bs4Card(width=8,title=strong("Dashboard IPC Argentina"),solidHeader=TRUE,status="primary",
          p("IPC INDEC – todas las divisiones COICOP nivel Nacional."),
          tags$ul(
            tags$li("Variacion mensual e interanual por rubro."),
            tags$li("Acumulado anual."),
            tags$li("Comparacion horizontal de todos los rubros."),
            tags$li("Multilinea: Nucleo, Regulados, Estacional, Bienes, Servicios, Nivel General.")
          ),
          p("Fuente: ",tags$a(href="https://www.indec.gob.ar/","INDEC")),
          p("Deploy: ",tags$a(href="https://render.com/","Render"))
        )
      )
    )
  )
)

server <- function(input, output, session) {

  output$vb_mensual <- renderbs4ValueBox({
    col <- req(input$sel_sector)
    val <- round(as.numeric(indec[[col]][nrow(indec)]),1)
    bs4ValueBox(value=paste0(val," %"),subtitle=paste0("Mensual - ",col),
      icon=icon("percent"),color="info",
      footer=div(paste0("Variacion en ",traducir_mes(format(max(indec$periodos),"%B"))," ",max(indec$year))))
  })

  output$vb_interanual <- renderbs4ValueBox({
    col <- req(input$sel_sector)
    val <- interanual_last_wide(indec,col)
    bs4ValueBox(value=paste0(val," %"),subtitle=paste0("Interanual - ",col),
      icon=icon("chart-line"),color="primary",gradient=TRUE,
      footer=div(paste0("Acumulado 12m hasta ",traducir_mes(format(max(indec$periodos),"%B"))," ",max(indec$year))))
  })

  output$vb_mayor <- renderbs4ValueBox({
    vals <- sapply(sector_cols, function(c) as.numeric(indec[[c]][nrow(indec)]))
    idx  <- which.max(vals)
    bs4ValueBox(value=paste0(round(vals[idx],1)," %"),
      subtitle=paste0("Mayor alza: ",sector_cols[idx]),
      icon=icon("arrow-trend-up"),color="danger",
      footer=div(paste0("Mayor incremento mensual en ",traducir_mes(format(max(indec$periodos),"%B"))," ",max(indec$year))))
  })

  output$plot_timeserie <- renderHighchart({
    col <- req(input$sel_sector)
    ts  <- tibble(x=indec$periodos[12:nrow(indec)],
                  y=interanual_rolling(indec,col)[12:nrow(indec)]) %>% filter(!is.na(y))
    if(input$rng_serie=="Ultimos 12 meses") ts <- tail(ts,12)
    hchart(ts,"line",hcaes(x=x,y=y),color="#1565c0",name=col) %>%
      hc_indec_theme() %>%
      hc_title(text=paste0("Inflacion Interanual - ",col),style=list(fontSize="15px",fontWeight="bold")) %>%
      hc_xAxis(type="datetime",labels=list(style=list(color="black",fontWeight="bold"))) %>%
      hc_yAxis(title=list(text="% Interanual",style=list(color="black",fontWeight="bold")),gridLineWidth=0) %>%
      hc_tooltip(pointFormat="Interanual: <b>{point.y:.1f} %</b>")
  })

  output$plot_acumulado <- renderHighchart({
    col  <- req(input$sel_sector)
    data <- acumulado_anual(indec,col)
    hchart(data,"column",hcaes(x=year,y=interanual),color="#1a237e",showInLegend=FALSE,
           dataLabels=list(enabled=TRUE,format="{point.y:.1f} %")) %>%
      hc_indec_theme() %>%
      hc_title(text=paste0("Acumulado anual - ",col),style=list(fontSize="15px",fontWeight="bold")) %>%
      hc_yAxis(title=list(text="% Acumulado",style=list(color="black",fontWeight="bold")),gridLineWidth=0) %>%
      hc_tooltip(pointFormat="Acumulado: <b>{point.y:.1f} %</b>")
  })

  output$plot_sector_barras <- renderHighchart({
    col    <- req(input$sel_sector2)
    metodo <- input$tipo_sector
    df     <- if(input$rng_sector=="Ultimos 12 meses") tail(indec,12) else indec

    if(metodo=="Mensual"){
      plot_df <- tibble(x=df$periodos, y=as.numeric(pull(df[,col]))) %>% filter(!is.na(y))
      ytitle  <- "% Mensual"; fmt <- "Variacion: <b>{point.y:.2f} %</b>"
    } else {
      ia      <- interanual_rolling(indec,col)
      plot_df <- tibble(x=indec$periodos,y=ia) %>% filter(!is.na(y))
      if(input$rng_sector=="Ultimos 12 meses") plot_df <- tail(plot_df,12)
      ytitle  <- "% Interanual"; fmt <- "Interanual: <b>{point.y:.2f} %</b>"
    }

    hchart(plot_df,"column",hcaes(x=x,y=y),color="#0d47a1",name=paste(metodo,col)) %>%
      hc_indec_theme() %>%
      hc_title(text=paste0("Variacion ",metodo," - ",col),style=list(fontSize="15px",fontWeight="bold")) %>%
      hc_xAxis(type="datetime",dateTimeLabelFormats=list(month="%b %Y"),
               labels=list(style=list(color="black",fontWeight="bold"))) %>%
      hc_yAxis(title=list(text=ytitle,style=list(color="black",fontWeight="bold")),gridLineWidth=0) %>%
      hc_tooltip(pointFormat=fmt)
  })

  output$plot_comparar <- renderHighchart({
    tipo <- req(input$tipo_comparar)
    data <- if(tipo=="Mensual") data_bar_mensual(indec,sector_cols) else data_bar_interanual_wide(indec,sector_cols)
    data <- data %>%
      filter(!is.na(tasa)) %>%
      arrange(tasa) %>%
      mutate(tasa=round(tasa,2),
             color=viridis::mako(n(),begin=0.15,end=0.85,direction=1))

    highchart() %>%
      hc_chart(type="bar") %>%
      hc_xAxis(categories=data$sector,
               labels=list(style=list(color="black",fontWeight="bold",fontSize="12px"))) %>%
      hc_yAxis(title=list(text=paste0("% ",tipo),style=list(color="black",fontWeight="bold")),
               gridLineWidth=1,labels=list(style=list(color="black"))) %>%
      hc_add_series(name=tipo,showInLegend=FALSE,
        data=lapply(seq_len(nrow(data)),function(i) list(y=data$tasa[i],color=data$color[i])),
        dataLabels=list(enabled=TRUE,format="{point.y:.1f} %",
                        style=list(fontSize="11px",fontWeight="bold",textOutline="none",color="black"))
      ) %>%
      hc_indec_theme() %>%
      hc_title(text=paste0("IPC ",tipo," por Rubro"),style=list(fontSize="15px",fontWeight="bold")) %>%
      hc_subtitle(text=paste0(traducir_mes(format(max(indec$periodos),"%B"))," de ",max(indec$year)),
                  style=list(fontSize="11px",color="#555")) %>%
      hc_tooltip(pointFormat=paste0(tipo,": <b>{point.y:.2f} %</b>")) %>%
      hc_plotOptions(bar=list(borderRadius=3,groupPadding=0.05,pointPadding=0.05))
  })

  output$plot_catlineas <- renderHighchart({
    metrica  <- input$metrica_cat
    rng      <- input$rng_catlineas
    col_val  <- if(metrica=="Mensual") "v_m" else "v_ia"

    ng_vals <- if(metrica=="Mensual") {
      as.numeric(indec[["NIVEL GENERAL"]])
    } else {
      interanual_rolling(indec,"NIVEL GENERAL")
    }
    ng <- tibble(periodos=indec$periodos, serie="NIVEL GENERAL", val=ng_vals)

    cats_prep <- cats %>% rename(serie=cat_nombre,val=all_of(col_val)) %>% select(periodos,serie,val)
    bys_prep  <- bys  %>% rename(serie=bys_nombre,val=all_of(col_val)) %>% select(periodos,serie,val)

    df_all <- bind_rows(cats_prep,bys_prep,ng) %>%
      filter(!is.na(val),!is.na(periodos)) %>% arrange(periodos)

    max_p <- max(df_all$periodos)
    if(rng=="Ultimos 24 meses") df_all <- df_all %>% filter(periodos >= max_p %m-% months(24))
    if(rng=="Ultimos 12 meses") df_all <- df_all %>% filter(periodos >= max_p %m-% months(12))

    series_names <- c("Nucleo","Regulados","Estacional","Bienes","Servicios","NIVEL GENERAL")
    series_names <- intersect(series_names, unique(df_all$serie))

    hc <- highchart() %>%
      hc_chart(type="line") %>%
      hc_xAxis(type="datetime",dateTimeLabelFormats=list(month="%b %Y"),
               labels=list(style=list(color="black",fontWeight="bold"))) %>%
      hc_yAxis(title=list(text=paste0("% ",metrica),style=list(color="black",fontWeight="bold")),
               gridLineWidth=1,labels=list(style=list(color="black"))) %>%
      hc_legend(enabled=TRUE,itemStyle=list(fontWeight="bold",fontSize="12px")) %>%
      hc_title(text=paste0("IPC por Categoria - ",metrica),style=list(fontSize="15px",fontWeight="bold")) %>%
      hc_subtitle(text=paste0(traducir_mes(format(max(indec$periodos),"%B"))," de ",max(indec$year)),
                  style=list(fontSize="11px",color="#555")) %>%
      hc_tooltip(shared=TRUE,
                 pointFormat="<span style='color:{series.color}'>&#9679;</span> {series.name}: <b>{point.y:.2f} %</b><br/>") %>%
      hc_credits(enabled=TRUE,text="INDEC",href="https://www.indec.gob.ar/",
                 align="right",verticalAlign="bottom",style=list(fontSize="10px",color="#555"))

    for(s in series_names){
      sdata <- df_all %>% filter(serie==s) %>% arrange(periodos)
      col_s <- COLORES_LINEAS[s]; if(is.na(col_s)) col_s <- "#607d8b"
      hc <- hc %>% hc_add_series(
        name=s,
        data=list_parse2(tibble(x=datetime_to_timestamp(sdata$periodos),y=round(sdata$val,2))),
        color=col_s, marker=list(radius=2)
      )
    }
    hc
  })

  output$plot_cat_barras <- renderHighchart({
    last_cats <- cats %>%
      filter(!is.na(v_m)) %>% group_by(cat_nombre) %>% slice_max(periodos,n=1) %>% ungroup() %>%
      arrange(desc(v_m)) %>%
      mutate(color=viridis::mako(n(),begin=0.2,end=0.8,direction=-1))
    highchart() %>% hc_chart(type="column") %>%
      hc_xAxis(categories=last_cats$cat_nombre,
               labels=list(style=list(color="black",fontWeight="bold"))) %>%
      hc_yAxis(title=list(text="% Mensual",style=list(color="black",fontWeight="bold")),gridLineWidth=0) %>%
      hc_add_series(name="Mensual",showInLegend=FALSE,
        data=lapply(seq_len(nrow(last_cats)),function(i) list(y=round(last_cats$v_m[i],2),color=last_cats$color[i])),
        dataLabels=list(enabled=TRUE,format="{point.y:.1f} %",
                        style=list(fontSize="12px",textOutline="none",fontWeight="bold"))
      ) %>%
      hc_indec_theme() %>%
      hc_title(text="Variacion Mensual por Categoria",style=list(fontSize="14px",fontWeight="bold")) %>%
      hc_subtitle(text=paste0(traducir_mes(format(max(indec$periodos),"%B"))," de ",max(indec$year)),
                  style=list(fontSize="11px",color="#555")) %>%
      hc_tooltip(pointFormat="Mensual: <b>{point.y:.2f} %</b>")
  })

  output$plot_bys_barras <- renderHighchart({
    last_bys <- bys %>%
      filter(!is.na(v_m)) %>% group_by(bys_nombre) %>% slice_max(periodos,n=1) %>% ungroup() %>%
      arrange(desc(v_m)) %>% mutate(color=c("#f57f17","#6a1b9a")[seq_len(n())])
    highchart() %>% hc_chart(type="column") %>%
      hc_xAxis(categories=last_bys$bys_nombre,
               labels=list(style=list(color="black",fontWeight="bold",fontSize="13px"))) %>%
      hc_yAxis(title=list(text="% Mensual",style=list(color="black",fontWeight="bold")),gridLineWidth=0) %>%
      hc_add_series(name="Mensual",showInLegend=FALSE,
        data=lapply(seq_len(nrow(last_bys)),function(i) list(y=round(last_bys$v_m[i],2),color=last_bys$color[i])),
        dataLabels=list(enabled=TRUE,format="{point.y:.1f} %",
                        style=list(fontSize="13px",textOutline="none",fontWeight="bold"))
      ) %>%
      hc_indec_theme() %>%
      hc_title(text="Bienes vs Servicios - Variacion Mensual",style=list(fontSize="14px",fontWeight="bold")) %>%
      hc_subtitle(text=paste0(traducir_mes(format(max(indec$periodos),"%B"))," de ",max(indec$year)),
                  style=list(fontSize="11px",color="#555")) %>%
      hc_tooltip(pointFormat="Mensual: <b>{point.y:.2f} %</b>")
  })

}

shinyApp(ui=ui, server=server)

                                            
    
    
    
