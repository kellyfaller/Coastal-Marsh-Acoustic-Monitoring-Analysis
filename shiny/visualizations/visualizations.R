# =============================================================
# Coastal Marsh Acoustic Monitoring — Visualization Dashboard
# Time Series + Site Map + Method Comparison
# =============================================================

library(shiny)
library(leaflet)
library(DBI)
library(RPostgres)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(glue)

# ── Connection ───────────────────────────────────────────
con_info <- list(
  host = "aws-0-us-west-2.pooler.supabase.com", dbname = "postgres",
  user = "postgres.nfyvxsccktlojkvvnvdb", password = "spartinapatens", port =  5432, 
  sslmode  = "require"
)

get_con <- function() do.call(dbConnect, c(list(RPostgres::Postgres()), con_info))

# =============================================================
# LOOKUPS
# point_counts uses "Cattus Island" (space)
# birdnet uses "Cattus_Island" (underscore)
# =============================================================
species_choices <- c(
  "Saltmarsh Sparrow" = "SALS",
  "Black Rail"        = "BLRA",
  "Virginia Rail"     = "VIRA",
  "Clapper Rail"      = "CLRA",
  "Nelson's Sparrow"  = "NESP",
  "Seaside Sparrow"   = "SESP"
)

birdnet_name <- c(
  SALS = "Saltmarsh Sparrow",
  BLRA = "Black Rail",
  VIRA = "Virginia Rail",
  CLRA = "Clapper Rail",
  NESP = "Nelson",
  SESP = "Seaside Sparrow"
)

# Site name as stored in each table
# point_counts: "Cattus Island", "Lighthouse Center" (spaces)
# birdnet:      "Cattus_Island", "Lighthouse_Center" (underscores)
site_choices <- c(
  "Cattus Island"     = "Cattus Island",
  "Lighthouse Center" = "Lighthouse Center"
)

# =============================================================
# SHARED SIDEBAR
# =============================================================
shared_controls <- function() {
  tagList(
    tags$div(
      style = "background:#f8f8f8; border-radius:8px;
               padding:14px; margin-bottom:8px;",
      tags$p("Filters",
             style = "font-size:10px; letter-spacing:0.1em;
                      text-transform:uppercase; color:#999;
                      margin-bottom:10px;"),
      selectInput("species", "Species:",
                  choices  = species_choices,
                  selected = "SALS"
      ),
      selectInput("site", "Site:",
                  choices  = site_choices,
                  selected = "Cattus Island"
      ),
      sliderInput("conf", "BirdNET min confidence:",
                  min = 0, max = 1, value = 0.5, step = 0.05
      ),
      checkboxGroupInput("methods", "Methods to show:",
                         choices  = c("BirdNET"                = "birdnet",
                                      "Arbimon Pattern Matching" = "pm",
                                      "Point Counts"             = "pc"),
                         selected = c("birdnet", "pm", "pc")
      )
    )
  )
}

# =============================================================
# UI
# =============================================================
ui <- navbarPage(
  title = "Coastal Marsh Acoustic Monitoring",
  
  # ── Tab 1: Time Series ─────────────────────────────────────
  tabPanel("Detection Time Series",
           tags$br(),
           fluidRow(
             column(3, shared_controls()),
             column(9,
                    plotOutput("ts_plot", height = "380px"),
                    tags$hr(),
                    fluidRow(
                      column(4, tags$div(
                        style = "text-align:center; padding:10px;
                     background:#f8f8f8; border-radius:8px;",
                        tags$h3(textOutput("ts_total_bn")),
                        tags$p("BirdNET detections",
                               style = "font-size:11px; color:#666;")
                      )),
                      column(4, tags$div(
                        style = "text-align:center; padding:10px;
                     background:#f8f8f8; border-radius:8px;",
                        tags$h3(textOutput("ts_total_pm")),
                        tags$p("Pattern matching hits",
                               style = "font-size:11px; color:#666;")
                      )),
                      column(4, tags$div(
                        style = "text-align:center; padding:10px;
                     background:#f8f8f8; border-radius:8px;",
                        tags$h3(textOutput("ts_total_pc")),
                        tags$p("Point count observations",
                               style = "font-size:11px; color:#666;")
                      ))
                    )
             )
           )
  ),
  
  # ── Tab 2: Site Map ─────────────────────────────────────────
  tabPanel("Site Map",
           tags$br(),
           fluidRow(
             column(3,
                    shared_controls(),
                    tags$hr(),
                    tags$h5("Station summary"),
                    uiOutput("map_summary")
             ),
             column(9,
                    leafletOutput("map", height = "560px")
             )
           )
  ),
  
  # ── Tab 3: Method Comparison ────────────────────────────────
  tabPanel("Method Comparison",
           tags$br(),
           fluidRow(
             column(3,
                    shared_controls(),
                    tags$hr(),
                    tags$h5("BirdNET vs Pattern Matching"),
                    tableOutput("agreement_table"),
                    tags$hr(),
                    tags$div(style = "font-size:11px; color:#666; line-height:1.6;",
                             tags$p(tags$b("How to read this tab:")),
                             tags$p("The top chart compares daily BirdNET and pattern
                  matching detections directly — both run on the same
                  ARU recordings so this is a true method comparison."),
                             tags$p("The bottom chart shows BirdNET daily detections with
                  point count survey dates marked as vertical lines.
                  Point counts are field validation snapshots, not
                  continuous monitoring.")
                    )
             ),
             column(9,
                    tags$h5("BirdNET vs Pattern Matching — daily detections across the season"),
                    plotOutput("bn_vs_pm_plot", height = "260px"),
                    tags$hr(),
                    tags$h5("BirdNET detections with point count survey dates"),
                    plotOutput("bn_with_pc_plot", height = "220px"),
                    tags$hr(),
                    fluidRow(
                      column(3, tags$div(
                        style = "text-align:center; padding:12px;
                     background:#fef9f0; border:1px solid #f0d9a0;
                     border-radius:8px;",
                        tags$h3(textOutput("comp_total_bn")),
                        tags$p("BirdNET detections",
                               style = "font-size:11px; color:#666;")
                      )),
                      column(3, tags$div(
                        style = "text-align:center; padding:12px;
                     background:#f0f7ff; border:1px solid #a0c4f0;
                     border-radius:8px;",
                        tags$h3(textOutput("comp_total_pm")),
                        tags$p("Pattern matching hits",
                               style = "font-size:11px; color:#666;")
                      )),
                      column(3, tags$div(
                        style = "text-align:center; padding:12px;
                     background:#f0faf4; border:1px solid #a0e0b8;
                     border-radius:8px;",
                        tags$h3(textOutput("comp_total_pc")),
                        tags$p("Point count observations",
                               style = "font-size:11px; color:#666;")
                      )),
                      column(3, tags$div(
                        style = "text-align:center; padding:12px;
                     background:#f8f8f8; border:1px solid #ddd;
                     border-radius:8px;",
                        tags$h3(textOutput("comp_agree_pct")),
                        tags$p("Days BN & PM agree",
                               style = "font-size:11px; color:#666;")
                      ))
                    )
             )
           )
  ),
  
  # ── Tab 4: Acoustic Indices ─────────────────────────────────
  tabPanel("Acoustic Indices",
           tags$br(),
           fluidRow(
             column(3,
                    tags$div(
                      style = "background:#f8f8f8; border-radius:8px; padding:14px; margin-bottom:8px;",
                      tags$p("Filters", style = "font-size:10px; letter-spacing:0.1em; text-transform:uppercase; color:#999; margin-bottom:10px;"),
                      checkboxGroupInput("ai_station", "Stations:",
                                         choices  = c("CAT_A1","CAT_A2","CAT_A3","CAT_A4","LHC_01","LHC_02"),
                                         selected = c("CAT_A1","CAT_A2","CAT_A3","CAT_A4","LHC_01","LHC_02")
                      ),
                      checkboxGroupInput("ai_year", "Year:",
                                         choices  = c("2024","2025"),
                                         selected = c("2024","2025")
                      ),
                      selectInput("ai_index", "Index to display:",
                                  choices = c(
                                    "ACI — Acoustic Complexity"    = "aci",
                                    "ADI — Acoustic Diversity"     = "adi",
                                    "BI  — Bioacoustic Index"      = "bi",
                                    "NDSI — Soundscape Index"      = "ndsi"
                                  ),
                                  selected = "aci"
                      ),
                      tags$hr(),
                      tags$p(tags$b("Index guide:"), style = "font-size:11px; color:#444;"),
                      tags$p("ACI — higher = more complex sound", style = "font-size:11px; color:#666; margin-bottom:4px;"),
                      tags$p("ADI — higher = greater frequency diversity", style = "font-size:11px; color:#666; margin-bottom:4px;"),
                      tags$p("BI  — energy in biological range (2–8 kHz)", style = "font-size:11px; color:#666; margin-bottom:4px;"),
                      tags$p("NDSI — positive = more biological than anthropogenic", style = "font-size:11px; color:#666;")
                    )
             ),
             column(9,
                    tags$h5("Seasonal trend — daily mean index value by station"),
                    plotOutput("ai_timeseries", height = "270px"),
                    tags$hr(),
                    tags$h5("Diel pattern — mean index value by hour of day"),
                    plotOutput("ai_diel", height = "230px"),
                    tags$hr(),
                    fluidRow(
                      column(6,
                             tags$h5("Distribution by station"),
                             plotOutput("ai_boxplot", height = "230px")
                      ),
                      column(6,
                             tags$h5("ACI vs BI relationship"),
                             plotOutput("ai_scatter", height = "230px")
                      )
                    )
             )
           )
  ),
  
  # ── Tab 5: Raw Data ─────────────────────────────────────────
  tabPanel("Raw Data",
           tags$br(),
           fluidRow(
             column(3, shared_controls()),
             column(9,
                    tabsetPanel(
                      tabPanel("BirdNET",
                               tags$br(),
                               tags$p(textOutput("raw_bn_count"),
                                      style = "color:#666; font-size:12px;"),
                               dataTableOutput("raw_birdnet")
                      ),
                      tabPanel("Pattern Matching",
                               tags$br(),
                               tags$p(textOutput("raw_pm_count"),
                                      style = "color:#666; font-size:12px;"),
                               dataTableOutput("raw_pm")
                      ),
                      tabPanel("Point Counts",
                               tags$br(),
                               tags$p(textOutput("raw_pc_count"),
                                      style = "color:#666; font-size:12px;"),
                               dataTableOutput("raw_pc")
                      ),
                      tabPanel("Acoustic Indices",
                               tags$br(),
                               tags$p(textOutput("raw_ai_count"),
                                      style = "color:#666; font-size:12px;"),
                               dataTableOutput("raw_ai")
                      )
                    )
             )
           )
  )
)

# =============================================================
# SERVER
# =============================================================
server <- function(input, output, session) {
  
  # Species scientific name lookup for pattern matching
  pm_scientific <- c(
    SALS = "Ammospiza caudacuta",
    BLRA = "Laterallus jamaicensis",
    VIRA = "Rallus limicola",
    CLRA = "Rallus crepitans",
    NESP = "Ammospiza nelsoni",
    SESP = "Ammospiza maritima"
  )
  # birdnet stores site as "Cattus_Island" (underscore)
  bn_site_clause <- reactive({
    bn_site <- gsub(" ", "_", input$site)
    glue("AND site = '{bn_site}'")
  })
  
  # point_counts stores site as "Cattus Island" (space)
  pc_site_clause <- reactive({
    glue("AND site = '{input$site}'")
  })
  
  # pm_detections stores station_name as station codes: CAT_A1, CAT_A2, CAT_A3
  pm_site_clause <- reactive({
    if (input$site == "Cattus Island")
      return("AND station_name IN ('CAT_A1','CAT_A2','CAT_A3','CAT_A4')")
    if (input$site == "Lighthouse Center")
      return("AND station_name IN ('LHC_01','LHC_02')")
    return("")
  })
  
  # ── SHARED REACTIVE DATA ──────────────────────────────────
  
  # BirdNET daily detections
  # date_time is stored as "M/D/YYYY H:MM" text
  bn_daily <- reactive({
    con <- get_con(); on.exit(dbDisconnect(con))
    sp <- birdnet_name[input$species]
    dbGetQuery(con, glue("
      SELECT
        DATE(date_time) as det_date,
        COUNT(*) as detections,
        AVG(confidence) as avg_conf
      FROM birdnet
      WHERE common_name = '{sp}'
        AND confidence >= {input$conf}
        AND EXTRACT(MONTH FROM date_time) BETWEEN 5 AND 9
        {bn_site_clause()}
      GROUP BY DATE(date_time)
      ORDER BY det_date
    "))
  })
  
  # Pattern matching daily detections
  # detection_datetime is a proper timestamp
  pm_daily <- reactive({
    con <- get_con(); on.exit(dbDisconnect(con))
    sp_sci <- pm_scientific[input$species]
    dbGetQuery(con, glue("
      SELECT
        DATE(detection_datetime) as det_date,
        COUNT(*) as detections
      FROM pm_detections
      WHERE scientific_name = '{sp_sci}'
        AND EXTRACT(MONTH FROM detection_datetime) BETWEEN 5 AND 9
        {pm_site_clause()}
      GROUP BY DATE(detection_datetime)
      ORDER BY det_date
    "))
  })
  
  # Point count daily detections
  # survey_date is stored as "DD-MM-YYYY" text
  pc_daily <- reactive({
    con <- get_con(); on.exit(dbDisconnect(con))
    dbGetQuery(con, glue("
      SELECT
        survey_date as det_date,
        SUM(total_count) as detections,
        COUNT(DISTINCT point_id) as points_surveyed
      FROM point_counts
      WHERE alpha_code = '{input$species}'
        AND EXTRACT(MONTH FROM survey_date) BETWEEN 5 AND 9
        {pc_site_clause()}
      GROUP BY survey_date
      ORDER BY survey_date
    "))
  })
  
  # Combined presence/absence by day
  combined <- reactive({
    pc <- pc_daily() %>%
      mutate(det_date   = as.Date(det_date),
             pc_present = detections > 0) %>%
      select(det_date, pc_count = detections, pc_present)
    
    bn <- bn_daily() %>%
      mutate(det_date   = as.Date(det_date),
             bn_present = detections > 0) %>%
      select(det_date, bn_detections = detections, bn_present)
    
    pm <- pm_daily() %>%
      mutate(det_date   = as.Date(det_date),
             pm_present = detections > 0) %>%
      select(det_date, pm_detections = detections, pm_present)
    
    all_dates <- sort(unique(c(pc$det_date,
                               bn$det_date,
                               pm$det_date)))
    
    data.frame(det_date = all_dates) %>%
      left_join(pc, by = "det_date") %>%
      left_join(bn, by = "det_date") %>%
      left_join(pm, by = "det_date") %>%
      mutate(
        across(ends_with("_present"), ~replace_na(.x, FALSE)),
        across(c(pc_count, bn_detections, pm_detections),
               ~replace_na(.x, 0))
      )
  })
  
  # ── TAB 1: TIME SERIES ───────────────────────────────────
  
  output$ts_plot <- renderPlot({
    sp_label   <- names(species_choices)[species_choices == input$species]
    site_label <- names(site_choices)[site_choices == input$site]
    
    p <- ggplot() +
      labs(
        title    = glue("{sp_label} detections — {site_label}"),
        subtitle = glue("BirdNET confidence ≥ {input$conf}"),
        x = NULL, y = "Daily detections",
        color = "Method", fill = "Method"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title       = element_text(face = "bold"),
            panel.grid.minor = element_blank(),
            legend.position  = "bottom")
    
    if ("pc" %in% input$methods && nrow(pc_daily()) > 0)
      p <- p + geom_col(
        data  = pc_daily(),
        aes(x = as.Date(det_date), y = detections, fill = "Point Counts"),
        alpha = 0.25, width = 1
      )
    
    if ("birdnet" %in% input$methods && nrow(bn_daily()) > 0)
      p <- p +
      geom_line(data = bn_daily(),
                aes(x = as.Date(det_date), y = detections, color = "BirdNET"),
                linewidth = 1) +
      geom_point(data = bn_daily(),
                 aes(x = as.Date(det_date), y = detections, color = "BirdNET"),
                 size = 2)
    
    if ("pm" %in% input$methods && nrow(pm_daily()) > 0)
      p <- p +
      geom_line(data = pm_daily(),
                aes(x = as.Date(det_date), y = detections,
                    color = "Pattern Matching"),
                linewidth = 1, linetype = "dashed") +
      geom_point(data = pm_daily(),
                 aes(x = as.Date(det_date), y = detections,
                     color = "Pattern Matching"),
                 size = 2)
    
    p +
      scale_color_manual(values = c(
        "BirdNET"         = "#e74c3c",
        "Pattern Matching" = "#3498db"
      )) +
      scale_fill_manual(values = c("Point Counts" = "#2ecc71"))
  })
  
  output$ts_total_bn <- renderText({
    if (nrow(bn_daily()) > 0)
      format(sum(bn_daily()$detections), big.mark = ",") else "—"
  })
  output$ts_total_pm <- renderText({
    if (nrow(pm_daily()) > 0)
      format(sum(pm_daily()$detections), big.mark = ",") else "—"
  })
  output$ts_total_pc <- renderText({
    if (nrow(pc_daily()) > 0)
      format(sum(pc_daily()$detections, na.rm = TRUE), big.mark = ",") else "—"
  })
  
  # ── TAB 2: SITE MAP ──────────────────────────────────────
  
  # Point count locations — pulled from Supabase pointcount_sites table
  pc_locations <- local({
    con <- get_con()
    on.exit(dbDisconnect(con))
    dbGetQuery(con, "
      SELECT point_id, site, latitude, longitude
      FROM pointcount_sites
      WHERE latitude IS NOT NULL
        AND longitude IS NOT NULL
      ORDER BY site, point_id
    ")
  })
  
  # ARU deployment locations from database
  aru_locations <- reactive({
    con <- get_con(); on.exit(dbDisconnect(con))
    site_filter <- if (input$site == "Cattus Island")
      "AND site_name = 'Cattus_Island'"
    else
      "AND site_name = 'Lighthouse_Center'"
    dbGetQuery(con, glue("
      SELECT DISTINCT ON (station)
        station, site_name as site,
        latitude, longitude
      FROM deployments
      WHERE latitude IS NOT NULL
        AND longitude IS NOT NULL
        {site_filter}
      ORDER BY station, year DESC
    "))
  })
  
  # BirdNET detection counts per ARU station
  station_counts <- reactive({
    con <- get_con(); on.exit(dbDisconnect(con))
    sp <- birdnet_name[input$species]
    dbGetQuery(con, glue("
      SELECT
        station,
        COUNT(*) as detections
      FROM birdnet
      WHERE common_name = '{sp}'
        AND confidence >= {input$conf}
        {bn_site_clause()}
      GROUP BY station
    "))
  })
  
  # Point count totals per point_id for selected species
  pc_counts <- reactive({
    con <- get_con(); on.exit(dbDisconnect(con))
    dbGetQuery(con, glue("
      SELECT
        point_id,
        SUM(total_count) as total_count,
        COUNT(DISTINCT survey_date) as survey_visits
      FROM point_counts
      WHERE alpha_code = '{input$species}'
        {pc_site_clause()}
      GROUP BY point_id
    "))
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      setView(lng = -74.155, lat = 39.88, zoom = 11)
  })
  
  # Fly to correct site when selection changes
  observeEvent(input$site, {
    if (input$site == "Cattus Island") {
      leafletProxy("map") %>%
        setView(lng = -74.124, lat = 39.987, zoom = 14)
    } else {
      leafletProxy("map") %>%
        setView(lng = -74.191, lat = 39.771, zoom = 14)
    }
  })
  
  observe({
    aru <- aru_locations()
    sc  <- station_counts()
    pc  <- pc_counts()
    
    # Join detection counts onto ARU locations
    if (nrow(aru) > 0 && nrow(sc) > 0) {
      aru <- left_join(aru, sc, by = "station") %>%
        mutate(detections = replace_na(detections, 0))
    } else if (nrow(aru) > 0) {
      aru$detections <- 0
    }
    
    # Join point count totals onto point locations
    # filter to current site
    site_filter <- input$site
    pc_locs <- pc_locations %>%
      filter(site == site_filter)
    
    if (nrow(pc) > 0) {
      pc_locs <- left_join(pc_locs, pc, by = "point_id") %>%
        mutate(total_count = replace_na(total_count, 0),
               survey_visits = replace_na(survey_visits, 0))
    } else {
      pc_locs$total_count   <- 0
      pc_locs$survey_visits <- 0
    }
    
    proxy <- leafletProxy("map") %>%
      clearMarkers() %>%
      clearControls()
    
    # ── ARU station markers (green circles, sized by BirdNET detections)
    if (nrow(aru) > 0) {
      max_det    <- max(aru$detections, 1)
      aru$radius <- 8 + (aru$detections / max_det) * 20
      
      aru_popups <- glue::glue(
        "<div style='font-family:monospace;font-size:12px;min-width:160px;'>",
        "<div style='font-size:14px;font-weight:600;color:#2c7a52;margin-bottom:4px;'>{aru$station}</div>",
        "<div style='color:#888;font-size:11px;margin-bottom:6px;'>ARU deployment</div>",
        "<b>{format(aru$detections, big.mark=',')}</b> BirdNET detections<br>",
        "<span style='color:#999;font-size:10px;'>conf ≥ {input$conf}</span>",
        "</div>"
      )
      
      proxy <- proxy %>%
        addCircleMarkers(
          data        = aru,
          lng         = ~longitude,
          lat         = ~latitude,
          radius      = ~radius,
          color       = "#1a1a1a",
          weight      = 1.5,
          fillColor   = "#2ecc71",
          fillOpacity = 0.85,
          popup       = aru_popups,
          label       = ~paste0(station, ": ", format(detections, big.mark=","), " detections"),
          group       = "ARU Stations"
        )
    }
    
    # ── Point count markers (blue squares via circle markers, sized by count)
    if (nrow(pc_locs) > 0) {
      sp_label <- names(species_choices)[species_choices == input$species]
      max_pc   <- max(pc_locs$total_count, 1)
      pc_locs$radius <- 7 + (pc_locs$total_count / max_pc) * 14
      
      pc_popups <- glue::glue(
        "<div style='font-family:monospace;font-size:12px;min-width:160px;'>",
        "<div style='font-size:14px;font-weight:600;color:#1B4F72;margin-bottom:4px;'>{pc_locs$point_id}</div>",
        "<div style='color:#888;font-size:11px;margin-bottom:6px;'>Point count station</div>",
        "<b>{pc_locs$total_count}</b> {sp_label} counted<br>",
        "<span style='color:#999;font-size:10px;'>{pc_locs$survey_visits} survey visit(s)</span>",
        "</div>"
      )
      
      proxy <- proxy %>%
        addCircleMarkers(
          data        = pc_locs,
          lng         = ~longitude,
          lat         = ~latitude,
          radius      = ~radius,
          color       = "#1a1a1a",
          weight      = 1.5,
          fillColor   = "#3498db",
          fillOpacity = 0.85,
          popup       = pc_popups,
          label       = ~paste0(point_id, ": ", total_count, " counted"),
          group       = "Point Count Stations"
        )
    }
    
    # ── Layer control + legend
    proxy %>%
      addLayersControl(
        overlayGroups = c("ARU Stations", "Point Count Stations"),
        options       = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(
        position = "bottomright",
        colors   = c("#2ecc71", "#3498db"),
        labels   = c("ARU station (BirdNET detections)", "Point count station (observed counts)"),
        opacity  = 0.85,
        title    = "Marker type"
      )
  })
  
  output$map_summary <- renderUI({
    aru <- aru_locations()
    sc  <- station_counts()
    pc  <- pc_counts()
    sp_label <- names(species_choices)[species_choices == input$species]
    tags$div(
      tags$p(tags$b(glue("{input$site}")),
             style = "font-size:13px; margin-bottom:6px;"),
      tags$p(glue("{nrow(aru)} ARU stations"),
             style = "font-size:12px; color:#2c7a52;"),
      tags$p(glue("{if(nrow(sc)>0) format(sum(sc$detections),big.mark=',') else 0} BirdNET detections"),
             style = "font-size:12px; color:#666;"),
      tags$hr(style="margin:6px 0;"),
      tags$p(glue("{nrow(pc_locations[pc_locations$site==input$site,])} point count stations"),
             style = "font-size:12px; color:#3498db;"),
      tags$p(glue("{if(nrow(pc)>0) sum(pc$total_count, na.rm=TRUE) else 0} {sp_label} counted"),
             style = "font-size:12px; color:#666;"),
      tags$hr(style="margin:6px 0;"),
      tags$p("Green = ARU  ·  Blue = point count",
             style = "font-size:11px; color:#999;"),
      tags$p("Marker size = detection / count intensity",
             style = "font-size:11px; color:#999;"),
      tags$p("Click any marker for details.",
             style = "font-size:11px; color:#999;")
    )
  })
  
  # ── TAB 3: METHOD COMPARISON ─────────────────────────────
  
  # BirdNET vs Pattern Matching direct comparison plot
  output$bn_vs_pm_plot <- renderPlot({
    bn <- bn_daily() %>% mutate(det_date = as.Date(det_date), method = "BirdNET")
    pm <- pm_daily() %>% mutate(det_date = as.Date(det_date), method = "Pattern Matching")
    
    req(nrow(bn) > 0 || nrow(pm) > 0)
    
    plot_df <- bind_rows(
      bn %>% select(det_date, detections, method),
      pm %>% select(det_date, detections, method)
    )
    
    ggplot(plot_df, aes(x = det_date, y = detections, color = method)) +
      geom_line(linewidth = 0.8, alpha = 0.9) +
      geom_point(size = 1.5, alpha = 0.7) +
      scale_color_manual(
        values = c("BirdNET" = "#e74c3c", "Pattern Matching" = "#3498db"),
        name   = NULL
      ) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      labs(
        x        = NULL,
        y        = "Daily detections",
        subtitle = glue("{names(species_choices)[species_choices == input$species]} · {names(site_choices)[site_choices == input$site]} · BirdNET conf ≥ {input$conf}")
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor  = element_blank(),
        legend.position   = "top",
        legend.text       = element_text(size = 11),
        axis.text.x       = element_text(angle = 30, hjust = 1),
        plot.subtitle     = element_text(color = "#666", size = 11)
      )
  })
  
  # BirdNET detections with point count survey dates as reference lines
  output$bn_with_pc_plot <- renderPlot({
    bn <- bn_daily() %>% mutate(det_date = as.Date(det_date))
    pc <- pc_daily() %>% mutate(det_date = as.Date(det_date))
    
    req(nrow(bn) > 0)
    
    p <- ggplot() +
      geom_col(data = bn,
               aes(x = det_date, y = detections),
               fill = "#e74c3c", alpha = 0.7, width = 1) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      labs(
        x        = NULL,
        y        = "BirdNET detections",
        subtitle = "Vertical lines show point count survey dates"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.x      = element_text(angle = 30, hjust = 1),
        plot.subtitle    = element_text(color = "#666", size = 11)
      )
    
    # Add point count survey date lines if data exists
    if (nrow(pc) > 0) {
      p <- p +
        geom_vline(data = pc,
                   aes(xintercept = as.numeric(det_date)),
                   color = "#2ecc71", linewidth = 1.2, alpha = 0.8) +
        annotate("text",
                 x     = min(pc$det_date),
                 y     = max(bn$detections) * 0.95,
                 label = "Point count dates",
                 color = "#2ecc71", size = 3.5, hjust = 0)
    }
    
    p
  })
  
  # Summary stats
  output$comp_total_bn <- renderText({
    if (nrow(bn_daily()) > 0)
      format(sum(bn_daily()$detections), big.mark = ",") else "—"
  })
  output$comp_total_pm <- renderText({
    if (nrow(pm_daily()) > 0)
      format(sum(pm_daily()$detections), big.mark = ",") else "—"
  })
  output$comp_total_pc <- renderText({
    if (nrow(pc_daily()) > 0)
      format(sum(pc_daily()$detections, na.rm = TRUE), big.mark = ",") else "—"
  })
  
  # Agreement: days where both BirdNET and PM detected species
  output$comp_agree_pct <- renderText({
    bn <- bn_daily() %>% mutate(det_date = as.Date(det_date), bn = TRUE) %>%
      select(det_date, bn)
    pm <- pm_daily() %>% mutate(det_date = as.Date(det_date), pm = TRUE) %>%
      select(det_date, pm)
    
    if (nrow(bn) == 0 || nrow(pm) == 0) return("—")
    
    both  <- inner_join(bn, pm, by = "det_date")
    total <- n_distinct(c(bn$det_date, pm$det_date))
    glue("{round(nrow(both) / total * 100)}%")
  })
  
  # Agreement table: BirdNET vs PM only
  output$agreement_table <- renderTable({
    bn <- bn_daily() %>% mutate(det_date = as.Date(det_date))
    pm <- pm_daily() %>% mutate(det_date = as.Date(det_date))
    
    bn_days   <- nrow(bn)
    pm_days   <- nrow(pm)
    both_days <- nrow(inner_join(bn, pm, by = "det_date"))
    bn_only   <- nrow(anti_join(bn, pm, by = "det_date"))
    pm_only   <- nrow(anti_join(pm, bn, by = "det_date"))
    
    data.frame(
      Metric = c("BirdNET detection days",
                 "PM detection days",
                 "Both detected same day",
                 "BirdNET only",
                 "PM only"),
      Count  = c(bn_days, pm_days, both_days, bn_only, pm_only)
    )
  }, striped = FALSE, bordered = FALSE, hover = FALSE, spacing = "xs")
  
  # ── TAB 4: RAW DATA ──────────────────────────────────────
  
  bn_raw <- reactive({
    con <- get_con(); on.exit(dbDisconnect(con))
    sp <- birdnet_name[input$species]
    dbGetQuery(con, glue("
      SELECT
        date_time, station, site,
        common_name, scientific_name, confidence
      FROM birdnet
      WHERE common_name = '{sp}'
        AND confidence >= {input$conf}
        {bn_site_clause()}
      ORDER BY date_time DESC
      LIMIT 500
    "))
  })
  
  pm_raw <- reactive({
    con <- get_con(); on.exit(dbDisconnect(con))
    dbGetQuery(con, glue("
      SELECT
        detection_datetime,
        station_name as station,
        scientific_name, score, validated
      FROM pm_detections
      WHERE 1=1
        {pm_site_clause()}
      ORDER BY detection_datetime DESC
      LIMIT 500
    "))
  })
  
  pc_raw <- reactive({
    con <- get_con(); on.exit(dbDisconnect(con))
    dbGetQuery(con, glue("
      SELECT
        survey_date, site, point_id, observer,
        alpha_code, total_count, dist_band, tide
      FROM point_counts
      WHERE alpha_code = '{input$species}'
        {pc_site_clause()}
      ORDER BY survey_date DESC
    "))
  })
  
  output$raw_bn_count <- renderText(
    glue("{nrow(bn_raw())} records (capped at 500)"))
  output$raw_pm_count <- renderText(
    glue("{nrow(pm_raw())} records (capped at 500)"))
  output$raw_pc_count <- renderText(
    glue("{nrow(pc_raw())} records"))
  
  output$raw_birdnet <- renderDataTable(bn_raw(),
                                        options = list(pageLength = 15, scrollX = TRUE))
  output$raw_pm      <- renderDataTable(pm_raw(),
                                        options = list(pageLength = 15, scrollX = TRUE))
  output$raw_pc      <- renderDataTable(pc_raw(),
                                        options = list(pageLength = 15, scrollX = TRUE))
  
  ai_raw <- reactive({
    con <- get_con(); on.exit(dbDisconnect(con))
    dbGetQuery(con, "
      SELECT
        station, year,
        recorded_at, hour,
        ROUND(aci::numeric, 3)  as aci,
        ROUND(adi::numeric, 4)  as adi,
        ROUND(bi::numeric, 3)   as bi,
        ROUND(ndsi::numeric, 5) as ndsi,
        duration,
        filename
      FROM acoustic_indices
      ORDER BY recorded_at DESC
      LIMIT 1000
    ")
  })
  
  output$raw_ai_count <- renderText(
    glue("{nrow(ai_raw())} records (capped at 1000)"))
  output$raw_ai <- renderDataTable(ai_raw(),
                                   options = list(pageLength = 15, scrollX = TRUE))
  
  # ── TAB 4: ACOUSTIC INDICES ──────────────────────────────
  
  # Pull filtered acoustic index data from DB
  ai_data <- reactive({
    req(length(input$ai_station) > 0, length(input$ai_year) > 0)
    con <- get_con(); on.exit(dbDisconnect(con))
    stations <- paste0("'", input$ai_station, "'", collapse = ",")
    years    <- paste(input$ai_year, collapse = ",")
    dbGetQuery(con, glue("
      SELECT
        station,
        year,
        DATE(recorded_at) as rec_date,
        hour,
        aci, adi, bi, ndsi
      FROM acoustic_indices
      WHERE station IN ({stations})
        AND year IN ({years})
        AND recorded_at IS NOT NULL
      ORDER BY recorded_at
    "))
  })
  
  # Helper: get the selected index column and its label
  index_label <- reactive({
    c(
      aci  = "ACI (Acoustic Complexity Index)",
      adi  = "ADI (Acoustic Diversity Index)",
      bi   = "BI (Bioacoustic Index)",
      ndsi = "NDSI (Normalized Difference Soundscape Index)"
    )[input$ai_index]
  })
  
  station_colors <- c(
    CAT_A1 = "#2ecc71", CAT_A2 = "#27ae60",
    CAT_A3 = "#1abc9c", CAT_A4 = "#16a085",
    LHC_01 = "#3498db", LHC_02 = "#2980b9"
  )
  
  # ── Plot 1: Seasonal time series ─────────────────────────
  output$ai_timeseries <- renderPlot({
    df <- ai_data()
    req(nrow(df) > 0)
    idx <- input$ai_index
    
    daily <- df %>%
      group_by(station, year, rec_date) %>%
      summarise(mean_val = mean(.data[[idx]], na.rm = TRUE),
                .groups = "drop") %>%
      mutate(rec_date = as.Date(rec_date),
             station_year = paste0(station, " (", year, ")"))
    
    ggplot(daily, aes(x = rec_date, y = mean_val,
                      color = station, linetype = factor(year))) +
      geom_line(linewidth = 0.7, alpha = 0.85) +
      scale_color_manual(values = station_colors, name = "Station") +
      scale_linetype_manual(values = c("2024" = "solid", "2025" = "dashed"),
                            name = "Year") +
      scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
      labs(x = NULL, y = index_label()) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor  = element_blank(),
            axis.text.x       = element_text(angle = 30, hjust = 1),
            legend.position   = "right",
            legend.text       = element_text(size = 10))
  })
  
  # ── Plot 2: Diel pattern ─────────────────────────────────
  output$ai_diel <- renderPlot({
    df <- ai_data()
    req(nrow(df) > 0)
    idx <- input$ai_index
    
    hourly <- df %>%
      group_by(station, hour) %>%
      summarise(mean_val = mean(.data[[idx]], na.rm = TRUE),
                se_val   = sd(.data[[idx]], na.rm = TRUE) /
                  sqrt(n()),
                .groups = "drop")
    
    ggplot(hourly, aes(x = hour, y = mean_val,
                       color = station, fill = station)) +
      geom_ribbon(aes(ymin = mean_val - se_val,
                      ymax = mean_val + se_val),
                  alpha = 0.12, color = NA) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 1.8) +
      scale_color_manual(values = station_colors, name = "Station") +
      scale_fill_manual(values  = station_colors, name = "Station") +
      scale_x_continuous(breaks = seq(0, 23, 3),
                         labels = c("Midnight","3am","6am","9am",
                                    "Noon","3pm","6pm","9pm")) +
      labs(x = NULL, y = index_label(),
           subtitle = "Shaded band = ±1 SE") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank(),
            legend.position  = "right",
            legend.text      = element_text(size = 10),
            plot.subtitle    = element_text(color = "#666", size = 10))
  })
  
  # ── Plot 3: Boxplot by station ────────────────────────────
  output$ai_boxplot <- renderPlot({
    df <- ai_data()
    req(nrow(df) > 0)
    idx <- input$ai_index
    
    ggplot(df, aes(x = station, y = .data[[idx]],
                   fill = station)) +
      geom_boxplot(outlier.size = 0.6, outlier.alpha = 0.4,
                   linewidth = 0.5, alpha = 0.85) +
      scale_fill_manual(values = station_colors) +
      facet_wrap(~year, nrow = 1) +
      labs(x = NULL, y = index_label()) +
      theme_minimal(base_size = 12) +
      theme(legend.position  = "none",
            panel.grid.minor = element_blank(),
            axis.text.x      = element_text(angle = 35, hjust = 1))
  })
  
  # ── Plot 4: ACI vs BI scatter ─────────────────────────────
  output$ai_scatter <- renderPlot({
    df <- ai_data()
    req(nrow(df) > 0)
    
    # Downsample to keep render fast — max 3000 points
    if (nrow(df) > 3000) df <- df %>% slice_sample(n = 3000)
    
    ggplot(df, aes(x = aci, y = bi, color = station)) +
      geom_point(size = 1.0, alpha = 0.35) +
      geom_smooth(method = "lm", se = FALSE,
                  linewidth = 0.8, alpha = 0.9) +
      scale_color_manual(values = station_colors, name = "Station") +
      labs(x = "ACI (Acoustic Complexity)",
           y = "BI (Bioacoustic Index)",
           subtitle = "Each point = one recording") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank(),
            legend.position  = "right",
            legend.text      = element_text(size = 10),
            plot.subtitle    = element_text(color = "#666", size = 10))
  })
  
}

shinyApp(ui, server)