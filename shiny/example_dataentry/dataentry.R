library(shiny)
library(DBI)
library(RPostgres)
library(readr)
library(dplyr)

# ── Connection ───────────────────────────────────────────
con_info <- list(
  host = "aws-0-us-west-2.pooler.supabase.com", dbname = "postgres",
  user = "postgres.nfyvxsccktlojkvvnvdb", password = "spartinapatens", port =  5432, 
  sslmode  = "require"
)

# ── Valid lookup values ──────────────────────────────────
valid_observers <- c("KNF", "KF", "ADD_YOUR_TEAM_INITIALS")
valid_stations  <- c("CAT_A1","CAT_A2","CAT_A3",
                     "CAT_A4","LHC_01","LHC_02")
valid_species   <- c("SALS","BLRA","VIRA","CLRA","KIRA",
                     "LEBI","SORA","AMBI","COMO","NESP",
                     "GRCA","MAWR","RWBL","SOSP","BARS")
field_season_months <- 5:9   # May through September

# ── UI ───────────────────────────────────────────────────
ui <- fluidPage(
  tags$h2("Point Count Data Upload"),
  
  # Download template button
  downloadButton("download_template", "Download CSV Template"),
  tags$hr(),
  
  # Upload section
  fileInput("upload", "Upload completed CSV:",
            accept = ".csv"),
  
  # Preview and validation results
  uiOutput("validation_summary"),
  tableOutput("preview"),
  tags$hr(),
  
  # Submit only appears if data passes validation
  uiOutput("submit_button"),
  textOutput("submit_status")
)

# ── Server ───────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Template download ──────────────────────────────────
  output$download_template <- downloadHandler(
    filename = "point_count_template.csv",
    content = function(file) {
      template <- data.frame(
        region_num    = integer(),
        state         = character(),
        site          = character(),
        point_id      = character(),
        survey_method = character(),
        visit_num     = integer(),
        survey_window = character(),
        survey_date   = character(),   # YYYY-MM-DD
        survey_time   = character(),   # HH:MM
        observer      = character(),
        tide          = character(),
        temp_f        = numeric(),
        sky           = integer(),
        wind_speed    = integer(),
        noise         = integer(),
        alpha_code    = character(),
        distance      = character(),
        call_type     = character(),
        total_count   = integer(),
        min_1         = integer(),
        min_2         = integer(),
        min_3         = integer(),
        min_4         = integer(),
        min_5         = integer(),
        comments      = character()
      )
      write_csv(template, file)
    }
  )
  
  # ── Read uploaded file ─────────────────────────────────
  uploaded_data <- reactive({
    req(input$upload)
    tryCatch(
      read_csv(input$upload$datapath, show_col_types = FALSE),
      error = function(e) NULL
    )
  })
  
  # ── QAQC validation ────────────────────────────────────
  validation_result <- reactive({
    req(uploaded_data())
    df <- uploaded_data()
    
    # Required columns
    required_cols <- c("survey_date","observer","point_id",
                       "alpha_code","total_count","distance")
    missing_cols <- setdiff(required_cols, names(df))
    
    if (length(missing_cols) > 0) {
      return(list(
        valid = FALSE,
        fatal = paste("Missing required columns:",
                      paste(missing_cols, collapse = ", ")),
        row_errors = NULL,
        clean_data = NULL
      ))
    }
    
    # Row-by-row checks
    row_errors <- list()
    
    for (i in seq_len(nrow(df))) {
      errors <- c()
      row    <- df[i, ]
      
      # Observer must be known
      if (!toupper(trimws(row$observer)) %in% valid_observers) {
        errors <- c(errors, glue::glue(
          "Unknown observer '{row$observer}' — expected one of: {paste(valid_observers, collapse=', ')}"
        ))
      }
      
      # Species code must be 4 letters
      if (!grepl("^[A-Za-z]{4}$", trimws(row$alpha_code))) {
        errors <- c(errors,
                    glue::glue("Alpha code '{row$alpha_code}' must be exactly 4 letters"))
      }
      
      # Date format and field season check
      date_parsed <- tryCatch(
        as.Date(row$survey_date),
        error = function(e) NA
      )
      if (is.na(date_parsed)) {
        errors <- c(errors,
                    glue::glue("Date '{row$survey_date}' is not valid — use YYYY-MM-DD format"))
      } else {
        if (date_parsed > Sys.Date()) {
          errors <- c(errors, "Survey date is in the future")
        }
        if (!as.integer(format(date_parsed, "%m")) %in% field_season_months) {
          errors <- c(errors,
                      glue::glue("Date {date_parsed} is outside the May-September field season"))
        }
      }
      
      # Count must be positive
      if (!is.na(row$total_count) && row$total_count <= 0) {
        errors <- c(errors, "Total count must be greater than 0")
      }
      
      # Distance band must be valid
      if (!row$distance %in% c("0-50m","51-100m",">100m")) {
        errors <- c(errors,
                    glue::glue("Distance '{row$distance}' not valid — use 0-50m, 51-100m, or >100m"))
      }
      
      if (length(errors) > 0) {
        row_errors[[i]] <- data.frame(
          row     = i + 1,   # +1 because row 1 is the header in Excel
          species = row$alpha_code,
          date    = as.character(row$survey_date),
          errors  = paste(errors, collapse = " | ")
        )
      }
    }
    
    error_df <- if (length(row_errors) > 0) {
      bind_rows(row_errors)
    } else {
      NULL
    }
    
    list(
      valid      = is.null(error_df),
      fatal      = NULL,
      row_errors = error_df,
      clean_data = df
    )
  })
  
  # ── Validation summary UI ──────────────────────────────
  output$validation_summary <- renderUI({
    req(validation_result())
    v <- validation_result()
    
    if (!is.null(v$fatal)) {
      tags$div(style = "color:red; font-weight:bold; margin:12px 0;",
               tags$p("⛔ Cannot read file:"),
               tags$p(v$fatal)
      )
    } else if (!v$valid) {
      tags$div(style = "color:#c0392b; margin:12px 0;",
               tags$p(tags$b(glue::glue(
                 "⚠ {nrow(v$row_errors)} row(s) have errors — fix and re-upload:"
               ))),
               renderTable(v$row_errors)
      )
    } else {
      tags$div(style = "color:#27ae60; font-weight:bold; margin:12px 0;",
               tags$p(glue::glue(
                 "✓ All {nrow(v$clean_data)} rows passed validation — ready to submit"
               ))
      )
    }
  })
  
  # ── Data preview ───────────────────────────────────────
  output$preview <- renderTable({
    req(uploaded_data())
    head(uploaded_data(), 10)
  })
  
  # ── Submit button only appears when data is clean ──────
  output$submit_button <- renderUI({
    req(validation_result())
    if (isTRUE(validation_result()$valid)) {
      actionButton("submit", "Submit to Database",
                   style = "background:#27ae60; color:white;
                            font-size:16px; padding:10px 24px;")
    }
  })
  
  # ── Write to database ──────────────────────────────────
  observeEvent(input$submit, {
    df <- validation_result()$clean_data
    
    con <- do.call(dbConnect,
                   c(list(RPostgres::Postgres()), con_info))
    
    tryCatch({
      dbWriteTable(con, "point_counts", df, append = TRUE)
      output$submit_status <- renderText(
        glue::glue("Successfully submitted {nrow(df)} rows to the database.")
      )
    }, error = function(e) {
      output$submit_status <- renderText(
        paste("Database error:", e$message)
      )
    })
    
    dbDisconnect(con)
  })
}

shinyApp(ui, server)
