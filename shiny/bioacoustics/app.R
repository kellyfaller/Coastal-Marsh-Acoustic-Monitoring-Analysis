library(shiny)
library(DBI)
library(RPostgres)
library(dplyr)
library(glue)

con_info <- list(
  host = "aws-0-us-west-2.pooler.supabase.com", dbname = "postgres",
  user = "postgres.nfyvxsccktlojkvvnvdb", password = "spartinapatens", port =  5432, 
  sslmode  = "require"
)

ui <- fluidPage(
  tags$h1("BirdNET Species Detections"),
  selectInput("species", "Choose a species:",
              choices = NULL),   # populated from DB
  tableOutput("detections")
)

server <- function(input, output, session) {
  
  # Populate dropdown from your actual species in the database
  observe({
    con <- do.call(dbConnect, c(RPostgres::Postgres(), con_info))
    species_list <- dbGetQuery(con,
                               "SELECT DISTINCT common_name FROM birdnet
       ORDER BY common_name")
    dbDisconnect(con)
    updateSelectInput(session, "species",
                      choices = species_list$common_name)
  })
  
  output$detections <- renderTable({
    req(input$species)
    con <- do.call(dbConnect, c(RPostgres::Postgres(), con_info))
    result <- dbGetQuery(con, glue::glue(
      "SELECT station, birdnet.date_time, confidence
       FROM birdnet
       WHERE common_name = '{input$species}'
       ORDER BY birdnet.date_time DESC
       LIMIT 100"
    ))
    dbDisconnect(con)
    result
  })
}

shinyApp(ui, server)
