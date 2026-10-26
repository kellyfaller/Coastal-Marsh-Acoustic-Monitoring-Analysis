library(DBI)
library(RPostgres)
library(dplyr)
library(glue)

# Reads Kaleidoscope acoustic index outputs and appends any new rows to the acoustic_indices table in the database.
# Uses the same reader as "Kaleidoscope Data Prep Script.Rmd" (R/kaleidoscope_functions.R).
source(here::here("R", "kaleidoscope_functions.R"))

# Database credentials are read from environment variables so they never live in the repository.
# Put them in your user .Renviron file (open it with usethis::edit_r_environ()), then restart R:
#   KALEIDOSCOPE_DB_HOST=aws-0-us-west-2.pooler.supabase.com
#   KALEIDOSCOPE_DB_USER=postgres.<project id>
#   KALEIDOSCOPE_DB_PASSWORD=<password>
db_env <- c(host = "KALEIDOSCOPE_DB_HOST", user = "KALEIDOSCOPE_DB_USER", password = "KALEIDOSCOPE_DB_PASSWORD")
missing_env <- db_env[Sys.getenv(db_env) == ""]
if (length(missing_env)) stop("Set these environment variables first (see top of this script): ", paste(missing_env, collapse = ", "))

# Root of your Kaleidoscope outputs folder
# Structure: kaleidoscope_root / year / station / acousticindex.csv
kaleidoscope_root <- here::here("data", "processed", "kaleidoscope_outputs")

combined <- read_kaleidoscope_indices(kaleidoscope_root, tz = "America/New_York")
checks <- check_kaleidoscope_indices(combined)
print(checks$by_station)
print(checks$problems)
if (any(checks$problems[["Unparsed date-times"]] > 0)) stop("Some date-times couldn't be parsed - fix before uploading.")

con <- dbConnect(
  RPostgres::Postgres(),
  host     = Sys.getenv("KALEIDOSCOPE_DB_HOST"),
  dbname   = Sys.getenv("KALEIDOSCOPE_DB_NAME", "postgres"),
  user     = Sys.getenv("KALEIDOSCOPE_DB_USER"),
  password = Sys.getenv("KALEIDOSCOPE_DB_PASSWORD"),
  port     = as.integer(Sys.getenv("KALEIDOSCOPE_DB_PORT", "5432")),
  sslmode  = "require"
)

# Append only rows that aren't already in the table. The unique key is filename + station, so re-running the
# script after adding a new station or year doesn't double-import the old ones.
if (dbExistsTable(con, "acoustic_indices")) {
  existing <- dbGetQuery(con, "SELECT DISTINCT filename, station FROM acoustic_indices")
  new_rows <- anti_join(combined, existing, by = c("filename", "station"))
} else {
  new_rows <- combined
}
cat(glue("{nrow(combined)} rows read, {nrow(combined) - nrow(new_rows)} already in the database, {nrow(new_rows)} to import"), "\n")

if (nrow(new_rows)) dbWriteTable(con, "acoustic_indices", new_rows, append = TRUE)

cat(glue("Imported {nrow(new_rows)} acoustic index records"), "\n")

# Verify
summary <- dbGetQuery(con, "
  SELECT station, year,
         COUNT(*) as recordings,
         ROUND(AVG(aci)::numeric, 2) as avg_aci,
         ROUND(AVG(ndsi)::numeric, 4) as avg_ndsi
  FROM acoustic_indices
  GROUP BY station, year
  ORDER BY year, station
")
print(summary)

dbDisconnect(con)
